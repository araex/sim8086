const std = @import("std");
const builtin = @import("builtin");

const dvui = @import("dvui");

const x86 = @import("x86.zig");

comptime {
    std.debug.assert(@hasDecl(dvui.backend, "SDLBackend"));
}

const num_registers = 8;
const State = struct {
    // The simulator
    sim: *x86.Simulator,

    // Disassembly of the instructions in the simulator
    instr_asm: []const []const u8,

    // Pre-allocated strings for AX, BX, CX, DX, SP, BP, SI, DI. 4 byte each + 0 termination
    register_strings: [num_registers][:0]u8,

    ui_scale: f32,
    allocator: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator, simulator: *x86.Simulator) !State {
        // Allocate a slice to hold all instruction strings
        var instr_asm = try alloc.alloc([]u8, simulator.instructions.len);

        // Temporary buffer for formatting each instruction
        var lineBuffer: [256]u8 = undefined;

        // Convert each instruction to its assembly representation
        for (simulator.instructions, 0..) |instruction, i| {
            // Format the instruction
            const instruction_string = try std.fmt.bufPrint(
                &lineBuffer,
                "{}",
                .{x86.fmt(instruction)},
            );

            // Allocate memory for the string and copy it
            instr_asm[i] = try alloc.dupe(u8, instruction_string);

            // Convert to lowercase if needed
            for (instr_asm[i]) |*c| {
                c.* = std.ascii.toLower(c.*);
            }
        }

        var register_strings: [num_registers][:0]u8 = undefined;
        for (0..8) |i| {
            register_strings[i] = try std.fmt.allocPrintZ(alloc, "xxxx", .{});
        }

        return State{
            .sim = simulator,
            .instr_asm = instr_asm,
            .register_strings = register_strings,
            .ui_scale = 1.2,
            .allocator = alloc,
        };
    }

    pub fn deinit(self: *State) void {
        // Free each individual instruction string
        for (self.instr_asm) |asm_str| {
            self.allocator.free(asm_str);
        }
        // Free the slice itself
        self.allocator.free(self.instr_asm);

        // Free register strings
        for (self.register_strings) |reg_str| {
            self.allocator.free(reg_str);
        }
    }

    // Helper to update register string values
    pub fn updateRegisterStrings(self: *State) void {
        const regs = self.sim.registers;

        // Format each register as a 4-digit hex string
        _ = std.fmt.bufPrintZ(self.register_strings[0], "{X:0>4}", .{regs.getWord(.AX)}) catch {};
        _ = std.fmt.bufPrintZ(self.register_strings[1], "{X:0>4}", .{regs.getWord(.BX)}) catch {};
        _ = std.fmt.bufPrintZ(self.register_strings[2], "{X:0>4}", .{regs.getWord(.CX)}) catch {};
        _ = std.fmt.bufPrintZ(self.register_strings[3], "{X:0>4}", .{regs.getWord(.DX)}) catch {};
        _ = std.fmt.bufPrintZ(self.register_strings[4], "{X:0>4}", .{regs.getWord(.SP)}) catch {};
        _ = std.fmt.bufPrintZ(self.register_strings[5], "{X:0>4}", .{regs.getWord(.BP)}) catch {};
        _ = std.fmt.bufPrintZ(self.register_strings[6], "{X:0>4}", .{regs.getWord(.SI)}) catch {};
        _ = std.fmt.bufPrintZ(self.register_strings[7], "{X:0>4}", .{regs.getWord(.DI)}) catch {};
    }
};

pub fn run(alloc: std.mem.Allocator, simulator: *x86.Simulator) !void {
    if (@import("builtin").os.tag == .windows) {
        // on windows graphical apps have no console, so output goes to nowhere
        // attach it manually. related: https://github.com/ziglang/zig/issues/4196
        _ = winapi.AttachConsole(0xFFFFFFFF);
    }
    std.log.info("SDL version: {}", .{dvui.backend.getSDLVersion()});

    var backend = try dvui.backend.initWindow(.{
        .allocator = alloc,
        .size = .{ .w = 1200.0, .h = 800.0 },
        .min_size = .{ .w = 250.0, .h = 350.0 },
        .vsync = true,
        .title = "sim8086",
        .icon = @embedFile("assets/zig-favicon.png"),
    });
    defer backend.deinit();

    var win = try dvui.Window.init(@src(), alloc, backend.backend(), .{});
    defer win.deinit();

    // load theme
    try @import("theme.zig").install(&win);

    var state = try State.init(alloc, simulator);
    defer state.deinit(); // Add this line to free memory when done

    main_loop: while (true) {
        // beginWait coordinates with waitTime below to run frames only when needed
        const nstime = win.beginWait(backend.hasEvent());

        // marks the beginning of a frame for dvui, can call dvui functions after this
        try win.begin(nstime);

        // send all SDL events to dvui for processing
        const quit = try backend.addAllEvents(&win);
        if (quit) break :main_loop;

        // if dvui widgets might not cover the whole window, then need to clear
        // the previous frame's render
        _ = dvui.backend.c.SDL_SetRenderDrawColor(backend.renderer, 0, 0, 0, 255);
        _ = dvui.backend.c.SDL_RenderClear(backend.renderer);

        try draw_ui(&state);

        // marks end of dvui frame, don't call dvui functions after this
        // - sends all dvui stuff to backend for rendering, must be called before renderPresent()
        const end_micros = try win.end(.{});

        // cursor management
        backend.setCursor(win.cursorRequested());
        backend.textInputRect(win.textInputRequested());

        // render frame to OS
        backend.renderPresent();

        // waitTime and beginWait combine to achieve variable framerates
        const wait_event_micros = win.waitTime(end_micros, null);
        backend.waitEventTimeout(wait_event_micros);
    }
}

fn draw_asm(state: *State) !void {
    var tl_asm = try dvui.textLayout(
        @src(),
        .{},
        .{ .expand = .horizontal },
    );
    defer tl_asm.deinit();
    const prefix_normal = "  ";
    const prefix_current = "> ";
    for (state.instr_asm, 0..) |line, i| {
        if (i == state.sim.cur_instruction) {
            try tl_asm.addText(prefix_current, .{});
            try tl_asm.addText(line, .{
                .font_style = .heading,
                .color_text = .{ .color = dvui.themeGet().color_accent },
            });
        } else {
            try tl_asm.addText(prefix_normal, .{});
            try tl_asm.addText(line, .{});
        }

        if (i < state.instr_asm.len - 1) {
            try tl_asm.addText("\n", .{});
        }
    }
}

fn draw_registers(state: *State) !void {
    // Update the register strings before displaying
    state.updateRegisterStrings();

    var hbox = try dvui.box(@src(), .horizontal, .{});
    defer hbox.deinit();

    const reg_names = [num_registers][:0]const u8{ "AX ", "BX ", "CX ", "DX ", "SP ", "BP ", "SI ", "DI " };

    const color_text_dim = dvui.Options.ColorOrName{
        .color = dvui.Color.average(dvui.themeGet().color_text, dvui.themeGet().color_fill_control),
    };

    // First column (AX, BX, CX, DX)
    {
        var tl = try dvui.textLayout(@src(), .{}, .{ .expand = .horizontal });
        defer tl.deinit();

        for (0..4) |i| {
            try tl.addText(reg_names[i], .{ .color_text = color_text_dim });
            try tl.addText(state.register_strings[i], .{ .font_style = .heading });
            if (i < 3) {
                try tl.addText("\n", .{});
            }
        }
    }

    // Second column (SP, BP, SI, DI)
    {
        var tl = try dvui.textLayout(@src(), .{}, .{ .expand = .horizontal });
        defer tl.deinit();

        for (4..8) |i| {
            try tl.addText(reg_names[i], .{ .color_text = color_text_dim });
            try tl.addText(state.register_strings[i], .{ .font_style = .heading });
            if (i < 7) {
                try tl.addText("\n", .{});
            }
        }
    }
}

fn buttonIconAndLabel(
    src: std.builtin.SourceLocation,
    label: []const u8,
    tvg_bytes: []const u8,
    opts: dvui.Options,
) !bool {
    var bw = dvui.ButtonWidget.init(src, .{}, opts);
    defer bw.deinit();
    try bw.install();
    bw.processEvents();
    try bw.drawBackground();
    try bw.drawFocus();

    const opts_inner = bw.data().options.strip().override(.{ .gravity_y = 0.5 });
    var bbox = try dvui.box(@src(), .horizontal, opts_inner);
    defer bbox.deinit();

    try dvui.icon(@src(), label, tvg_bytes, opts_inner);
    _ = try dvui.spacer(@src(), .{ .w = 4 }, .{});
    try dvui.labelNoFmt(@src(), label, opts_inner);

    return bw.clicked();
}

fn draw_controls(state: *State) !void {
    var hbox = try dvui.box(@src(), .horizontal, .{});
    defer hbox.deinit();

    const opts = dvui.Options{ .gravity_y = 0.5 };
    if (!state.sim.isDone()) {
        if (try buttonIconAndLabel(@src(), "step", dvui.entypo.controller_play, opts)) {
            try state.sim.step();
        }
        if (try dvui.buttonIcon(@src(), "play", dvui.entypo.controller_next, .{}, opts)) {
            while (!state.sim.isDone()) {
                try state.sim.step();
            }
        }

        if (state.sim.isDone()) {
            try dvui.toast(@src(), .{ .message = "Program ended" });
        }
    }
    if (try buttonIconAndLabel(@src(), "reset", dvui.entypo.ccw, opts)) {
        state.sim.reset();
    }
}

fn draw_ui(state: *State) !void {
    var scroll = try dvui.scrollArea(
        @src(),
        .{},
        .{
            .expand = .both,
            .color_fill = .{
                .name = .fill_window,
            },
        },
    );
    defer scroll.deinit();

    var scaler = try dvui.scale(@src(), state.ui_scale, .{ .expand = .horizontal });
    defer scaler.deinit();

    {
        var hbox = try dvui.box(@src(), .horizontal, .{});
        defer hbox.deinit();
        try draw_registers(state);
    }
    try draw_controls(state);
    try draw_asm(state);

    const label = if (dvui.Examples.show_demo_window) "Hide Demo Window" else "Show Demo Window";
    if (try dvui.button(@src(), label, .{}, .{})) {
        dvui.Examples.show_demo_window = !dvui.Examples.show_demo_window;
    }

    {
        var hbox = try dvui.box(@src(), .horizontal, .{});
        defer hbox.deinit();

        if (try dvui.button(@src(), "Zoom In", .{}, .{})) {
            state.ui_scale = @round(dvui.themeGet().font_body.size * state.ui_scale + 1.0) / dvui.themeGet().font_body.size;
        }

        if (try dvui.button(@src(), "Zoom Out", .{}, .{})) {
            state.ui_scale = @round(dvui.themeGet().font_body.size * state.ui_scale - 1.0) / dvui.themeGet().font_body.size;
        }
    }

    // look at demo() for examples of dvui widgets, shows in a floating window
    try dvui.Examples.demo();
}

// Optional: windows os only
const winapi = if (builtin.os.tag == .windows) struct {
    extern "kernel32" fn AttachConsole(dwProcessId: std.os.windows.DWORD) std.os.windows.BOOL;
} else struct {};
