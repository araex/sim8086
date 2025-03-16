const std = @import("std");
const builtin = @import("builtin");

const dvui = @import("dvui");

const x86 = @import("x86.zig");

comptime {
    std.debug.assert(@hasDecl(dvui.backend, "SDLBackend"));
}

const State = struct {
    instr: []const x86.Instruction,
    instr_asm: []const []const u8,
    cur_instr: usize,
    ui_scale: f32,
    allocator: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator, instructions: []x86.Instruction) !State {
        // Allocate a slice to hold all instruction strings
        var instr_asm = try alloc.alloc([]u8, instructions.len);

        // Temporary buffer for formatting each instruction
        var lineBuffer: [256]u8 = undefined;

        // Convert each instruction to its assembly representation
        for (instructions, 0..) |instruction, i| {
            // Format the instruction
            const instruction_string = try std.fmt.bufPrint(
                &lineBuffer,
                "{}\n",
                .{x86.fmt(instruction)},
            );

            // Allocate memory for the string and copy it
            instr_asm[i] = try alloc.dupe(u8, instruction_string);

            // Convert to lowercase if needed
            for (instr_asm[i]) |*c| {
                c.* = std.ascii.toLower(c.*);
            }
        }

        return State{
            .instr = instructions,
            .instr_asm = instr_asm,
            .cur_instr = 0,
            .ui_scale = 1.0,
            .allocator = alloc,
        };
    }

    pub fn deinit(self: *State) void {
        // Free each individual string
        for (self.instr_asm) |asm_str| {
            self.allocator.free(asm_str);
        }
        // Free the slice itself
        self.allocator.free(self.instr_asm);
    }
};

pub fn run(alloc: std.mem.Allocator, instructions: []x86.Instruction) !void {
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

    var state = try State.init(alloc, instructions);
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
        var tl_asm = try dvui.textLayout(
            @src(),
            .{},
            .{ .expand = .horizontal },
        );
        defer tl_asm.deinit();
        for (state.instr_asm, 0..) |line, i| {
            try tl_asm.addText(line, if (i == state.cur_instr) .{
                .font_style = .heading,
                .color_text = .{ .color = dvui.themeGet().color_accent },
            } else .{});
        }
    }

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
