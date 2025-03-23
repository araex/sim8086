const std = @import("std");

const dvui = @import("dvui");

const theme = @import("theme.zig");
const x86 = @import("x86.zig");

pub fn drawHexView(sim: x86.Simulator) !void {
    const Data = struct {
        var start_addr: usize = 0;
    };
    var vbox = try dvui.box(@src(), .vertical, .{});
    defer vbox.deinit();
    {
        const ip = sim.registers.getWord(.IP);
        const cur_instr_len = if (sim.isValidInstructionPointer()) x86.size.instruction(sim.getCurrentInstruction()) else 0;

        // Display a fixed number of rows (10) starting at start_addr
        const bytesPerRow: usize = 16;
        const numRowsToShow: usize = 10;
        const row_start = (Data.start_addr / bytesPerRow) * bytesPerRow;
        var tl = try dvui.textLayout(@src(), .{}, .{ .expand = .both });
        defer tl.deinit();

        try tl.addText("       00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n", theme.optionTextDim());
        for (0..numRowsToShow) |i| {
            const addr: usize = row_start + i * bytesPerRow;
            try tl.format("0x{X:0>4} ", .{addr}, theme.optionTextDim());

            if (addr < sim.memory.data.len) {
                const row_data = sim.memory.data[addr..@min(addr + bytesPerRow, sim.memory.data.len)];
                for (row_data, 0..) |byte, offset| {
                    const cur_addr = addr + offset;
                    const is_cur_instr = cur_addr >= ip and cur_addr < ip + cur_instr_len;
                    const opts = if (is_cur_instr) theme.optionTextHighlight() else if (cur_addr < sim.program_length) theme.optionTextDim() else dvui.Options{};
                    try tl.format("{X:0<2} ", .{byte}, opts);
                }
            } else {
                try tl.addText("   ", .{});
            }

            if (i < numRowsToShow - 1) {
                try tl.addText("\n", .{});
            }
        }
    }

    {
        var hbox = try dvui.box(@src(), .horizontal, .{});
        defer hbox.deinit();

        try dvui.label(@src(), "Jump to:", .{}, .{});
        const result = try dvui.textEntryNumber(
            @src(),
            usize,
            .{ .min = 0, .max = sim.memory.data.len },
            .{ .min_size_content = dvui.Options.sizeM(8, 1) },
        );
        const label = switch (result.value) {
            .TooBig => "Too Big",
            .TooSmall => "Too Small",
            .Invalid => "Invalid",
            .Valid, .Empty => " ",
        };
        try dvui.labelNoFmt(@src(), label, .{});
        if (result.value == .Valid and result.enter_pressed) {
            Data.start_addr = result.value.Valid;
        }
    }
}
