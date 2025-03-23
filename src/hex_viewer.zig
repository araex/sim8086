const std = @import("std");

const dvui = @import("dvui");

const x86 = @import("x86.zig");

pub fn hexViewer(_: std.builtin.SourceLocation, mem: x86.Memory, _: dvui.Options) !void {
    const Data = struct {
        var start_addr: usize = 0;
    };
    var vbox = try dvui.box(@src(), .vertical, .{});
    defer vbox.deinit();
    {
        var hbox = try dvui.box(@src(), .horizontal, .{});
        defer hbox.deinit();

        try dvui.label(@src(), "Jump to:", .{}, .{});
        const result = try dvui.textEntryNumber(
            @src(),
            usize,
            .{ .min = 0, .max = mem.data.len },
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
    {
        // Display a fixed number of rows (10) starting at start_addr
        const bytesPerRow: usize = 16;
        const numRowsToShow: usize = 10;
        const row_start = (Data.start_addr / bytesPerRow) * bytesPerRow;
        var tl = try dvui.textLayout(@src(), .{}, .{ .expand = .both });
        defer tl.deinit();
        for (0..numRowsToShow) |i| {
            const addr: usize = row_start + i * bytesPerRow;
            try tl.format("0x{X:0<4} ", .{addr}, .{});

            if (addr < mem.data.len) {
                const row_data = mem.data[addr..@min(addr + bytesPerRow, mem.data.len)];
                for (row_data) |byte| {
                    try tl.format("{X:0<2} ", .{byte}, .{});
                }
            } else {
                try tl.addText("   ", .{});
            }

            if (i < numRowsToShow - 1) {
                try tl.addText("\n", .{});
            }
        }
    }
}
