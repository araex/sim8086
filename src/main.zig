const std = @import("std");
const assert = @import("std").debug.assert;
const nasm = @import("nasm.zig");
const x8086 = @import("x8086.zig");

// pub fn main() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
//     const alloc = gpa.allocator();

//     var args = try std.process.argsWithAllocator(alloc);
//     while (args.next()) |arg| {
//         std.debug.print("{s}\n", .{arg});
//     }
// }

fn withoutCommentsAndEmptyLines(
    alloc: std.mem.Allocator,
    data: []const u8,
) ![]u8 {
    var result = std.ArrayList(u8).init(alloc);
    var it = std.mem.tokenizeScalar(u8, data, '\n');
    while (it.next()) |line| {
        if (std.mem.startsWith(u8, line, ";")) {
            continue;
        }
        if (line.len == 1 and line[0] == '\n') {
            continue;
        }
        if (result.items.len > 0) {
            try result.append('\n');
        }
        try result.appendSlice(line);
    }
    if (data[data.len - 1] == '\n') {
        try result.append('\n');
    }
    return result.toOwnedSlice();
}

test "withoutCommentsAndEmptyLiness" {
    const TestCase = struct {
        input: [:0]const u8,
        expected: [:0]const u8,
    };

    const test_cases = [_]TestCase{
        .{
            .input =
            \\; This stripped
            \\ This is kept
            \\; This stripped
            \\; This stripped
            \\
            \\; This stripped
            \\ This is kept
            ,
            .expected =
            \\ This is kept
            \\ This is kept
            ,
        },
        .{
            .input =
            \\ Last newline is kept
            \\
            ,
            .expected =
            \\ Last newline is kept
            \\
            ,
        },
    };

    const alloc = std.testing.allocator;

    for (test_cases) |case| {
        const actual = try withoutCommentsAndEmptyLines(alloc, case.input);
        defer alloc.free(actual);
        try std.testing.expectEqualSlices(u8, case.expected, actual);
    }
}

const PrettyDebugPrinter = struct {
    cfg: std.io.tty.Config,
    color: ?std.io.tty.Color,

    pub fn withColor(
        self: PrettyDebugPrinter,
        color: std.io.tty.Color,
    ) PrettyDebugPrinter {
        return PrettyDebugPrinter{
            .cfg = self.cfg,
            .color = color,
        };
    }

    pub fn print(
        self: PrettyDebugPrinter,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        if (self.color != null) self.cfg.setColor(std.io.getStdErr(), self.color.?) catch return;
        std.debug.print(fmt, args);
        if (self.color != null) self.cfg.setColor(std.io.getStdErr(), .reset) catch return;
    }
};

fn debugOut() PrettyDebugPrinter {
    const state = struct {
        var printer: PrettyDebugPrinter = undefined;
        var is_initialized = false;
    };
    if (!state.is_initialized) {
        state.printer = PrettyDebugPrinter{
            .cfg = std.io.tty.detectConfig(std.io.getStdErr()),
            .color = null,
        };
    }
    return state.printer;
}

fn runNasm(alloc: std.mem.Allocator, got_asm: []const u8, file_name: []const u8) ![]u8 {
    debugOut().print("Assemble decoded asm...\n", .{});
    try debugOut().cfg.setColor(std.io.getStdErr(), .bright_black);
    defer debugOut().cfg.setColor(std.io.getStdErr(), .reset) catch {};
    return try nasm.assemble(alloc, got_asm, file_name);
}

fn testDecodeEncode(comptime listing_file_name: []const u8) !void {
    const data_dir = "data";
    const in_asm_path = std.fmt.comptimePrint("{s}/{s}.asm", .{ data_dir, listing_file_name });
    const in_bin_path = std.fmt.comptimePrint("{s}/{s}", .{ data_dir, listing_file_name });
    debugOut().print("\nDecode ", .{});
    debugOut()
        .withColor(.bright_blue)
        .print("'{s}'...\n", .{in_bin_path});

    const alloc = std.testing.allocator;
    const want_asm_with_comments = @embedFile(in_asm_path);
    const want_asm = try withoutCommentsAndEmptyLines(alloc, want_asm_with_comments);
    defer alloc.free(want_asm);
    const want_bin = @embedFile(in_bin_path);

    const got_asm = try x8086.decode(alloc, want_bin[0..]);
    defer alloc.free(got_asm);

    debugOut().print("Compare decoded asm to '{s}'...\n", .{in_asm_path});
    try std.testing.expectEqualSlices(u8, want_asm, got_asm);

    const got_bin = try runNasm(alloc, got_asm, listing_file_name);
    defer alloc.free(got_bin);

    debugOut().print("Compare assembled binary to ground truth...\n", .{});
    try std.testing.expectEqualSlices(u8, want_bin, got_bin);

    debugOut()
        .withColor(.bright_green)
        .print("Success!\n", .{});
}

test "Homework Part 1 - Listing 37" {
    try testDecodeEncode("listing_0037_single_register_mov");
}

test "Homework Part 1 - Listing 38" {
    try testDecodeEncode("listing_0038_many_register_mov");
}

// test "Homework Part 1 - Listing 39" {
//     try testDecodeEncode("listing_0039_more_movs");
// }
