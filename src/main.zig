const std = @import("std");
const assert = @import("std").debug.assert;
const nasm = @import("nasm.zig");
const pretty = @import("log.zig").pretty;
const x8086 = @import("x8086.zig");
const format = @import("format.zig");

test "individual instructions" {
    try testEncodeDecode("mov si, bx");
    try testEncodeDecode("mov dh, al");
    try testEncodeDecode("mov cl, 12");
    try testEncodeDecode("mov ch, 244");
    try testEncodeDecode("mov dx, 3948");
    try testEncodeDecode("mov al, [bx + si]");
    try testEncodeDecode("mov bx, [bp + di]");
    try testEncodeDecode("mov dx, [bp]");
    try testEncodeDecode("mov ah, [bx + si + 4]");
    try testEncodeDecode("mov al, [bx + si + 4999]");
    try testEncodeDecode("mov [bx + di], cx");
    try testEncodeDecode("mov [bp + si], cl");
    try testEncodeDecode("mov [bp], ch");
    try testEncodeDecode("mov ax, [bx + di - 37]");
    try testEncodeDecode("mov [si - 300], cx");
    try testEncodeDecode("mov dx, [bx - 32]");
    try testEncodeDecode("mov [bp + di], byte 7");
    try testEncodeDecode("mov [di + 901], word 347");
    try testEncodeDecode("mov bp, [5]");
    try testEncodeDecode("mov bx, [3458]");
    try testEncodeDecode("mov ax, [2555]");
    try testEncodeDecode("mov ax, [16]");
    try testEncodeDecode("mov [2554], ax");
    try testEncodeDecode("mov [15], ax");

    try testEncodeDecode("add bx, [bx + si]");
    try testEncodeDecode("add bx, [bp]");
    try testEncodeDecode("add si, 2");
    try testEncodeDecode("add bp, 2");
    try testEncodeDecode("add cx, 8");
    try testEncodeDecode("add bx, [bp]");
    try testEncodeDecode("add cx, [bx + 2]");
    try testEncodeDecode("add bh, [bp + si + 4]");
    try testEncodeDecode("add di, [bp + di + 6]");
    try testEncodeDecode("add [bx + si], bx");
    try testEncodeDecode("add [bp], bx");
    try testEncodeDecode("add [bp], bx");
    try testEncodeDecode("add [bx + 2], cx");
    try testEncodeDecode("add [bp + si + 4], bh");
    try testEncodeDecode("add [bp + di + 6], di");
    // try testEncodeDecode("add byte [bx], 34");
    // try testEncodeDecode("add word [bp + si + 1000], 29");
    try testEncodeDecode("add ax, [bp]");
    try testEncodeDecode("add al, [bx + si]");
    try testEncodeDecode("add ax, bx");
    try testEncodeDecode("add al, ah");
    try testEncodeDecode("add ax, 1000");
    try testEncodeDecode("add al, 226");
    try testEncodeDecode("add al, 9");

    try testEncodeDecode("sub bx, [bx + si]");
    try testEncodeDecode("sub bx, [bp]");
    try testEncodeDecode("sub si, 2");
    try testEncodeDecode("sub bp, 2");
    try testEncodeDecode("sub cx, 8");
    try testEncodeDecode("sub bx, [bp]");
    try testEncodeDecode("sub cx, [bx + 2]");
    try testEncodeDecode("sub bh, [bp + si + 4]");
    try testEncodeDecode("sub di, [bp + di + 6]");
    try testEncodeDecode("sub [bx + si], bx");
    try testEncodeDecode("sub [bp], bx");
    try testEncodeDecode("sub [bp], bx");
    try testEncodeDecode("sub [bx + 2], cx");
    try testEncodeDecode("sub [bp + si + 4], bh");
    try testEncodeDecode("sub [bp + di + 6], di");
    // try testEncodeDecode("sub byte [bx], 34");
    // try testEncodeDecode("sub word [bx + di], 29");
    try testEncodeDecode("sub ax, [bp]");
    try testEncodeDecode("sub al, [bx + si]");
    try testEncodeDecode("sub ax, bx");
    try testEncodeDecode("sub al, ah");
    try testEncodeDecode("sub ax, 1000");
    try testEncodeDecode("sub al, 226");
    try testEncodeDecode("sub al, 9");
}

test "Homework" {
    try testDecodeEncodeListing("listing_0037_single_register_mov");
}

test "Homework Part 1 - Listing 38" {
    try testDecodeEncodeListing("listing_0038_many_register_mov");
}

test "Homework Part 1 - Listing 39" {
    try testDecodeEncodeListing("listing_0039_more_movs");
}

test "Homework Part 1 - Listing 40" {
    try testDecodeEncodeListing("listing_0040_challenge_movs");
}

// 1. Read given bin from data folder
// 2. Decode bin to ASM
// 3. Assemble decoded ASM to new bin
// 4. Diff reassembled bin to original bin
fn testDecodeEncodeListing(comptime listing_file_name: []const u8) !void {
    const data_dir = "data";
    const in_asm_path = std.fmt.comptimePrint("{s}/{s}.asm", .{ data_dir, listing_file_name });
    const in_bin_path = std.fmt.comptimePrint("{s}/{s}", .{ data_dir, listing_file_name });
    pretty().print("\nDecode ", .{});
    pretty()
        .withColor(.bright_blue)
        .print("'{s}'...\n", .{in_bin_path});

    const alloc = std.testing.allocator;
    const want_asm_with_comments = @embedFile(in_asm_path);
    const want_asm = try withoutCommentsAndEmptyLines(alloc, want_asm_with_comments);
    defer alloc.free(want_asm);
    const want_bin = @embedFile(in_bin_path);

    const instructions = try x8086.decode(alloc, want_bin[0..]);
    defer instructions.deinit();

    const got_asm = try format.toAsm(alloc, instructions.items);
    defer got_asm.deinit();

    const got_bin = try runNasm(alloc, got_asm.items, listing_file_name);
    defer alloc.free(got_bin);

    pretty().print("Compare assembled binary to ground truth...\n", .{});
    std.testing.expectEqualSlices(u8, want_bin, got_bin) catch |e| {
        pretty()
            .withColor(.red)
            .print("- Compiled binary differs from ground truth. Diffing decoded ASM...\n", .{});
        std.testing.expectEqualSlices(u8, want_asm, got_asm.items) catch {};
        return e;
    };

    pretty()
        .withColor(.bright_green)
        .print("Success!\n", .{});
}

// 1. Assemble given ASM to bin
// 2. Decode assembled bin
// 3. Diff decoded ASM to original ASM
fn testEncodeDecode(comptime asm_instruction: [:0]const u8) !void {
    pretty().print("\nEncode->Decode:", .{});
    pretty()
        .withColor(.bright_blue)
        .print("'{s}'...\n", .{asm_instruction});

    const hash = comptime std.hash.CityHash64.hash(asm_instruction);
    const file_name = std.fmt.comptimePrint("{d}", .{hash});

    const prefix = "bits 16\n";
    const postfix = "\n";
    const want_asm = std.fmt.comptimePrint("{s}{s}{s}", .{ prefix, asm_instruction, postfix });

    const alloc = std.testing.allocator;
    const got_bin = try runNasm(alloc, want_asm, file_name);
    defer alloc.free(got_bin);

    const instructions = try x8086.decode(alloc, got_bin);
    defer instructions.deinit();

    const got_asm = try format.toAsm(alloc, instructions.items);
    defer got_asm.deinit();

    try std.testing.expectEqualSlices(u8, want_asm, got_asm.items);
    pretty()
        .withColor(.bright_green)
        .print("Success!\n", .{});
}

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

fn runNasm(alloc: std.mem.Allocator, asm_content: []const u8, file_name: []const u8) ![]u8 {
    pretty().print("Assemble decoded asm...\n", .{});
    try pretty().cfg.setColor(std.io.getStdErr(), .bright_black);
    defer pretty().cfg.setColor(std.io.getStdErr(), .reset) catch {};
    return try nasm.assemble(alloc, asm_content, file_name);
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
