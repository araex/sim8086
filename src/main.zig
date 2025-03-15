const std = @import("std");
const assert = @import("std").debug.assert;
const nasm = @import("nasm.zig");
const pretty = @import("log.zig").pretty;
const x8086 = @import("x8086.zig");
const format = @import("format.zig");

test "encode->decode with ASM diff" {
    try testEncodeDecodeWithAsmDiff("mov si, bx");
    try testEncodeDecodeWithAsmDiff("mov dh, al");
    try testEncodeDecodeWithAsmDiff("mov cl, 12");
    try testEncodeDecodeWithAsmDiff("mov ch, 244");
    try testEncodeDecodeWithAsmDiff("mov dx, 3948");
    try testEncodeDecodeWithAsmDiff("mov al, [bx + si]");
    try testEncodeDecodeWithAsmDiff("mov bx, [bp + di]");
    try testEncodeDecodeWithAsmDiff("mov dx, [bp]");
    try testEncodeDecodeWithAsmDiff("mov ah, [bx + si + 4]");
    try testEncodeDecodeWithAsmDiff("mov al, [bx + si + 4999]");
    try testEncodeDecodeWithAsmDiff("mov [bx + di], cx");
    try testEncodeDecodeWithAsmDiff("mov [bp + si], cl");
    try testEncodeDecodeWithAsmDiff("mov [bp], ch");
    try testEncodeDecodeWithAsmDiff("mov ax, [bx + di - 37]");
    try testEncodeDecodeWithAsmDiff("mov [si - 300], cx");
    try testEncodeDecodeWithAsmDiff("mov dx, [bx - 32]");
    try testEncodeDecodeWithAsmDiff("mov byte [bp + di], 7");
    try testEncodeDecodeWithAsmDiff("mov word [di + 901], 42");
    try testEncodeDecodeWithAsmDiff("mov word [di + 901], 347");
    try testEncodeDecodeWithAsmDiff("mov bp, [5]");
    try testEncodeDecodeWithAsmDiff("mov bx, [3458]");
    try testEncodeDecodeWithAsmDiff("mov ax, [2555]");
    try testEncodeDecodeWithAsmDiff("mov ax, [16]");
    try testEncodeDecodeWithAsmDiff("mov [2554], ax");
    try testEncodeDecodeWithAsmDiff("mov [15], ax");

    try testEncodeDecodeWithAsmDiff("add bx, [bx + si]");
    try testEncodeDecodeWithAsmDiff("add bx, [bp]");
    try testEncodeDecodeWithAsmDiff("add si, 2");
    try testEncodeDecodeWithAsmDiff("add bp, 2");
    try testEncodeDecodeWithAsmDiff("add cx, 8");
    try testEncodeDecodeWithAsmDiff("add bx, [bp]");
    try testEncodeDecodeWithAsmDiff("add cx, [bx + 2]");
    try testEncodeDecodeWithAsmDiff("add bh, [bp + si + 4]");
    try testEncodeDecodeWithAsmDiff("add di, [bp + di + 6]");
    try testEncodeDecodeWithAsmDiff("add [bx + si], bx");
    try testEncodeDecodeWithAsmDiff("add [bp], bx");
    try testEncodeDecodeWithAsmDiff("add [bp], bx");
    try testEncodeDecodeWithAsmDiff("add [bx + 2], cx");
    try testEncodeDecodeWithAsmDiff("add [bp + si + 4], bh");
    try testEncodeDecodeWithAsmDiff("add [bp + di + 6], di");
    try testEncodeDecodeWithAsmDiff("add byte [bx], 34");
    try testEncodeDecodeWithAsmDiff("add word [bp + si + 1000], 29");
    try testEncodeDecodeWithAsmDiff("add ax, [bp]");
    try testEncodeDecodeWithAsmDiff("add al, [bx + si]");
    try testEncodeDecodeWithAsmDiff("add ax, bx");
    try testEncodeDecodeWithAsmDiff("add al, ah");
    try testEncodeDecodeWithAsmDiff("add ax, 1000");
    try testEncodeDecodeWithAsmDiff("add al, 226");
    try testEncodeDecodeWithAsmDiff("add al, 9");

    try testEncodeDecodeWithAsmDiff("sub bx, [bx + si]");
    try testEncodeDecodeWithAsmDiff("sub bx, [bp]");
    try testEncodeDecodeWithAsmDiff("sub si, 2");
    try testEncodeDecodeWithAsmDiff("sub bp, 2");
    try testEncodeDecodeWithAsmDiff("sub cx, 8");
    try testEncodeDecodeWithAsmDiff("sub bx, [bp]");
    try testEncodeDecodeWithAsmDiff("sub cx, [bx + 2]");
    try testEncodeDecodeWithAsmDiff("sub bh, [bp + si + 4]");
    try testEncodeDecodeWithAsmDiff("sub di, [bp + di + 6]");
    try testEncodeDecodeWithAsmDiff("sub [bx + si], bx");
    try testEncodeDecodeWithAsmDiff("sub [bp], bx");
    try testEncodeDecodeWithAsmDiff("sub [bp], bx");
    try testEncodeDecodeWithAsmDiff("sub [bx + 2], cx");
    try testEncodeDecodeWithAsmDiff("sub [bp + si + 4], bh");
    try testEncodeDecodeWithAsmDiff("sub [bp + di + 6], di");
    try testEncodeDecodeWithAsmDiff("sub byte [bx], 34");
    try testEncodeDecodeWithAsmDiff("sub word [bx + di], 29");
    try testEncodeDecodeWithAsmDiff("sub ax, [bp]");
    try testEncodeDecodeWithAsmDiff("sub al, [bx + si]");
    try testEncodeDecodeWithAsmDiff("sub ax, bx");
    try testEncodeDecodeWithAsmDiff("sub al, ah");
    try testEncodeDecodeWithAsmDiff("sub ax, 1000");
    try testEncodeDecodeWithAsmDiff("sub al, 226");
    try testEncodeDecodeWithAsmDiff("sub al, 9");

    try testEncodeDecodeWithAsmDiff("cmp bx, [bx + si]");
    try testEncodeDecodeWithAsmDiff("cmp bx, [bp]");
    try testEncodeDecodeWithAsmDiff("cmp si, 2");
    try testEncodeDecodeWithAsmDiff("cmp bp, 2");
    try testEncodeDecodeWithAsmDiff("cmp cx, 8");
    try testEncodeDecodeWithAsmDiff("cmp bx, [bp]");
    try testEncodeDecodeWithAsmDiff("cmp cx, [bx + 2]");
    try testEncodeDecodeWithAsmDiff("cmp bh, [bp + si + 4]");
    try testEncodeDecodeWithAsmDiff("cmp di, [bp + di + 6]");
    try testEncodeDecodeWithAsmDiff("cmp [bx + si], bx");
    try testEncodeDecodeWithAsmDiff("cmp [bp], bx");
    try testEncodeDecodeWithAsmDiff("cmp [bp], bx");
    try testEncodeDecodeWithAsmDiff("cmp [bx + 2], cx");
    try testEncodeDecodeWithAsmDiff("cmp [bp + si + 4], bh");
    try testEncodeDecodeWithAsmDiff("cmp [bp + di + 6], di");
    try testEncodeDecodeWithAsmDiff("cmp byte [bx], 34");
    try testEncodeDecodeWithAsmDiff("cmp word [4834], 29");
    try testEncodeDecodeWithAsmDiff("cmp ax, [bp]");
    try testEncodeDecodeWithAsmDiff("cmp al, [bx + si]");
    try testEncodeDecodeWithAsmDiff("cmp ax, bx");
    try testEncodeDecodeWithAsmDiff("cmp al, ah");
    try testEncodeDecodeWithAsmDiff("cmp ax, 1000");
    try testEncodeDecodeWithAsmDiff("cmp al, 226");
    try testEncodeDecodeWithAsmDiff("cmp al, 9");
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

test "Homework Part 1 - Listing 41" {
    try testDecodeEncodeListing("listing_0041_add_sub_cmp_jnz");
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

fn testEncodeDecodeWithAsmDiff(comptime asm_instruction: [:0]const u8) !void {
    pretty().print("\nEncode->Decode:", .{});
    pretty()
        .withColor(.bright_blue)
        .print("'{s}'...\n", .{asm_instruction});

    const hash = comptime std.hash.CityHash64.hash(asm_instruction);
    const file_name = std.fmt.comptimePrint("{d}", .{hash});

    const prefix = "bits 16\n";
    const postfix = "\n";
    const want_asm = std.fmt.comptimePrint("{s}{s}{s}", .{ prefix, asm_instruction, postfix });

    // 1. Assemble given ASM to bin
    const alloc = std.testing.allocator;
    const got_bin = try runNasm(alloc, want_asm, file_name);
    defer alloc.free(got_bin);

    // 2. Decode assembled bin
    const instructions = try x8086.decode(alloc, got_bin);
    defer instructions.deinit();
    const got_asm = try format.toAsm(alloc, instructions.items);
    defer got_asm.deinit();

    // 3. Diff decoded ASM to original ASM
    try std.testing.expectEqualSlices(u8, want_asm, got_asm.items);

    // 4. Encode the decoded ASM again
    const reassembled_bin = try runNasm(alloc, got_asm.items, file_name);
    defer alloc.free(reassembled_bin);

    // 5. Diff binaries from 1 & 4
    try std.testing.expectEqualSlices(u8, got_bin, reassembled_bin);

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
