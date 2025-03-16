const assert = @import("std").debug.assert;
const std = @import("std");

const nasm = @import("nasm.zig");
const x86 = @import("x86.zig");
const x86_SimRegisters = @import("x86/simulator.zig").Registers;

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

test "decode instructions" {
    const TestCase = struct {
        name: [:0]const u8,
        in: []const u8,
        expected: x86.Instruction,
    };
    const test_cases = [_]TestCase{
        .{
            .name = "mov BX, AX",
            .in = &[_]u8{ 0b10001001, 0b11000011 },
            .expected = x86.makeInstruction(
                .mov_rm_to_from_r,
                .Word,
                x86.Register.BX,
                x86.Register.AX,
            ),
        },
        .{
            .name = "mov AX, BX",
            .in = &[2]u8{ 0b10001011, 0b11000011 },
            .expected = x86.makeInstruction(
                .mov_rm_to_from_r,
                .Word,
                x86.Register.AX,
                x86.Register.BX,
            ),
        },
        .{
            .name = "mov BL, AL",
            .in = &[2]u8{ 0b10001000, 0b11000011 },
            .expected = x86.makeInstruction(
                .mov_rm_to_from_r,
                .Byte,
                x86.Register.BL,
                x86.Register.AL,
            ),
        },
        .{
            .name = "mov CL, 42",
            .in = &[2]u8{ 0b10110001, 42 },
            .expected = x86.makeInstruction(
                .mov_imm_to_r,
                .Byte,
                x86.Register.CL,
                @as(u8, 42),
            ),
        },
        .{
            .name = "mov CL, -42",
            .in = &[2]u8{ 0b10110001, 0b11010110 },
            .expected = x86.makeInstruction(
                .mov_imm_to_r,
                .Byte,
                x86.Register.CL,
                @as(u8, 256 - 42),
            ),
        },
        .{
            .name = "mov BX, 256",
            .in = &[_]u8{ 0b10111011, 0x0, 0x0001 },
            .expected = x86.makeInstruction(
                .mov_imm_to_r,
                .Word,
                x86.Register.BX,
                @as(u16, 256),
            ),
        },
        .{
            .name = "mov [di + 256], word 515",
            .in = &[_]u8{ 0b11000111, 0b10000101, 0x0, 0x1, 0x3, 0x2 },
            .expected = x86.makeInstruction(
                .mov_imm_to_rm,
                .Word,
                x86.Memory{
                    .calc = x86.EffectiveAddressCalculation.DI,
                    .displacement = x86.Displacement{
                        .word = 256,
                    },
                },
                x86.makeImmediate(@as(u16, 515)),
            ),
        },
        .{
            .name = "add cl, 8",
            .in = &[_]u8{ 0x80, 0b11000001, 0x8 },
            .expected = x86.makeInstruction(
                .add_imm_to_rm,
                .Byte,
                x86.Register.CL,
                @as(u8, 8),
            ),
        },
        .{
            .name = "add cx, 256",
            .in = &[_]u8{ 0x81, 0b11000001, 0x0, 0x1 },
            .expected = x86.makeInstruction(
                .add_imm_to_rm,
                .Word,
                x86.Register.CX,
                @as(u16, 256),
            ),
        },
        .{
            .name = "add cl, -8",
            .in = &[_]u8{ 0x82, 0b11000001, 0b11110111 },
            .expected = x86.makeInstruction(
                .add_imm_to_rm,
                .Byte,
                x86.Register.CL,
                @as(u8, 247),
            ),
        },
        .{
            .name = "add al, 8",
            .in = &[_]u8{ 0x04, 0x08 },
            .expected = x86.makeInstruction(
                .add_imm_to_acc,
                .Byte,
                x86.Register.AL,
                @as(u8, 8),
            ),
        },
        .{
            .name = "sub cl, 8",
            .in = &[_]u8{ 0x80, 0b11101001, 0x8 },
            .expected = x86.makeInstruction(
                .sub_imm_to_rm,
                .Byte,
                x86.Register.CL,
                @as(u8, 8),
            ),
        },
        .{
            .name = "sub cx, 256",
            .in = &[_]u8{ 0x81, 0b11101001, 0x0, 0x1 },
            .expected = x86.makeInstruction(
                .sub_imm_to_rm,
                .Word,
                x86.Register.CX,
                @as(u16, 256),
            ),
        },
        .{
            .name = "sub cx, -8",
            .in = &[_]u8{ 0x83, 0b11101001, 0b11110111 },
            .expected = x86.makeInstruction(
                .sub_imm_to_rm,
                .Word,
                x86.Register.CX,
                @as(u8, 247),
            ),
        },
        .{
            .name = "sub al, 8",
            .in = &[_]u8{ 0x2C, 0x08 },
            .expected = x86.makeInstruction(
                .sub_imm_to_acc,
                .Byte,
                x86.Register.AL,
                @as(u8, 8),
            ),
        },
    };

    const log = std.log.scoped(.test_decode_instructions);
    for (test_cases) |case| {
        log.debug("-> {s}", .{case.name});
        var stream = std.io.fixedBufferStream(case.in[0..]);
        var reader = stream.reader().any();
        const actual = try x86.decodeInstruction(&reader);

        std.testing.expectEqual(case.expected, actual) catch |err| {
            log.err("FAILURE!\n", .{});
            log.err("Expected:\n", .{});
            try std.json.stringify(case.expected, .{}, std.io.getStdErr().writer());
            log.err("\nGot:\n", .{});
            try std.json.stringify(actual, .{}, std.io.getStdErr().writer());
            return err;
        };
    }
    log.debug("Success!\n", .{});
}

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

    try testEncodeDecodeWithAsmDiff("jne $+4+0");
    try testEncodeDecodeWithAsmDiff("jne $-2+0");
    try testEncodeDecodeWithAsmDiff("jne $-4+0");
    try testEncodeDecodeWithAsmDiff("jne $-2+0");
    try testEncodeDecodeWithAsmDiff("je $+0");
    try testEncodeDecodeWithAsmDiff("jl $-2+0");
    try testEncodeDecodeWithAsmDiff("jle $-4+0");
    try testEncodeDecodeWithAsmDiff("jb $-6+0");
    try testEncodeDecodeWithAsmDiff("jbe $-8+0");
    try testEncodeDecodeWithAsmDiff("jp $-10+0");
    try testEncodeDecodeWithAsmDiff("jo $-12+0");
    try testEncodeDecodeWithAsmDiff("js $-14+0");
    try testEncodeDecodeWithAsmDiff("jne $-16+0");
    try testEncodeDecodeWithAsmDiff("jnl $-18+0");
    try testEncodeDecodeWithAsmDiff("jnle $-20+0");
    try testEncodeDecodeWithAsmDiff("jnb $-22+0");
    try testEncodeDecodeWithAsmDiff("jnbe $-24+0");
    try testEncodeDecodeWithAsmDiff("jnp $-26+0");
    try testEncodeDecodeWithAsmDiff("jno $-28+0");
    try testEncodeDecodeWithAsmDiff("jns $-30+0");
    try testEncodeDecodeWithAsmDiff("loop $-32+0");
    try testEncodeDecodeWithAsmDiff("loopz $-34+0");
    try testEncodeDecodeWithAsmDiff("loopnz $-36+0");
    try testEncodeDecodeWithAsmDiff("jcxz $-38+0");
}

fn expectEqualRegisters(expected: x86_SimRegisters, actual: x86_SimRegisters) !void {
    try std.testing.expectEqual(expected.getByte(.AL), actual.getByte(.AL));
    try std.testing.expectEqual(expected.getByte(.AH), actual.getByte(.AH));
    try std.testing.expectEqual(expected.getByte(.BL), actual.getByte(.BL));
    try std.testing.expectEqual(expected.getByte(.BH), actual.getByte(.BH));
    try std.testing.expectEqual(expected.getByte(.CL), actual.getByte(.CL));
    try std.testing.expectEqual(expected.getByte(.CH), actual.getByte(.CH));
    try std.testing.expectEqual(expected.getByte(.DL), actual.getByte(.DL));
    try std.testing.expectEqual(expected.getByte(.DH), actual.getByte(.DH));

    try std.testing.expectEqual(expected.getWord(.AX), actual.getWord(.AX));
    try std.testing.expectEqual(expected.getWord(.BX), actual.getWord(.BX));
    try std.testing.expectEqual(expected.getWord(.CX), actual.getWord(.CX));
    try std.testing.expectEqual(expected.getWord(.DX), actual.getWord(.DX));
    try std.testing.expectEqual(expected.getWord(.SP), actual.getWord(.SP));
    try std.testing.expectEqual(expected.getWord(.BP), actual.getWord(.BP));
    try std.testing.expectEqual(expected.getWord(.SI), actual.getWord(.SI));
    try std.testing.expectEqual(expected.getWord(.DI), actual.getWord(.DI));
}

test "Simulator" {
    const asm_instructions =
        \\bits 16
        \\mov al, 5
        \\mov bx, 3
    ;

    const alloc = std.testing.allocator;
    const file_name = getUniqueFileName(asm_instructions);
    const bin = try runNasm(alloc, asm_instructions, file_name);
    defer alloc.free(bin);

    const instructions = try x86.decode(alloc, bin);
    defer instructions.deinit();

    var expected_registers = x86_SimRegisters{};
    var sim = try x86.Simulator.init(instructions.items);
    try std.testing.expectEqual(0, sim.cur_instruction);
    try expectEqualRegisters(expected_registers, sim.registers);

    expected_registers.setByte(.AL, 5);
    try sim.step();
    try expectEqualRegisters(expected_registers, sim.registers);

    expected_registers.setWord(.BX, 3);
    try sim.step();
}

fn testEncodeDecodeWithAsmDiff(comptime asm_instruction: [:0]const u8) !void {
    std.log.info("Encode->Decode '{s}'...", .{asm_instruction});

    const file_name = getUniqueFileName(asm_instruction);

    const prefix = "bits 16\n";
    const postfix = "\n";
    const want_asm = std.fmt.comptimePrint("{s}{s}{s}", .{ prefix, asm_instruction, postfix });

    // 1. Assemble given ASM to bin
    const alloc = std.testing.allocator;
    const got_bin = try runNasm(alloc, want_asm, file_name);
    defer alloc.free(got_bin);

    // 2. Decode assembled bin
    const instructions = try x86.decode(alloc, got_bin);
    defer instructions.deinit();
    const got_asm = try x86.toAsm(alloc, instructions.items);
    defer got_asm.deinit();

    // 3. Diff decoded ASM to original ASM
    try std.testing.expectEqualSlices(u8, want_asm, got_asm.items);

    // 4. Encode the decoded ASM again
    const reassembled_bin = try runNasm(alloc, got_asm.items, file_name);
    defer alloc.free(reassembled_bin);

    // 5. Diff binaries from 1 & 4
    try std.testing.expectEqualSlices(u8, got_bin, reassembled_bin);

    std.log.info("Success!", .{});
}

fn runNasm(alloc: std.mem.Allocator, asm_content: []const u8, file_name: []const u8) ![]u8 {
    std.log.debug("Assemble decoded asm...\n", .{});
    return try nasm.assemble(alloc, asm_content, file_name);
}

// 1. Read given bin from data folder
// 2. Decode bin to ASM
// 3. Assemble decoded ASM to new bin
// 4. Diff reassembled bin to original bin
fn testDecodeEncodeListing(comptime listing_file_name: []const u8) !void {
    const data_dir = "data";
    const in_asm_path = std.fmt.comptimePrint("{s}/{s}.asm", .{ data_dir, listing_file_name });
    const in_bin_path = std.fmt.comptimePrint("{s}/{s}", .{ data_dir, listing_file_name });
    std.log.debug("Decode '{s}'...", .{in_bin_path});

    const alloc = std.testing.allocator;
    const want_asm_with_comments = @embedFile(in_asm_path);
    const want_asm = try withoutCommentsAndEmptyLines(alloc, want_asm_with_comments);
    defer alloc.free(want_asm);
    const want_bin = @embedFile(in_bin_path);

    const instructions = try x86.decode(alloc, want_bin[0..]);
    defer instructions.deinit();

    const got_asm = try x86.toAsm(alloc, instructions.items);
    defer got_asm.deinit();

    const got_bin = try runNasm(alloc, got_asm.items, listing_file_name);
    defer alloc.free(got_bin);

    std.log.debug("Compare assembled binary to ground truth...\n", .{});
    std.testing.expectEqualSlices(u8, want_bin, got_bin) catch |e| {
        std.log.err("- Compiled binary differs from ground truth. Diffing decoded ASM...\n", .{});
        std.testing.expectEqualSlices(u8, want_asm, got_asm.items) catch {};
        return e;
    };

    std.log.info("Success!\n", .{});
}

fn getUniqueFileName(comptime asm_instruction: [:0]const u8) []const u8 {
    const hash = comptime std.hash.CityHash64.hash(asm_instruction);
    return &std.fmt.comptimePrint("{d}", .{hash}).*;
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
