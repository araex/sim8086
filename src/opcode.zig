const std = @import("std");
const assert = @import("std").debug.assert;

pub const Mnemonic = enum {
    Unknown,
    // move
    mov_rm_to_from_r,
    mov_imm_to_rm,
    mov_imm_to_r,
    mov_mem_to_accumulator,
    mov_accumulator_to_mem,
    // add
    add_rm_with_r_to_either,
    add_imm_to_rm,
    add_imm_to_acc,
    // sub
    // cmp
    // jumps
};

// http://ref.x86asm.net/coder32.html#xB8
const opcode_table = [256]Mnemonic{
    // 0x00
    .add_rm_with_r_to_either,
    .add_rm_with_r_to_either,
    .add_rm_with_r_to_either,
    .add_rm_with_r_to_either,
    .add_imm_to_acc,
    .add_imm_to_acc,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x0F
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x1F
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x2F
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x3F
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x4F
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x5F
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x6F
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x7F
    .Unknown, // 0x80: needs opcode extension
    .Unknown, // 0x81: needs opcode extension
    .Unknown, // 0x82: needs opcode extension
    .Unknown, // 0x83: needs opcode extension
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .mov_rm_to_from_r,
    .mov_rm_to_from_r,
    .mov_rm_to_from_r,
    .mov_rm_to_from_r,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x8F
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0x9F
    .mov_mem_to_accumulator,
    .mov_mem_to_accumulator,
    .mov_accumulator_to_mem,
    .mov_accumulator_to_mem,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0xAF
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    // 0xBF
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .mov_imm_to_rm,
    .mov_imm_to_rm,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0xCF
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0xDF
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    // 0xEF
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
};

const opcode_extension_table_0x80 = [_]Mnemonic{
    .add_imm_to_rm,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
};
const opcode_extension_table_0x81 = [_]Mnemonic{
    .add_imm_to_rm,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
};
const opcode_extension_table_0x82 = [_]Mnemonic{
    .add_imm_to_rm,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
};
const opcode_extension_table_0x83 = [_]Mnemonic{
    .add_imm_to_rm,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
};

fn hasOpcodeExtension(byte: u8) bool {
    return byte == 0x80 or byte == 0x81 or byte == 0x82 or byte == 0x83;
}

fn decodeWithExtensions(byte_1: u8, byte_2: u8) Mnemonic {
    const ext_bits: u3 = @intCast((byte_2 & 0b00111000) >> 3);
    switch (byte_1) {
        0x80 => return opcode_extension_table_0x80[ext_bits],
        0x81 => return opcode_extension_table_0x81[ext_bits],
        0x82 => return opcode_extension_table_0x82[ext_bits],
        0x83 => return opcode_extension_table_0x83[ext_bits],
        else => unreachable,
    }
}

pub fn decode(byte_1: u8, byte_2: u8) Mnemonic {
    if (!hasOpcodeExtension(byte_1)) {
        return opcode_table[byte_1];
    }
    return decodeWithExtensions(byte_1, byte_2);
}

test "decode" {
    const TestCase = struct {
        in: [2]u8,
        expected: Mnemonic,
    };
    const test_cases = [_]TestCase{
        .{ .in = .{ 0b10011000, 0 }, .expected = .Unknown },
        .{ .in = .{ 0b10001100, 0 }, .expected = .Unknown },
        .{ .in = .{ 0x88, 0 }, .expected = .mov_rm_to_from_r },
        .{ .in = .{ 0x89, 0 }, .expected = .mov_rm_to_from_r },
        .{ .in = .{ 0x8A, 0 }, .expected = .mov_rm_to_from_r },
        .{ .in = .{ 0x8B, 0 }, .expected = .mov_rm_to_from_r },
        .{ .in = .{ 0xB0, 0 }, .expected = .mov_imm_to_r },
        .{ .in = .{ 0xB8, 0 }, .expected = .mov_imm_to_r },
        .{ .in = .{ 0xC6, 0 }, .expected = .mov_imm_to_rm },
        .{ .in = .{ 0xC7, 0 }, .expected = .mov_imm_to_rm },
        .{ .in = .{ 0xA0, 0 }, .expected = .mov_mem_to_accumulator },
        .{ .in = .{ 0xA1, 0 }, .expected = .mov_mem_to_accumulator },
        .{ .in = .{ 0b10100010, 0 }, .expected = .mov_accumulator_to_mem },
        .{ .in = .{ 0b10100011, 0 }, .expected = .mov_accumulator_to_mem },
        .{ .in = .{ 0x80, 0 }, .expected = .add_imm_to_rm },
        .{ .in = .{ 0x81, 0 }, .expected = .add_imm_to_rm },
        .{ .in = .{ 0x82, 0 }, .expected = .add_imm_to_rm },
        .{ .in = .{ 0x83, 0 }, .expected = .add_imm_to_rm },
        .{ .in = .{ 0x04, 0 }, .expected = .add_imm_to_acc },
        .{ .in = .{ 0x05, 0 }, .expected = .add_imm_to_acc },
        .{ .in = .{ 0x00, 0b0000000 }, .expected = .add_rm_with_r_to_either },
        .{ .in = .{ 0x01, 0b0000000 }, .expected = .add_rm_with_r_to_either },
        .{ .in = .{ 0x02, 0b0000000 }, .expected = .add_rm_with_r_to_either },
        .{ .in = .{ 0x03, 0b0000000 }, .expected = .add_rm_with_r_to_either },
    };

    for (test_cases) |case| {
        const actual = decode(case.in[0], case.in[1]);
        if (actual != case.expected) {
            std.debug.print("0b{b}: want: {}, got: {}\n", .{ case.in, case.expected, actual });
            return error.TestExpectedEqual;
        }
    }
}
