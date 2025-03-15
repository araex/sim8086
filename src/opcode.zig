const std = @import("std");
const assert = @import("std").debug.assert;

pub const Mnemonic = enum {
    Unknown,
    mov_rm_to_from_r,
    mov_imm_to_rm,
    mov_imm_to_r,
    mov_mem_to_accumulator,
    mov_accumulator_to_mem,
    add_rm_with_r_to_either,
    add_imm_to_rm,
    add_imm_to_acc,
    sub_rm_and_r_to_either,
    sub_imm_to_rm,
    sub_imm_to_acc,
    cmp_rm_with_r,
    cmp_imm_with_rm,
    cmp_imm_with_acc,
    // cmp
    // jumps
};

// http://ref.x86asm.net/coder32.html#xB8
const opcode_table = [256]Mnemonic{
    .add_rm_with_r_to_either, // <- 0x00
    .add_rm_with_r_to_either,
    .add_rm_with_r_to_either,
    .add_rm_with_r_to_either,
    .add_imm_to_acc,
    .add_imm_to_acc,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x08
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x10
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x18
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x20
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .sub_rm_and_r_to_either, // <-0x28
    .sub_rm_and_r_to_either,
    .sub_rm_and_r_to_either,
    .sub_rm_and_r_to_either,
    .sub_imm_to_acc,
    .sub_imm_to_acc,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x30
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .cmp_rm_with_r, // <- 0x38
    .cmp_rm_with_r,
    .cmp_rm_with_r,
    .cmp_rm_with_r,
    .cmp_imm_with_acc,
    .cmp_imm_with_acc,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x40
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x48
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x50
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x58
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x60
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x68
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x70
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x78
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x80: needs opcode extension
    .Unknown, // <- 0x81: needs opcode extension
    .Unknown, // <- 0x82: needs opcode extension
    .Unknown, // <- 0x83: needs opcode extension
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .mov_rm_to_from_r, // <- 0x88
    .mov_rm_to_from_r,
    .mov_rm_to_from_r,
    .mov_rm_to_from_r,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x90
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0x98
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .mov_mem_to_accumulator, // <- 0xA0
    .mov_mem_to_accumulator,
    .mov_accumulator_to_mem,
    .mov_accumulator_to_mem,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0xA8
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .mov_imm_to_r, // <- 0xB0
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r, // <- 0xB8
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .mov_imm_to_r,
    .Unknown, // <- 0xC0
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .mov_imm_to_rm,
    .mov_imm_to_rm,
    .Unknown, // <- 0xC8
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0xD0
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0xD8
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0xE0
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0xE8
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0xF0
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown, // <- 0xF8
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
    .sub_imm_to_rm,
    .Unknown,
    .cmp_imm_with_rm,
};
const opcode_extension_table_0x81 = [_]Mnemonic{
    .add_imm_to_rm,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .sub_imm_to_rm,
    .Unknown,
    .cmp_imm_with_rm,
};
const opcode_extension_table_0x82 = [_]Mnemonic{
    .add_imm_to_rm,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .sub_imm_to_rm,
    .Unknown,
    .cmp_imm_with_rm,
};
const opcode_extension_table_0x83 = [_]Mnemonic{
    .add_imm_to_rm,
    .Unknown,
    .Unknown,
    .Unknown,
    .Unknown,
    .sub_imm_to_rm,
    .Unknown,
    .cmp_imm_with_rm,
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
        .{ .in = .{ 0x00, 0b0000000 }, .expected = .add_rm_with_r_to_either },
        .{ .in = .{ 0x01, 0b0000000 }, .expected = .add_rm_with_r_to_either },
        .{ .in = .{ 0x02, 0b0000000 }, .expected = .add_rm_with_r_to_either },
        .{ .in = .{ 0x03, 0b0000000 }, .expected = .add_rm_with_r_to_either },
        .{ .in = .{ 0x04, 0b0000000 }, .expected = .add_imm_to_acc },
        .{ .in = .{ 0x05, 0b0000000 }, .expected = .add_imm_to_acc },
        .{ .in = .{ 0x28, 0b0000000 }, .expected = .sub_rm_and_r_to_either },
        .{ .in = .{ 0x29, 0b0000000 }, .expected = .sub_rm_and_r_to_either },
        .{ .in = .{ 0x2A, 0b0000000 }, .expected = .sub_rm_and_r_to_either },
        .{ .in = .{ 0x2B, 0b0000000 }, .expected = .sub_rm_and_r_to_either },
        .{ .in = .{ 0x2C, 0b0000000 }, .expected = .sub_imm_to_acc },
        .{ .in = .{ 0x2D, 0b0000000 }, .expected = .sub_imm_to_acc },
        .{ .in = .{ 0x38, 0b0000000 }, .expected = .cmp_rm_with_r },
        .{ .in = .{ 0x39, 0b0000000 }, .expected = .cmp_rm_with_r },
        .{ .in = .{ 0x3A, 0b0000000 }, .expected = .cmp_rm_with_r },
        .{ .in = .{ 0x3B, 0b0000000 }, .expected = .cmp_rm_with_r },
        .{ .in = .{ 0x3C, 0b0000000 }, .expected = .cmp_imm_with_acc },
        .{ .in = .{ 0x3D, 0b0000000 }, .expected = .cmp_imm_with_acc },
        .{ .in = .{ 0x80, 0b0000000 }, .expected = .add_imm_to_rm },
        .{ .in = .{ 0x80, 0b0101000 }, .expected = .sub_imm_to_rm },
        .{ .in = .{ 0x80, 0b0111000 }, .expected = .cmp_imm_with_rm },
        .{ .in = .{ 0x81, 0b0000000 }, .expected = .add_imm_to_rm },
        .{ .in = .{ 0x81, 0b0101000 }, .expected = .sub_imm_to_rm },
        .{ .in = .{ 0x81, 0b0111000 }, .expected = .cmp_imm_with_rm },
        .{ .in = .{ 0x82, 0b0000000 }, .expected = .add_imm_to_rm },
        .{ .in = .{ 0x82, 0b0101000 }, .expected = .sub_imm_to_rm },
        .{ .in = .{ 0x82, 0b0111000 }, .expected = .cmp_imm_with_rm },
        .{ .in = .{ 0x83, 0b0000000 }, .expected = .add_imm_to_rm },
        .{ .in = .{ 0x83, 0b0101000 }, .expected = .sub_imm_to_rm },
        .{ .in = .{ 0x83, 0b0111000 }, .expected = .cmp_imm_with_rm },
        .{ .in = .{ 0x88, 0b0000000 }, .expected = .mov_rm_to_from_r },
        .{ .in = .{ 0x89, 0b0000000 }, .expected = .mov_rm_to_from_r },
        .{ .in = .{ 0x8A, 0b0000000 }, .expected = .mov_rm_to_from_r },
        .{ .in = .{ 0x8B, 0b0000000 }, .expected = .mov_rm_to_from_r },
        .{ .in = .{ 0x8C, 0b0000000 }, .expected = .Unknown },
        .{ .in = .{ 0x98, 0b0000000 }, .expected = .Unknown },
        .{ .in = .{ 0xA0, 0b0000000 }, .expected = .mov_mem_to_accumulator },
        .{ .in = .{ 0xA1, 0b0000000 }, .expected = .mov_mem_to_accumulator },
        .{ .in = .{ 0xA2, 0b0000000 }, .expected = .mov_accumulator_to_mem },
        .{ .in = .{ 0xA3, 0b0000000 }, .expected = .mov_accumulator_to_mem },
        .{ .in = .{ 0xB0, 0b0000000 }, .expected = .mov_imm_to_r },
        .{ .in = .{ 0xB8, 0b0000000 }, .expected = .mov_imm_to_r },
        .{ .in = .{ 0xC6, 0b0000000 }, .expected = .mov_imm_to_rm },
        .{ .in = .{ 0xC7, 0b0000000 }, .expected = .mov_imm_to_rm },
    };

    for (test_cases) |case| {
        const actual = decode(case.in[0], case.in[1]);
        if (actual != case.expected) {
            std.debug.print("0b{b}: want: {}, got: {}\n", .{ case.in, case.expected, actual });
            return error.TestExpectedEqual;
        }
    }
}
