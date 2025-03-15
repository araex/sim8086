const assert = @import("std").debug.assert;
const std = @import("std");

pub fn decodeOpcode(byte_1: u8, byte_2: u8) Opcode {
    if (hasOpcodeExtension(byte_1)) {
        return decodeWithExtensions(byte_1, byte_2);
    }
    return opcode_table[byte_1];
}

const opcode_mappings = [_]OpcodeEntry{
    // ADD instructions
    .{ .range = .{ .start = 0x00, .end = 0x03, .opcode = .add_rm_with_r_to_either } },
    .{ .range = .{ .start = 0x04, .end = 0x05, .opcode = .add_imm_to_acc } },

    // SUB instructions
    .{ .range = .{ .start = 0x28, .end = 0x2B, .opcode = .sub_rm_and_r_to_either } },
    .{ .range = .{ .start = 0x2C, .end = 0x2D, .opcode = .sub_imm_to_acc } },

    // CMP instructions
    .{ .range = .{ .start = 0x38, .end = 0x3B, .opcode = .cmp_rm_with_r } },
    .{ .range = .{ .start = 0x3C, .end = 0x3D, .opcode = .cmp_imm_with_acc } },

    // Jump instructions
    .{ .single = .{ .byte = 0x70, .opcode = .jo } },
    .{ .single = .{ .byte = 0x71, .opcode = .jno } },
    .{ .single = .{ .byte = 0x72, .opcode = .jb_jnae } },
    .{ .single = .{ .byte = 0x73, .opcode = .jnb_jae } },
    .{ .single = .{ .byte = 0x74, .opcode = .je_jz } },
    .{ .single = .{ .byte = 0x75, .opcode = .jne_jnz } },
    .{ .single = .{ .byte = 0x76, .opcode = .jbe_jna } },
    .{ .single = .{ .byte = 0x77, .opcode = .jnbe_ja } },
    .{ .single = .{ .byte = 0x78, .opcode = .js } },
    .{ .single = .{ .byte = 0x79, .opcode = .jns } },
    .{ .single = .{ .byte = 0x7A, .opcode = .jp_jpe } },
    .{ .single = .{ .byte = 0x7B, .opcode = .jnp_jpo } },
    .{ .single = .{ .byte = 0x7C, .opcode = .jl_jnge } },
    .{ .single = .{ .byte = 0x7D, .opcode = .jnl_jge } },
    .{ .single = .{ .byte = 0x7E, .opcode = .jle_jng } },
    .{ .single = .{ .byte = 0x7F, .opcode = .jnle_jg } },

    // MOV instructions
    .{ .range = .{ .start = 0x88, .end = 0x8B, .opcode = .mov_rm_to_from_r } },
    .{ .range = .{ .start = 0xA0, .end = 0xA1, .opcode = .mov_mem_to_accumulator } },
    .{ .range = .{ .start = 0xA2, .end = 0xA3, .opcode = .mov_accumulator_to_mem } },
    .{ .range = .{ .start = 0xB0, .end = 0xBF, .opcode = .mov_imm_to_r } },
    .{ .range = .{ .start = 0xC6, .end = 0xC7, .opcode = .mov_imm_to_rm } },

    // LOOP instructions
    .{ .single = .{ .byte = 0xE0, .opcode = .loopnz_loopne } },
    .{ .single = .{ .byte = 0xE1, .opcode = .loopz_loope } },
    .{ .single = .{ .byte = 0xE2, .opcode = .loop } },
    .{ .single = .{ .byte = 0xE3, .opcode = .jcxz } },
};

// Generate the lookup table at comptime
const opcode_table = blk: {
    var table: [256]Opcode = [_]Opcode{.Unknown} ** 256;

    for (opcode_mappings) |mapping| {
        switch (mapping) {
            .single => |s| {
                table[s.byte] = s.opcode;
            },
            .range => |r| {
                var i = r.start;
                while (i <= r.end) : (i += 1) {
                    table[i] = r.opcode;
                }
            },
        }
    }

    break :blk table;
};

const extension_bytes = [_]u8{ 0x80, 0x81, 0x82, 0x83 };

const opcode_extensions = [_]OpcodeExtension{
    .{ .reg_bits = 0, .opcode = .add_imm_to_rm },
    .{ .reg_bits = 5, .opcode = .sub_imm_to_rm },
    .{ .reg_bits = 7, .opcode = .cmp_imm_with_rm },
};

fn hasOpcodeExtension(byte: u8) bool {
    for (extension_bytes) |ext_byte| {
        if (byte == ext_byte) return true;
    }
    return false;
}

fn decodeWithExtensions(byte_1: u8, byte_2: u8) Opcode {
    assert(hasOpcodeExtension(byte_1));
    const reg_bits: u3 = @intCast((byte_2 & 0b00111000) >> 3);

    // Find the matching extension
    for (opcode_extensions) |ext| {
        if (ext.reg_bits == reg_bits) {
            return ext.opcode;
        }
    }

    return .Unknown;
}

const OpcodeRange = struct {
    start: u8,
    end: u8,
    opcode: Opcode,
};

const OpcodeEntry = union(enum) {
    single: struct { byte: u8, opcode: Opcode },
    range: OpcodeRange,
};

// Extension opcodes - table for opcodes that need extension
const OpcodeExtension = struct {
    reg_bits: u3,
    opcode: Opcode,
};

pub const Opcode = enum {
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
    jo,
    jno,
    jb_jnae,
    jnb_jae,
    je_jz,
    jne_jnz,
    jbe_jna,
    jnbe_ja,
    js,
    jns,
    jp_jpe,
    jnp_jpo,
    jl_jnge,
    jnl_jge,
    jle_jng,
    jnle_jg,
    loopnz_loopne,
    loopz_loope,
    loop,
    jcxz,
};

test "decode x86_opcode" {
    const TestCase = struct {
        in: [2]u8,
        expected: Opcode,
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
        .{ .in = .{ 0x70, 0b00000000 }, .expected = .jo },
        .{ .in = .{ 0x71, 0b00000000 }, .expected = .jno },
        .{ .in = .{ 0x72, 0b00000000 }, .expected = .jb_jnae },
        .{ .in = .{ 0x73, 0b00000000 }, .expected = .jnb_jae },
        .{ .in = .{ 0x74, 0b00000000 }, .expected = .je_jz },
        .{ .in = .{ 0x75, 0b00000000 }, .expected = .jne_jnz },
        .{ .in = .{ 0x76, 0b00000000 }, .expected = .jbe_jna },
        .{ .in = .{ 0x77, 0b00000000 }, .expected = .jnbe_ja },
        .{ .in = .{ 0x78, 0b00000000 }, .expected = .js },
        .{ .in = .{ 0x79, 0b00000000 }, .expected = .jns },
        .{ .in = .{ 0x7A, 0b00000000 }, .expected = .jp_jpe },
        .{ .in = .{ 0x7B, 0b00000000 }, .expected = .jnp_jpo },
        .{ .in = .{ 0x7C, 0b00000000 }, .expected = .jl_jnge },
        .{ .in = .{ 0x7D, 0b00000000 }, .expected = .jnl_jge },
        .{ .in = .{ 0x7E, 0b00000000 }, .expected = .jle_jng },
        .{ .in = .{ 0x7F, 0b00000000 }, .expected = .jnle_jg },
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
        .{ .in = .{ 0xE0, 0b0000000 }, .expected = .loopnz_loopne },
        .{ .in = .{ 0xE1, 0b0000000 }, .expected = .loopz_loope },
        .{ .in = .{ 0xE2, 0b0000000 }, .expected = .loop },
        .{ .in = .{ 0xE3, 0b0000000 }, .expected = .jcxz },
    };

    for (test_cases) |case| {
        const actual = decodeOpcode(case.in[0], case.in[1]);
        if (actual != case.expected) {
            std.log.err("0b{b}: want: {}, got: {}\n", .{ case.in, case.expected, actual });
            return error.TestExpectedEqual;
        }
    }
}
