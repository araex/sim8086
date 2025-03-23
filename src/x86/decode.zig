const std = @import("std");

const AddressingMode = @import("instruction.zig").AddressingMode;
const Direction = @import("instruction.zig").Direction;
const Displacement = @import("memory.zig").Displacement;
const EffectiveAddressCalculation = @import("memory.zig").EffectiveAddressCalculation;
const ImmediateOperand = @import("instruction.zig").ImmediateOperand;
const Instruction = @import("instruction.zig").Instruction;
const JumpDestination = @import("instruction.zig").JumpDestination;
const makeDst = @import("instruction.zig").makeDst;
const makeImmediate = @import("instruction.zig").makeImmediate;
const makeInstruction = @import("instruction.zig").makeInstruction;
const makeSrc = @import("instruction.zig").makeSrc;
const MemoryOperand = @import("memory.zig").MemoryOperand;
const Operation = @import("instruction.zig").Operation;
const RegisterName = @import("register.zig").RegisterName;
const RegOrMem = @import("instruction.zig").RegOrMem;
const Width = @import("instruction.zig").Width;

const native_endian = @import("builtin").target.cpu.arch.endian();

pub fn instructionList(alloc: std.mem.Allocator, bin: []const u8) !std.ArrayList(Instruction) {
    const fmt = @import("format.zig").fmt;

    var result = std.ArrayList(Instruction).init(alloc);
    errdefer result.deinit();
    var stream = std.io.fixedBufferStream(bin);
    var reader = stream.reader().any();
    while (true) {
        const decoded = instruction(&reader) catch |err| switch (err) {
            error.EndOfStream => break,
            error.UnknownInstruction => {
                std.log.err("Got unknown instruction. Parsed {d} sucessfully:\n", .{result.items.len});
                for (result.items) |item| {
                    std.log.err("{}", .{fmt(item)});
                }
                return error.UnknownInstruction;
            },
            else => return err,
        };
        try result.append(decoded);
    }

    return result;
}

pub fn instruction(reader: *std.io.AnyReader) !Instruction {
    const byte_1 = try reader.readByte();
    const byte_2 = try reader.readByte();
    const op = decodeOpcode(byte_1, byte_2);

    return switch (op) {
        .add_rm_with_r_to_either,
        .sub_rm_and_r_to_either,
        .cmp_rm_with_r,
        .mov_rm_to_from_r,
        => try decodeRegMemToFromReg(op, byte_1, byte_2, reader),
        .mov_sr_to_rm,
        .mov_rm_to_sr,
        => try decodeMovSegmentRegister(op, byte_1, byte_2, reader),
        .mov_imm_to_r,
        => try decodeImmediateToRegister(op, byte_1, byte_2, reader),
        .mov_imm_to_rm,
        => try decodeImmediateToRegMem(op, byte_1, byte_2, reader),
        .mov_mem_to_accumulator,
        .mov_accumulator_to_mem,
        => try decodeMemToFromAccumulator(op, byte_1, byte_2, reader),
        .add_imm_to_rm,
        .sub_imm_to_rm,
        .cmp_imm_with_rm,
        => try decodeImmediateToRegMemWithExtensionInstruction(op, byte_1, byte_2, reader),
        .add_imm_to_acc,
        .sub_imm_to_acc,
        .cmp_imm_with_acc,
        => try decodeImmediateToAccumulatorInstruction(op, byte_1, byte_2, reader),
        .jo,
        .jno,
        .jb_jnae,
        .jnb_jae,
        .je_jz,
        .jne_jnz,
        .jbe_jna,
        .jnbe_ja,
        .js,
        .jns,
        .jp_jpe,
        .jnp_jpo,
        .jl_jnge,
        .jnl_jge,
        .jle_jng,
        .jnle_jg,
        .loopnz_loopne,
        .loopz_loope,
        .loop,
        .jcxz,
        => decodeJumpInstruction(op, byte_2),
        else => {
            std.log.err("Unknown opcode: {x} {x}\n", .{ byte_1, byte_2 });
            return error.UnknownInstruction;
        },
    };
}

fn decodeRegMemToFromReg(op: Operation, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP D W | MOD REG R/M | [DISP] | D=direction, W=width
    const width = decodeOperatesOn(0b00000001, byte_1);
    const dir = decodeDirection(0b00000010, byte_1);
    const mode = decodeMode(0b11000000, byte_2);
    const reg = decodeRegister(width, 0b00111000, byte_2);
    const rm = try decodeRM(mode, width, 0b00000111, byte_2, reader);

    return switch (dir) {
        Direction.FromRegister => makeInstruction(op, width, rm, reg),
        Direction.ToRegister => makeInstruction(op, width, reg, rm),
    };
}

fn decodeMovSegmentRegister(op: Operation, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    _ = byte_1;
    // Format: | OP | MOD 0b0 SR R/M | [DISP] | SR=Segment Register
    const mode = decodeMode(0b11000000, byte_2);
    const sr = decodeSegmentRegister(0b00011000, byte_2);
    const rm = try decodeRM(mode, .Word, 0b00000111, byte_2, reader);
    return switch (op) {
        .mov_sr_to_rm => return makeInstruction(op, .Word, rm, sr),
        .mov_rm_to_sr => return makeInstruction(op, .Word, sr, rm),
        else => unreachable,
    };
}

fn decodeImmediateToRegister(op: Operation, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP + reg | IMM | W encoded in opcode
    const width = decodeOperatesOn(0b00001000, byte_1);
    const reg = decodeRegister(width, 0b00000111, byte_1);
    const immediate = try decodeImmediate(width, byte_2, reader);

    return makeInstruction(op, width, reg, immediate);
}

fn decodeImmediateToRegMem(op: Operation, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP W | MOD 000 R/M | [DISP] | IMM | W=width
    const width = decodeOperatesOn(0b00000001, byte_1);
    const mode = decodeMode(0b11000000, byte_2);
    std.debug.assert(mode != AddressingMode.Reg);
    const rm = try decodeRM(mode, width, 0b00000111, byte_2, reader);

    const immediate = try decodeImmediate(width, try reader.readByte(), reader);
    return makeInstruction(op, width, rm, immediate);
}

fn decodeMemToFromAccumulator(op: Operation, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP W | ADDR-LO | [ADDR-HI] | W=width
    const width = decodeOperatesOn(0b00000001, byte_1);
    const reg = if (width == Width.Byte) RegisterName.AL else RegisterName.AX;

    // Read address (displacement)
    const displacement = if (width == Width.Byte)
        Displacement{ .byte = byte_2 }
    else
        Displacement{ .word = @as(u16, byte_2) | (@as(u16, try reader.readByte()) << 8) };

    const mem = MemoryOperand{
        .calc = EffectiveAddressCalculation.DIRECT_ADDRESS,
        .displacement = displacement,
    };

    // Direction is encoded in the opcode itself
    return switch (op) {
        .mov_accumulator_to_mem => makeInstruction(op, width, makeDst(mem), reg),
        .mov_mem_to_accumulator => makeInstruction(op, width, reg, makeSrc(mem)),
        else => unreachable,
    };
}

fn decodeImmediateToRegMemWithExtensionInstruction(op: Operation, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP S W | MOD 000 R/M | [DISP] | IMM | S=sign ext, W=width
    const width = decodeOperatesOn(0b00000001, byte_1);
    const sign_extension = (byte_1 & 0b00000010) != 0;
    const mode = decodeMode(0b11000000, byte_2);
    const rm = try decodeRM(mode, width, 0b00000111, byte_2, reader);

    // Handle sign extension cases
    const immediate = if (sign_extension)
        try decodeImmediateWithSignExtension(try reader.readByte())
    else
        try decodeImmediate(width, try reader.readByte(), reader);

    return makeInstruction(op, width, rm, immediate);
}

fn decodeImmediateToAccumulatorInstruction(op: Operation, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP W | IMM | W=width
    const width = decodeOperatesOn(0b00000001, byte_1);
    const reg = if (width == Width.Byte) RegisterName.AL else RegisterName.AX;
    const immediate = try decodeImmediate(width, byte_2, reader);

    return makeInstruction(op, width, reg, immediate);
}

fn decodeJumpInstruction(op: Operation, byte_2: u8) Instruction {
    // Format: | OP | DISP | 8-bit signed displacement
    // No need for pattern here as it's very simple
    const jump = decodeJumpDestination(byte_2);
    return makeInstruction(op, .Byte, jump, null);
}

fn decodeOpcode(byte_1: u8, byte_2: u8) Operation {
    if (hasOpcodeExtension(byte_1)) {
        return decodeWithExtensions(byte_1, byte_2);
    }
    return opcode_table[byte_1];
}

fn decodeDisplacement(mode: AddressingMode, calc: EffectiveAddressCalculation, reader: *std.io.AnyReader) !?Displacement {
    switch (mode) {
        .Reg => return null,
        .Mem => {
            if (calc != EffectiveAddressCalculation.DIRECT_ADDRESS) {
                return null;
            }
            const val = try reader.readInt(u16, native_endian);
            return Displacement{
                .word = val,
            };
        },
        .Mem8BitDisplacement => {
            const val = try reader.readByte();
            return Displacement{
                .byte = val,
            };
        },
        .Mem16BitDisplacement => {
            const val = try reader.readInt(u16, native_endian);
            return Displacement{
                .word = val,
            };
        },
    }
    unreachable;
}

fn decodeOperatesOn(comptime mask: u8, byte: u8) Width {
    // Table 4-7: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n257/mode/1up
    if ((byte & mask) == 0) {
        return Width.Byte;
    }
    return Width.Word;
}

fn decodeDirection(comptime mask: u8, byte: u8) Direction {
    // Table 4-7: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n257/mode/1up
    if ((byte & mask) == 0) {
        return Direction.FromRegister;
    }
    return Direction.ToRegister;
}

fn decodeMode(comptime mask_mode_bits: u8, byte: u8) AddressingMode {
    // Table 4-8: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n258/mode/1up
    const num_bits = 2;
    comptime std.debug.assert(isValidMask(mask_mode_bits, num_bits));
    const required_shift: u3 = @intCast(@bitSizeOf(u8) - num_bits - @clz(mask_mode_bits));
    const mode_bits: u3 = @intCast((byte & mask_mode_bits) >> required_shift);
    return @enumFromInt(mode_bits);
}

fn decodeWithExtensions(byte_1: u8, byte_2: u8) Operation {
    std.debug.assert(hasOpcodeExtension(byte_1));
    const reg_bits: u3 = @intCast((byte_2 & 0b00111000) >> 3);

    // Find the matching extension
    for (opcode_extensions) |ext| {
        if (ext.reg_bits == reg_bits) {
            return ext.opcode;
        }
    }

    return .Unknown;
}

fn decodeJumpDestination(byte: u8) JumpDestination {
    return JumpDestination{
        .increment = @bitCast(byte),
    };
}

// Table 4-9: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n258/mode/1up
fn decodeRegister(width: Width, comptime mask_reg_bits: u8, byte: u8) RegisterName {
    const num_bits = 3;
    comptime std.debug.assert(isValidMask(mask_reg_bits, num_bits));
    const required_shift: u3 = @intCast(@bitSizeOf(u8) - num_bits - @clz(mask_reg_bits));
    const reg_bits: u3 = @intCast((byte & mask_reg_bits) >> required_shift);
    switch (width) {
        Width.Byte => return @enumFromInt(reg_bits),
        Width.Word => return @enumFromInt(@as(u8, reg_bits) + 8),
    }
    unreachable;
}

// Table 4-11: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n258/mode/1up
fn decodeSegmentRegister(comptime mask_reg_bits: u8, byte: u8) RegisterName {
    const num_bits = 2;
    comptime std.debug.assert(isValidMask(mask_reg_bits, num_bits));
    const required_shift: u2 = @intCast(@bitSizeOf(u8) - num_bits - @clz(mask_reg_bits));
    const reg_bits: u2 = @intCast((byte & mask_reg_bits) >> required_shift);
    switch (reg_bits) {
        0b00 => return RegisterName.ES,
        0b01 => return RegisterName.CS,
        0b10 => return RegisterName.SS,
        0b11 => return RegisterName.DS,
    }
}

fn decodeEffectiveAddressCalcuation(mode: AddressingMode, comptime mask_rm_bits: u8, byte: u8) EffectiveAddressCalculation {
    // Table 4-10: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n258/mode/1up
    const num_bits = 3;
    comptime std.debug.assert(isValidMask(mask_rm_bits, num_bits));
    const required_shift: u3 = @intCast(@bitSizeOf(u8) - num_bits - @clz(mask_rm_bits));
    const reg_bits: u3 = @intCast((byte & mask_rm_bits) >> required_shift);

    if (reg_bits == 0b000) return EffectiveAddressCalculation.BX_PLUS_SI;
    if (reg_bits == 0b001) return EffectiveAddressCalculation.BX_PLUS_DI;
    if (reg_bits == 0b010) return EffectiveAddressCalculation.BP_PLUS_SI;
    if (reg_bits == 0b011) return EffectiveAddressCalculation.BP_PLUS_DI;
    if (reg_bits == 0b100) return EffectiveAddressCalculation.SI;
    if (reg_bits == 0b101) return EffectiveAddressCalculation.DI;
    if (reg_bits == 0b110) {
        if (mode == AddressingMode.Mem) {
            return EffectiveAddressCalculation.DIRECT_ADDRESS;
        }
        return EffectiveAddressCalculation.BP;
    }
    if (reg_bits == 0b111) return EffectiveAddressCalculation.BX;
    unreachable;
}

fn decodeRM(mode: AddressingMode, width: Width, comptime mask_rm_bits: u8, value_to_decode: u8, reader: *std.io.AnyReader) !RegOrMem {
    // Table 4-10: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    switch (mode) {
        AddressingMode.Reg => {
            return RegOrMem{
                .register = decodeRegister(width, mask_rm_bits, value_to_decode),
            };
        },
        else => {
            const calc = decodeEffectiveAddressCalcuation(mode, mask_rm_bits, value_to_decode);
            const displacement = try decodeDisplacement(mode, calc, reader);
            return RegOrMem{
                .memory = MemoryOperand{
                    .calc = calc,
                    .displacement = displacement,
                },
            };
        },
    }
    unreachable;
}

fn decodeImmediate(width: Width, byte: u8, reader: anytype) !ImmediateOperand {
    switch (width) {
        Width.Byte => return makeImmediate(byte),
        Width.Word => {
            const second_byte = try reader.readByte();
            const value: u16 = @as(u16, byte) | (@as(u16, second_byte) << 8);
            return makeImmediate(value);
        },
    }
    unreachable;
}

fn decodeImmediateWithSignExtension(byte: u8) !ImmediateOperand {
    const signed_byte: i8 = @bitCast(byte);
    const signed_word: i16 = signed_byte;
    const unsigned_word: u16 = @bitCast(signed_word);
    return makeImmediate(unsigned_word);
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
    .{ .single = .{ .byte = 0x8C, .opcode = .mov_sr_to_rm } },
    .{ .single = .{ .byte = 0x8E, .opcode = .mov_rm_to_sr } },
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
    var table: [256]Operation = [_]Operation{.Unknown} ** 256;

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

const OpcodeRange = struct {
    start: u8,
    end: u8,
    opcode: Operation,
};

const OpcodeEntry = union(enum) {
    single: struct { byte: u8, opcode: Operation },
    range: OpcodeRange,
};

const OpcodeExtension = struct {
    reg_bits: u3,
    opcode: Operation,
};

test "decodeRegister" {
    const TestCase = struct {
        in: u3,
        width: Width,
        expected: RegisterName,
    };
    const test_cases = [_]TestCase{
        // Byte
        .{ .in = 0b000, .width = Width.Byte, .expected = .AL },
        .{ .in = 0b001, .width = Width.Byte, .expected = .CL },
        .{ .in = 0b010, .width = Width.Byte, .expected = .DL },
        .{ .in = 0b011, .width = Width.Byte, .expected = .BL },
        .{ .in = 0b100, .width = Width.Byte, .expected = .AH },
        .{ .in = 0b101, .width = Width.Byte, .expected = .CH },
        .{ .in = 0b110, .width = Width.Byte, .expected = .DH },
        .{ .in = 0b111, .width = Width.Byte, .expected = .BH },
        // Word
        .{ .in = 0b000, .width = Width.Word, .expected = .AX },
        .{ .in = 0b001, .width = Width.Word, .expected = .CX },
        .{ .in = 0b010, .width = Width.Word, .expected = .DX },
        .{ .in = 0b011, .width = Width.Word, .expected = .BX },
        .{ .in = 0b100, .width = Width.Word, .expected = .SP },
        .{ .in = 0b101, .width = Width.Word, .expected = .BP },
        .{ .in = 0b110, .width = Width.Word, .expected = .SI },
        .{ .in = 0b111, .width = Width.Word, .expected = .DI },
    };

    for (test_cases) |case| {
        const actual = decodeRegister(case.width, 0b00000111, case.in);
        if (actual != case.expected) {
            std.log.err("0b{b}: want: {}, got: {}\n", .{ case.in, case.expected, actual });
            return error.TestExpectedEqual;
        }
    }
}

test "decode x86_opcode" {
    const TestCase = struct {
        in: [2]u8,
        expected: Operation,
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
        .{ .in = .{ 0x8C, 0b0000000 }, .expected = .mov_sr_to_rm },
        .{ .in = .{ 0x8E, 0b0000000 }, .expected = .mov_rm_to_sr },
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

test "decodeDisplacement" {
    const TestCase = struct {
        name: [:0]const u8,
        mode: AddressingMode,
        calc: EffectiveAddressCalculation,
        disp_buf: []const u8,
        expected: ?Displacement,
    };
    const test_cases = [_]TestCase{
        .{
            .name = "AddressingMode.Reg has no displacment",
            .mode = AddressingMode.Reg,
            .calc = EffectiveAddressCalculation.BP, // doesn't matter
            .disp_buf = &[_]u8{},
            .expected = null,
        },
        .{
            .name = "AddressingMode.Mem decode DIRECT ADDRESS",
            .mode = AddressingMode.Mem,
            .calc = EffectiveAddressCalculation.DIRECT_ADDRESS,
            .disp_buf = &[_]u8{ 0x0, 0x1 },
            .expected = Displacement{
                .word = 256,
            },
        },
        .{
            .name = "AddressingMode.Mem8BitDisplacement decode 8 bits",
            .mode = AddressingMode.Mem8BitDisplacement,
            .calc = EffectiveAddressCalculation.BP, // doesn't matter
            .disp_buf = &[_]u8{42},
            .expected = Displacement{
                .byte = 42,
            },
        },
        .{
            .name = "AddressingMode.Mem16BitDisplacement decode 16 bits",
            .mode = AddressingMode.Mem16BitDisplacement,
            .calc = EffectiveAddressCalculation.BP, // doesn't matter
            .disp_buf = &[_]u8{ 0x0, 0x1 },
            .expected = Displacement{
                .word = 256,
            },
        },
    };

    for (test_cases) |case| {
        var stream = std.io.fixedBufferStream(case.disp_buf);
        var reader = stream.reader().any();
        const actual = decodeDisplacement(case.mode, case.calc, &reader) catch |actual_error| {
            std.log.err("Unexpected error: {}", .{actual_error});
            return error.TestUnexpectedResult;
        };
        std.testing.expectEqual(case.expected, actual) catch |err| {
            std.log.err("Test failure: {s}\n", .{case.name});
            return err;
        };
    }
}

fn countConsecutiveBits(comptime mask: u8) u8 {
    return @clz(mask) + @ctz(mask);
}

fn isValidMask(comptime mask: u8, comptime expected_set_bit_count: u8) bool {
    return (@popCount(mask) == expected_set_bit_count) and (countConsecutiveBits(mask) == (8 - expected_set_bit_count));
}
