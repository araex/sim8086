const std = @import("std");
const assert = @import("std").debug.assert;
const native_endian = @import("builtin").target.cpu.arch.endian();
const opcode = @import("opcode.zig");

const OperatesOn = enum {
    Byte,
    Word,
};
fn decodeOperatesOn(comptime mask: u8, byte: u8) OperatesOn {
    // Table 4-7: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n257/mode/1up
    if ((byte & mask) == 0) {
        return OperatesOn.Byte;
    }
    return OperatesOn.Word;
}

const Direction = enum {
    ToRegister, // Instruction destination is specified in REG field
    FromRegister, // Instruction source is specified in REG field
};
fn decodeDirection(comptime mask: u8, byte: u8) Direction {
    // Table 4-7: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n257/mode/1up
    if ((byte & mask) == 0) {
        return Direction.FromRegister;
    }
    return Direction.ToRegister;
}

// Table 4-8: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n258/mode/1up
const Mode = enum(u3) {
    Mem,
    Mem8BitDisplacement,
    Mem16BitDisplacement,
    Reg,
};
fn decodeMode(comptime mask_mode_bits: u8, byte: u8) Mode {
    const num_bits = 2;
    comptime assert(isValidMask(mask_mode_bits, num_bits));
    const required_shift: u3 = @intCast(@bitSizeOf(u8) - num_bits - @clz(mask_mode_bits));
    const mode_bits: u3 = @intCast((byte & mask_mode_bits) >> required_shift);
    return @enumFromInt(mode_bits);
}

// Table 4-9: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n258/mode/1up
pub const Register = enum { AL, CL, DL, BL, AH, CH, DH, BH, AX, CX, DX, BX, SP, BP, SI, DI };
fn decodeRegister(operates_on: OperatesOn, comptime mask_reg_bits: u8, byte: u8) Register {
    const num_bits = 3;
    comptime assert(isValidMask(mask_reg_bits, num_bits));
    const required_shift: u3 = @intCast(@bitSizeOf(u8) - num_bits - @clz(mask_reg_bits));
    const reg_bits: u3 = @intCast((byte & mask_reg_bits) >> required_shift);
    switch (operates_on) {
        OperatesOn.Byte => return @enumFromInt(reg_bits),
        OperatesOn.Word => return @enumFromInt(@as(u8, reg_bits) + 8),
    }
    unreachable;
}

pub const EffectiveAddressCalculation = enum {
    BX_PLUS_SI,
    BX_PLUS_DI,
    BP_PLUS_SI,
    BP_PLUS_DI,
    SI,
    DI,
    BP,
    DIRECT_ADDRESS,
    BX,
};
fn decodeEffectiveAddressCalcuation(mode: Mode, comptime mask_rm_bits: u8, byte: u8) EffectiveAddressCalculation {
    // Table 4-10: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n258/mode/1up
    const num_bits = 3;
    comptime assert(isValidMask(mask_rm_bits, num_bits));
    const required_shift: u3 = @intCast(@bitSizeOf(u8) - num_bits - @clz(mask_rm_bits));
    const reg_bits: u3 = @intCast((byte & mask_rm_bits) >> required_shift);

    if (reg_bits == 0b000) return EffectiveAddressCalculation.BX_PLUS_SI;
    if (reg_bits == 0b001) return EffectiveAddressCalculation.BX_PLUS_DI;
    if (reg_bits == 0b010) return EffectiveAddressCalculation.BP_PLUS_SI;
    if (reg_bits == 0b011) return EffectiveAddressCalculation.BP_PLUS_DI;
    if (reg_bits == 0b100) return EffectiveAddressCalculation.SI;
    if (reg_bits == 0b101) return EffectiveAddressCalculation.DI;
    if (reg_bits == 0b110) {
        if (mode == Mode.Mem) {
            return EffectiveAddressCalculation.DIRECT_ADDRESS;
        }
        return EffectiveAddressCalculation.BP;
    }
    if (reg_bits == 0b111) return EffectiveAddressCalculation.BX;
    unreachable;
}

const DisplacementTagType = enum {
    byte,
    word,
};
const Displacement = union(DisplacementTagType) {
    byte: u8,
    word: u16,
};
fn decodeDisplacement(mode: Mode, calc: EffectiveAddressCalculation, reader: *std.io.AnyReader) !?Displacement {
    switch (mode) {
        Mode.Reg => return null,
        Mode.Mem => {
            if (calc != EffectiveAddressCalculation.DIRECT_ADDRESS) {
                return null;
            }
            const val = try reader.readInt(u16, native_endian);
            return Displacement{
                .word = val,
            };
        },
        Mode.Mem8BitDisplacement => {
            const val = try reader.readByte();
            return Displacement{
                .byte = val,
            };
        },
        Mode.Mem16BitDisplacement => {
            const val = try reader.readInt(u16, native_endian);
            return Displacement{
                .word = val,
            };
        },
    }
    unreachable;
}

pub const Memory = struct {
    calc: EffectiveAddressCalculation,
    displacement: ?Displacement,
};

const RegMemFieldTag = enum {
    register,
    memory,
};

const RegMemField = union(RegMemFieldTag) {
    register: Register,
    memory: Memory,
};

fn decodeRM(mode: Mode, operates_on: OperatesOn, comptime mask_rm_bits: u8, value_to_decode: u8, reader: *std.io.AnyReader) !RegMemField {
    // Table 4-10: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    switch (mode) {
        Mode.Reg => {
            return RegMemField{
                .register = decodeRegister(operates_on, mask_rm_bits, value_to_decode),
            };
        },
        else => {
            const calc = decodeEffectiveAddressCalcuation(mode, mask_rm_bits, value_to_decode);
            const displacement = try decodeDisplacement(mode, calc, reader);
            return RegMemField{
                .memory = Memory{
                    .calc = calc,
                    .displacement = displacement,
                },
            };
        },
    }
    unreachable;
}

const ImmediateValueTag = enum {
    byte,
    word,
};

pub const ImmediateValue = union(ImmediateValueTag) {
    byte: u8,
    word: u16,
};

pub const ImmediateField = struct {
    value: ImmediateValue,
    explicit_size: bool,
};

fn decodeImmediate(operates_on: OperatesOn, reader: anytype, explicit_size: bool) !ImmediateField {
    switch (operates_on) {
        OperatesOn.Byte => return ImmediateField{
            .value = ImmediateValue{
                .byte = try reader.readByte(),
            },
            .explicit_size = explicit_size,
        },
        OperatesOn.Word => return ImmediateField{
            .value = ImmediateValue{
                .word = try reader.readInt(u16, native_endian),
            },
            .explicit_size = explicit_size,
        },
    }
    unreachable;
}

const SrcTypeTag = enum {
    register,
    immediate,
    memory,
};

const DstTypeTag = enum {
    register,
    memory,
};

pub const SrcType = union(SrcTypeTag) {
    register: Register,
    immediate: ImmediateField,
    memory: Memory,
};

fn makeSrc(val: anytype) SrcType {
    switch (@TypeOf(val)) {
        SrcType => return val,
        Register => {
            return SrcType{
                .register = val,
            };
        },
        Memory => {
            return SrcType{
                .memory = val,
            };
        },
        RegMemField => {
            switch (val) {
                .register => |r| return makeSrc(r),
                .memory => |m| return makeSrc(m),
            }
        },
        ImmediateField => {
            return SrcType{
                .immediate = val,
            };
        },
        else => unreachable,
    }
}

pub const DstType = union(DstTypeTag) {
    register: Register,
    memory: Memory,
};

fn makeDst(val: anytype) DstType {
    switch (@TypeOf(val)) {
        DstType => return val,
        Register => {
            return DstType{
                .register = val,
            };
        },
        Memory => {
            return DstType{
                .memory = val,
            };
        },
        RegMemField => {
            switch (val) {
                .register => |r| return makeDst(r),
                .memory => |m| return makeDst(m),
            }
        },
        else => unreachable,
    }
}

pub const Instruction = struct {
    op: opcode.Mnemonic,
    src: SrcType,
    dst: DstType,
};

fn makeInstruction(op: opcode.Mnemonic, dst: anytype, src: anytype) Instruction {
    return Instruction{
        .op = op,
        .dst = makeDst(dst),
        .src = makeSrc(src),
    };
}

fn decodeInstruction(reader: *std.io.AnyReader) !Instruction {
    const byte_1 = try reader.readByte();
    const op = opcode.decode(byte_1);

    switch (op) {
        opcode.Mnemonic.mov_rm_to_from_r => {
            const dir = decodeDirection(0b00000010, byte_1);
            const operates_on = decodeOperatesOn(0b00000001, byte_1);
            const byte_2 = try reader.readByte();
            const mode = decodeMode(0b11000000, byte_2);
            const reg = decodeRegister(operates_on, 0b00111000, byte_2);
            const rm = try decodeRM(mode, operates_on, 0b00000111, byte_2, reader);
            switch (dir) {
                Direction.FromRegister => return makeInstruction(op, rm, reg),
                Direction.ToRegister => return makeInstruction(op, reg, rm),
            }
            unreachable;
        },
        opcode.Mnemonic.mov_imm_to_r => {
            const operates_on = decodeOperatesOn(0b00001000, byte_1);
            const reg = decodeRegister(operates_on, 0b00000111, byte_1);
            const immediate = try decodeImmediate(operates_on, reader, false);
            return makeInstruction(op, reg, immediate);
        },
        opcode.Mnemonic.mov_imm_to_rm => {
            const operates_on = decodeOperatesOn(0b00000001, byte_1);
            const byte_2 = try reader.readByte();
            const mode = decodeMode(0b11000000, byte_2);
            assert(mode != Mode.Reg);
            const rm = try decodeRM(mode, operates_on, 0b00000111, byte_2, reader);
            if (mode == Mode.Mem8BitDisplacement) {
                // DISP is always 16bit before data begins
                _ = try reader.readByte();
            }
            const immediate = try decodeImmediate(operates_on, reader, true);
            return makeInstruction(op, rm, immediate);
        },
        opcode.Mnemonic.mov_accumulator_to_mem => {
            const operates_on = decodeOperatesOn(0b00000001, byte_1);
            const src = SrcType{
                .register = if (operates_on == OperatesOn.Byte) .AL else .AX,
            };
            var displacement: Displacement = undefined;
            if (operates_on == OperatesOn.Byte) {
                displacement = Displacement{
                    .byte = try reader.readByte(),
                };
                // Fixed length instruction, need to skip a byte
                _ = try reader.readByte();
            } else {
                displacement = Displacement{
                    .word = try reader.readInt(u16, native_endian),
                };
            }
            const dst = DstType{
                .memory = Memory{
                    .calc = EffectiveAddressCalculation.DIRECT_ADDRESS,
                    .displacement = displacement,
                },
            };
            return Instruction{
                .op = op,
                .src = src,
                .dst = dst,
            };
        },
        opcode.Mnemonic.mov_mem_to_accumulator => {
            const operates_on = decodeOperatesOn(0b00000001, byte_1);
            const dst_reg = if (operates_on == OperatesOn.Byte) Register.AL else Register.AX;
            const displacement = blk: {
                if (operates_on == OperatesOn.Byte) {
                    const val = try reader.readByte();
                    // Fixed length instruction, need to skip a byte
                    _ = try reader.readByte();
                    break :blk Displacement{
                        .byte = val,
                    };
                } else {
                    break :blk Displacement{
                        .word = try reader.readInt(u16, native_endian),
                    };
                }
            };
            return makeInstruction(op, dst_reg, makeSrc(Memory{
                .calc = EffectiveAddressCalculation.DIRECT_ADDRESS,
                .displacement = displacement,
            }));
        },
        opcode.Mnemonic.Unknown => {
            return error.UnknownInstruction;
        },
    }
    unreachable;
}

pub fn decode(alloc: std.mem.Allocator, bin: []const u8) !std.ArrayList(Instruction) {
    var result = std.ArrayList(Instruction).init(alloc);
    var stream = std.io.fixedBufferStream(bin);
    var reader = stream.reader().any();
    while (true) {
        const instruction = decodeInstruction(&reader) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        try result.append(instruction);
    }

    return result;
}

fn countConsecutiveBits(comptime mask: u8) u8 {
    return @clz(mask) + @ctz(mask);
}

fn isValidMask(comptime mask: u8, comptime expected_set_bit_count: u8) bool {
    return (@popCount(mask) == expected_set_bit_count) and (countConsecutiveBits(mask) == (8 - expected_set_bit_count));
}

//   _____           _
//  |_   _|___  ___ | |_  ___
//    | | / _ \/ __|| __|/ __|
//    | ||  __/\__ \| |_ \__ \
//    |_| \___||___/ \__||___/

test "decodeRegister" {
    const TestCase = struct {
        in: u3,
        operates_on: OperatesOn,
        expected: Register,
    };
    const test_cases = [_]TestCase{
        // Byte
        .{ .in = 0b000, .operates_on = OperatesOn.Byte, .expected = .AL },
        .{ .in = 0b001, .operates_on = OperatesOn.Byte, .expected = .CL },
        .{ .in = 0b010, .operates_on = OperatesOn.Byte, .expected = .DL },
        .{ .in = 0b011, .operates_on = OperatesOn.Byte, .expected = .BL },
        .{ .in = 0b100, .operates_on = OperatesOn.Byte, .expected = .AH },
        .{ .in = 0b101, .operates_on = OperatesOn.Byte, .expected = .CH },
        .{ .in = 0b110, .operates_on = OperatesOn.Byte, .expected = .DH },
        .{ .in = 0b111, .operates_on = OperatesOn.Byte, .expected = .BH },
        // Word
        .{ .in = 0b000, .operates_on = OperatesOn.Word, .expected = .AX },
        .{ .in = 0b001, .operates_on = OperatesOn.Word, .expected = .CX },
        .{ .in = 0b010, .operates_on = OperatesOn.Word, .expected = .DX },
        .{ .in = 0b011, .operates_on = OperatesOn.Word, .expected = .BX },
        .{ .in = 0b100, .operates_on = OperatesOn.Word, .expected = .SP },
        .{ .in = 0b101, .operates_on = OperatesOn.Word, .expected = .BP },
        .{ .in = 0b110, .operates_on = OperatesOn.Word, .expected = .SI },
        .{ .in = 0b111, .operates_on = OperatesOn.Word, .expected = .DI },
    };

    for (test_cases) |case| {
        const actual = decodeRegister(case.operates_on, 0b00000111, case.in);
        if (actual != case.expected) {
            std.debug.print("0b{b}: want: {}, got: {}\n", .{ case.in, case.expected, actual });
            return error.TestExpectedEqual;
        }
    }
}

test "decodeDisplacement" {
    const TestCase = struct {
        name: [:0]const u8,
        mode: Mode,
        calc: EffectiveAddressCalculation,
        disp_buf: []const u8,
        expected: ?Displacement,
    };
    const test_cases = [_]TestCase{
        .{
            .name = "Mode.Reg has no displacment",
            .mode = Mode.Reg,
            .calc = EffectiveAddressCalculation.BP, // doesn't matter
            .disp_buf = &[_]u8{},
            .expected = null,
        },
        .{
            .name = "Mode.Mem decode DIRECT ADDRESS",
            .mode = Mode.Mem,
            .calc = EffectiveAddressCalculation.DIRECT_ADDRESS,
            .disp_buf = &[_]u8{ 0x0, 0x1 },
            .expected = Displacement{
                .word = 256,
            },
        },
        .{
            .name = "Mode.Mem8BitDisplacement decode 8 bits",
            .mode = Mode.Mem8BitDisplacement,
            .calc = EffectiveAddressCalculation.BP, // doesn't matter
            .disp_buf = &[_]u8{42},
            .expected = Displacement{
                .byte = 42,
            },
        },
        .{
            .name = "Mode.Mem16BitDisplacement decode 16 bits",
            .mode = Mode.Mem16BitDisplacement,
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
            std.debug.print("Unexpected error: {}", .{actual_error});
            return error.TestUnexpectedResult;
        };
        std.testing.expectEqual(case.expected, actual) catch |err| {
            std.debug.print("Test failure: {s}\n", .{case.name});
            return err;
        };
    }
}

test "decodeInstruction" {
    const TestCase = struct {
        name: [:0]const u8,
        in: []const u8,
        expected: Instruction,
    };
    const test_cases = [_]TestCase{
        .{
            .name = "mov BX, AX",
            .in = &[_]u8{ 0b10001001, 0b11000011 },
            .expected = .{
                .op = opcode.Mnemonic.mov_rm_to_from_r,
                .src = SrcType{
                    .register = Register.AX,
                },
                .dst = DstType{
                    .register = Register.BX,
                },
            },
        },
        .{
            .name = "mov AX, BX",
            .in = &[2]u8{ 0b10001011, 0b11000011 },
            .expected = .{
                .op = opcode.Mnemonic.mov_rm_to_from_r,
                .src = SrcType{
                    .register = Register.BX,
                },
                .dst = DstType{
                    .register = Register.AX,
                },
            },
        },
        .{
            .name = "mov BL, AL",
            .in = &[2]u8{ 0b10001000, 0b11000011 },
            .expected = .{
                .op = opcode.Mnemonic.mov_rm_to_from_r,
                .src = SrcType{
                    .register = Register.AL,
                },
                .dst = DstType{
                    .register = Register.BL,
                },
            },
        },
        .{
            .name = "mov CL, 42",
            .in = &[2]u8{ 0b10110001, 42 },
            .expected = .{
                .op = opcode.Mnemonic.mov_imm_to_r,
                .src = SrcType{
                    .immediate = ImmediateField{
                        .value = ImmediateValue{
                            .byte = 42,
                        },
                        .explicit_size = false,
                    },
                },
                .dst = DstType{
                    .register = Register.CL,
                },
            },
        },
        .{
            .name = "mov CL, -42",
            .in = &[2]u8{ 0b10110001, 0b11010110 },
            .expected = .{
                .op = opcode.Mnemonic.mov_imm_to_r,
                .src = SrcType{
                    .immediate = ImmediateField{
                        .value = ImmediateValue{
                            .byte = 256 - 42,
                        },
                        .explicit_size = false,
                    },
                },
                .dst = DstType{
                    .register = Register.CL,
                },
            },
        },
        .{
            .name = "mov BX, 256",
            .in = &[_]u8{ 0b10111011, 0x0, 0x0001 },
            .expected = .{
                .op = opcode.Mnemonic.mov_imm_to_r,
                .src = SrcType{
                    .immediate = ImmediateField{
                        .value = ImmediateValue{
                            .word = 256,
                        },
                        .explicit_size = false,
                    },
                },
                .dst = DstType{
                    .register = Register.BX,
                },
            },
        },
        .{
            .name = "mov [di + 256], word 515",
            .in = &[_]u8{ 0b11000111, 0b10000101, 0x0, 0x1, 0x3, 0x2 },
            .expected = .{
                .op = opcode.Mnemonic.mov_imm_to_rm,
                .src = SrcType{
                    .immediate = ImmediateField{
                        .value = ImmediateValue{
                            .word = 515,
                        },
                        .explicit_size = true,
                    },
                },
                .dst = DstType{
                    .memory = Memory{
                        .calc = EffectiveAddressCalculation.DI,
                        .displacement = Displacement{
                            .word = 256,
                        },
                    },
                },
            },
        },
    };
    std.debug.print("\n", .{});
    for (test_cases) |case| {
        var stream = std.io.fixedBufferStream(case.in[0..]);
        var reader = stream.reader().any();
        const actual = decodeInstruction(&reader);
        try std.testing.expectEqual(case.expected, actual);

        std.testing.expectEqual(case.expected, actual) catch |err| {
            std.debug.print("Test failure: {s}\n", .{case.name});
            return err;
        };
    }
}
