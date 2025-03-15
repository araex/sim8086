const std = @import("std");
const assert = @import("std").debug.assert;
const native_endian = @import("builtin").target.cpu.arch.endian();
const opcode = @import("opcode.zig");
const pretty = @import("log.zig").pretty;

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

fn isWideRegister(reg: Register) bool {
    return @intFromEnum(reg) < @intFromEnum(Register.AX);
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
};

fn makeImmediate(val: anytype) ImmediateField {
    return ImmediateField{
        .value = switch (@TypeOf(val)) {
            u8 => .{ .byte = val },
            u16 => .{ .word = val },
            ImmediateValue => val,
            else => unreachable,
        },
    };
}

fn decodeImmediate(operates_on: OperatesOn, byte: u8, reader: anytype) !ImmediateField {
    switch (operates_on) {
        OperatesOn.Byte => return makeImmediate(byte),
        OperatesOn.Word => {
            // We alreadty have the first byte, read the second byte and construct an u16 (native endianness)
            const second_byte = try reader.readByte();
            const value: u16 = @as(u16, byte) | (@as(u16, second_byte) << 8);
            return makeImmediate(value);
        },
    }
    unreachable;
}

fn decodeImmediateWithSignExtension(byte: u8) !ImmediateField {
    return ImmediateField{
        .value = ImmediateValue{
            .byte = byte,
        },
    };
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
        u8, u16 => return makeSrc(makeImmediate(val)),
        else => {
            pretty().withColor(.red).print("unhandled type '{}' with value '{any}'", .{ @TypeOf(val), val });
        },
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
        else => {
            pretty().withColor(.red).print("unhandled type '{}' with value '{any}'", .{ @TypeOf(val), val });
        },
    }
}

pub const Instruction = struct {
    op: opcode.Mnemonic,
    wide: OperatesOn,
    src: SrcType,
    dst: DstType,
};

fn makeInstruction(op: opcode.Mnemonic, operates_on: OperatesOn, dst: anytype, src: anytype) Instruction {
    return Instruction{
        .op = op,
        .wide = operates_on,
        .dst = makeDst(dst),
        .src = makeSrc(src),
    };
}

fn decodeInstruction(reader: *std.io.AnyReader) !Instruction {
    const byte_1 = try reader.readByte();
    const byte_2 = try reader.readByte();
    const op = opcode.decode(byte_1, byte_2);

    switch (op) {
        .add_rm_with_r_to_either,
        .sub_rm_and_r_to_either,
        .cmp_rm_with_r,
        .mov_rm_to_from_r,
        => {
            const operates_on = decodeOperatesOn(0b00000001, byte_1);
            const dir = decodeDirection(0b00000010, byte_1);
            const mode = decodeMode(0b11000000, byte_2);
            const reg = decodeRegister(operates_on, 0b00111000, byte_2);
            const rm = try decodeRM(mode, operates_on, 0b00000111, byte_2, reader);
            switch (dir) {
                Direction.FromRegister => return makeInstruction(op, operates_on, rm, reg),
                Direction.ToRegister => return makeInstruction(op, operates_on, reg, rm),
            }
            unreachable;
        },
        .mov_imm_to_r => {
            const operates_on = decodeOperatesOn(0b00001000, byte_1);
            const reg = decodeRegister(operates_on, 0b00000111, byte_1);
            const immediate = try decodeImmediate(operates_on, byte_2, reader);
            return makeInstruction(op, operates_on, reg, immediate);
        },
        .mov_imm_to_rm => {
            const operates_on = decodeOperatesOn(0b00000001, byte_1);
            const mode = decodeMode(0b11000000, byte_2);
            assert(mode != Mode.Reg);
            const rm = try decodeRM(mode, operates_on, 0b00000111, byte_2, reader);
            if (mode == Mode.Mem8BitDisplacement) {
                // DISP is always 16bit before data begins
                _ = try reader.readByte();
            }
            const immediate = try decodeImmediate(operates_on, try reader.readByte(), reader);
            return makeInstruction(op, operates_on, rm, immediate);
        },
        .mov_accumulator_to_mem,
        .mov_mem_to_accumulator,
        => |dir| {
            const operates_on = decodeOperatesOn(0b00000001, byte_1);
            const reg = if (operates_on == OperatesOn.Byte) Register.AL else Register.AX;
            const displacement = if (operates_on == OperatesOn.Byte)
                Displacement{ .byte = byte_2 }
            else
                Displacement{ .word = @as(u16, byte_2) | (@as(u16, try reader.readByte()) << 8) };

            const mem = Memory{
                .calc = EffectiveAddressCalculation.DIRECT_ADDRESS,
                .displacement = displacement,
            };

            switch (dir) {
                .mov_accumulator_to_mem => return makeInstruction(op, operates_on, makeDst(mem), reg),
                .mov_mem_to_accumulator => return makeInstruction(op, operates_on, reg, makeSrc(mem)),
                else => unreachable,
            }
        },
        .add_imm_to_rm,
        .sub_imm_to_rm,
        .cmp_imm_with_rm,
        => {
            const operates_on = decodeOperatesOn(0b00000001, byte_1);
            const sign_extension = (byte_1 & 0b00000010) != 0;
            const mode = decodeMode(0b11000000, byte_2);
            const rm = try decodeRM(mode, operates_on, 0b00000111, byte_2, reader);
            if (mode == Mode.Mem8BitDisplacement) {
                // DISP is always 16bit before data begins
                _ = try reader.readByte();
            }
            const immediate = if (sign_extension)
                try decodeImmediateWithSignExtension(try reader.readByte())
            else
                try decodeImmediate(operates_on, try reader.readByte(), reader);
            return makeInstruction(op, operates_on, rm, immediate);
        },
        .add_imm_to_acc,
        .sub_imm_to_acc,
        .cmp_imm_with_acc,
        => {
            const operates_on = decodeOperatesOn(0b00000001, byte_1);
            const reg = if (operates_on == OperatesOn.Byte) Register.AL else Register.AX;
            const immediate = try decodeImmediate(operates_on, byte_2, reader);
            return makeInstruction(op, operates_on, reg, immediate);
        },
        .Unknown => {
            pretty().withColor(.red).print("Unknown opcode: {x} {b}\n", .{ byte_1, byte_2 });
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
            .expected = makeInstruction(
                .mov_rm_to_from_r,
                .Word,
                Register.BX,
                Register.AX,
            ),
        },
        .{
            .name = "mov AX, BX",
            .in = &[2]u8{ 0b10001011, 0b11000011 },
            .expected = makeInstruction(
                .mov_rm_to_from_r,
                .Word,
                Register.AX,
                Register.BX,
            ),
        },
        .{
            .name = "mov BL, AL",
            .in = &[2]u8{ 0b10001000, 0b11000011 },
            .expected = makeInstruction(
                .mov_rm_to_from_r,
                .Byte,
                Register.BL,
                Register.AL,
            ),
        },
        .{
            .name = "mov CL, 42",
            .in = &[2]u8{ 0b10110001, 42 },
            .expected = makeInstruction(
                .mov_imm_to_r,
                .Byte,
                Register.CL,
                @as(u8, 42),
            ),
        },
        .{
            .name = "mov CL, -42",
            .in = &[2]u8{ 0b10110001, 0b11010110 },
            .expected = makeInstruction(
                .mov_imm_to_r,
                .Byte,
                Register.CL,
                @as(u8, 256 - 42),
            ),
        },
        .{
            .name = "mov BX, 256",
            .in = &[_]u8{ 0b10111011, 0x0, 0x0001 },
            .expected = makeInstruction(
                .mov_imm_to_r,
                .Word,
                Register.BX,
                @as(u16, 256),
            ),
        },
        .{
            .name = "mov [di + 256], word 515",
            .in = &[_]u8{ 0b11000111, 0b10000101, 0x0, 0x1, 0x3, 0x2 },
            .expected = makeInstruction(
                .mov_imm_to_rm,
                .Word,
                Memory{
                    .calc = EffectiveAddressCalculation.DI,
                    .displacement = Displacement{
                        .word = 256,
                    },
                },
                ImmediateField{
                    .value = ImmediateValue{
                        .word = 515,
                    },
                },
            ),
        },
        .{
            .name = "add cl, 8",
            .in = &[_]u8{ 0x80, 0b11000001, 0x8 },
            .expected = makeInstruction(
                .add_imm_to_rm,
                .Byte,
                Register.CL,
                @as(u8, 8),
            ),
        },
        .{
            .name = "add cx, 256",
            .in = &[_]u8{ 0x81, 0b11000001, 0x0, 0x1 },
            .expected = makeInstruction(
                .add_imm_to_rm,
                .Word,
                Register.CX,
                @as(u16, 256),
            ),
        },
        .{
            .name = "add cl, -8",
            .in = &[_]u8{ 0x82, 0b11000001, 0b11110111 },
            .expected = makeInstruction(
                .add_imm_to_rm,
                .Byte,
                Register.CL,
                @as(u8, 247),
            ),
        },
        .{
            .name = "add al, 8",
            .in = &[_]u8{ 0x04, 0x08 },
            .expected = makeInstruction(
                .add_imm_to_acc,
                .Byte,
                Register.AL,
                @as(u8, 8),
            ),
        },
        .{
            .name = "sub cl, 8",
            .in = &[_]u8{ 0x80, 0b11101001, 0x8 },
            .expected = makeInstruction(
                .sub_imm_to_rm,
                .Byte,
                Register.CL,
                @as(u8, 8),
            ),
        },
        .{
            .name = "sub cx, 256",
            .in = &[_]u8{ 0x81, 0b11101001, 0x0, 0x1 },
            .expected = makeInstruction(
                .sub_imm_to_rm,
                .Word,
                Register.CX,
                @as(u16, 256),
            ),
        },
        .{
            .name = "sub cx, -8",
            .in = &[_]u8{ 0x83, 0b11101001, 0b11110111 },
            .expected = makeInstruction(
                .sub_imm_to_rm,
                .Word,
                Register.CX,
                @as(u8, 247),
            ),
        },
        .{
            .name = "sub al, 8",
            .in = &[_]u8{ 0x2C, 0x08 },
            .expected = makeInstruction(
                .sub_imm_to_acc,
                .Byte,
                Register.AL,
                @as(u8, 8),
            ),
        },
    };

    std.debug.print("\nTest decodeInstructions\n", .{});
    try pretty().cfg.setColor(std.io.getStdErr(), .bright_black);
    defer pretty().cfg.setColor(std.io.getStdErr(), .reset) catch {};

    const err_out = pretty().withColor(.red);

    for (test_cases) |case| {
        std.debug.print("-> {s}\n", .{case.name});
        var stream = std.io.fixedBufferStream(case.in[0..]);
        var reader = stream.reader().any();
        const actual = try decodeInstruction(&reader);

        std.testing.expectEqual(case.expected, actual) catch |err| {
            err_out.print("FAILURE!\n", .{});
            err_out.print("Expected:\n", .{});
            try std.json.stringify(case.expected, .{}, std.io.getStdErr().writer());
            err_out.print("\nGot:\n", .{});
            try std.json.stringify(actual, .{}, std.io.getStdErr().writer());
            return err;
        };
    }
    pretty().withColor(.green).print("Success!\n", .{});
}
