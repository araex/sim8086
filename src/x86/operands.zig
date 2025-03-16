const std = @import("std");

const decodeDisplacement = @import("fields.zig").decodeDisplacement;
const Displacement = @import("fields.zig").Displacement;
const EffectiveAddressCalculation = @import("fields.zig").EffectiveAddressCalculation;
const Mode = @import("fields.zig").Mode;
const OperatesOn = @import("fields.zig").OperatesOn;

const SrcTypeTag = enum {
    register,
    immediate,
    memory,
};

const DstTypeTag = enum {
    register,
    memory,
    jump,
};

pub const SrcType = union(SrcTypeTag) {
    register: Register,
    immediate: ImmediateField,
    memory: Memory,

    pub fn as(self: SrcType, t: type) !t {
        switch (t) {
            u8, u16 => {
                const imm = try self.asImmediateField();
                return imm.as(t);
            },
            ImmediateField => return self.asImmediateField(),
            Register => return self.asRegister(),
            Memory => return self.asMemory(),
            else => return error.InvalidAcces,
        }
    }

    fn asImmediateField(self: SrcType) !ImmediateField {
        switch (self) {
            .immediate => |imm| return imm,
            else => return error.InvalidAcces,
        }
    }

    fn asRegister(self: SrcType) !Register {
        switch (self) {
            .register => |r| return r,
            else => return error.InvalidAcces,
        }
    }

    fn asMemory(self: SrcType) !Memory {
        switch (self) {
            .memory => |m| return m,
            else => return error.InvalidAcces,
        }
    }
};

pub fn makeSrc(val: anytype) ?SrcType {
    switch (@TypeOf(val)) {
        SrcType => return val,
        ?SrcType => return val,
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
        @TypeOf(null) => return null,
        else => {
            std.log.err("unhandled type '{}' with value '{any}'", .{ @TypeOf(val), val });
            unreachable;
        },
    }
}

pub const DstType = union(DstTypeTag) {
    register: Register,
    memory: Memory,
    jump: JumpDestination,
};

pub fn makeDst(val: anytype) DstType {
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
        JumpDestination => {
            return DstType{
                .jump = val,
            };
        },
        else => {
            std.log.err("unhandled type '{}' with value '{any}'", .{ @TypeOf(val), val });
        },
    }
}

pub const RegMemFieldTag = enum {
    register,
    memory,
};

pub const RegMemField = union(RegMemFieldTag) {
    register: Register,
    memory: Memory,
};

pub const Memory = struct {
    calc: EffectiveAddressCalculation,
    displacement: ?Displacement,
};

pub const Register = enum { AL, CL, DL, BL, AH, CH, DH, BH, AX, CX, DX, BX, SP, BP, SI, DI };

pub const ImmediateValueTag = enum {
    byte,
    word,
};

pub const ImmediateValue = union(ImmediateValueTag) {
    byte: u8,
    word: u16,
};

pub const ImmediateField = struct {
    value: ImmediateValue,

    pub fn as(self: ImmediateField, t: type) !t {
        switch (t) {
            u8 => {
                switch (self.value) {
                    .byte => |b| return b,
                    .word => |w| return @intCast(w),
                }
            },
            u16 => {
                switch (self.value) {
                    .byte => |b| return @as(u16, b),
                    .word => |w| return w,
                }
            },
            else => return error.UnsupportedType,
        }
    }
};

pub fn makeImmediate(val: anytype) ImmediateField {
    return ImmediateField{
        .value = switch (@TypeOf(val)) {
            u8 => .{ .byte = val },
            u16 => .{ .word = val },
            ImmediateValue => val,
            else => unreachable,
        },
    };
}

pub const JumpDestination = struct {
    increment: i8,
};

pub fn decodeJumpDestination(byte: u8) JumpDestination {
    return JumpDestination{
        .increment = @bitCast(byte),
    };
}

// Table 4-9: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n258/mode/1up
pub fn decodeRegister(operates_on: OperatesOn, comptime mask_reg_bits: u8, byte: u8) Register {
    const num_bits = 3;
    comptime std.debug.assert(isValidMask(mask_reg_bits, num_bits));
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

pub fn decodeEffectiveAddressCalcuation(mode: Mode, comptime mask_rm_bits: u8, byte: u8) EffectiveAddressCalculation {
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
        if (mode == Mode.Mem) {
            return EffectiveAddressCalculation.DIRECT_ADDRESS;
        }
        return EffectiveAddressCalculation.BP;
    }
    if (reg_bits == 0b111) return EffectiveAddressCalculation.BX;
    unreachable;
}

pub fn decodeRM(mode: Mode, operates_on: OperatesOn, comptime mask_rm_bits: u8, value_to_decode: u8, reader: *std.io.AnyReader) !RegMemField {
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

pub fn decodeImmediate(operates_on: OperatesOn, byte: u8, reader: anytype) !ImmediateField {
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

pub fn decodeImmediateWithSignExtension(byte: u8) !ImmediateField {
    return ImmediateField{
        .value = ImmediateValue{
            .byte = byte,
        },
    };
}

fn countConsecutiveBits(comptime mask: u8) u8 {
    return @clz(mask) + @ctz(mask);
}

fn isValidMask(comptime mask: u8, comptime expected_set_bit_count: u8) bool {
    return (@popCount(mask) == expected_set_bit_count) and (countConsecutiveBits(mask) == (8 - expected_set_bit_count));
}

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
            std.log.err("0b{b}: want: {}, got: {}\n", .{ case.in, case.expected, actual });
            return error.TestExpectedEqual;
        }
    }
}
