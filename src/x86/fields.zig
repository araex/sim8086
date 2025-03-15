const assert = @import("std").debug.assert;
const std = @import("std");

const native_endian = @import("builtin").target.cpu.arch.endian();

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

pub const Mode = enum(u3) {
    Mem,
    Mem8BitDisplacement,
    Mem16BitDisplacement,
    Reg,
};

pub const OperatesOn = enum {
    Byte,
    Word,
};

pub const Direction = enum {
    ToRegister, // Instruction destination is specified in REG field
    FromRegister, // Instruction source is specified in REG field
};

pub const DisplacementTagType = enum {
    byte,
    word,
};
pub const Displacement = union(DisplacementTagType) {
    byte: u8,
    word: u16,
};

pub fn decodeDisplacement(mode: Mode, calc: EffectiveAddressCalculation, reader: *std.io.AnyReader) !?Displacement {
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

pub fn decodeOperatesOn(comptime mask: u8, byte: u8) OperatesOn {
    // Table 4-7: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n257/mode/1up
    if ((byte & mask) == 0) {
        return OperatesOn.Byte;
    }
    return OperatesOn.Word;
}

pub fn decodeDirection(comptime mask: u8, byte: u8) Direction {
    // Table 4-7: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n257/mode/1up
    if ((byte & mask) == 0) {
        return Direction.FromRegister;
    }
    return Direction.ToRegister;
}

pub fn decodeMode(comptime mask_mode_bits: u8, byte: u8) Mode {
    // Table 4-8: https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n258/mode/1up
    const num_bits = 2;
    comptime assert(isValidMask(mask_mode_bits, num_bits));
    const required_shift: u3 = @intCast(@bitSizeOf(u8) - num_bits - @clz(mask_mode_bits));
    const mode_bits: u3 = @intCast((byte & mask_mode_bits) >> required_shift);
    return @enumFromInt(mode_bits);
}

fn countConsecutiveBits(comptime mask: u8) u8 {
    return @clz(mask) + @ctz(mask);
}

fn isValidMask(comptime mask: u8, comptime expected_set_bit_count: u8) bool {
    return (@popCount(mask) == expected_set_bit_count) and (countConsecutiveBits(mask) == (8 - expected_set_bit_count));
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
            std.log.err("Unexpected error: {}", .{actual_error});
            return error.TestUnexpectedResult;
        };
        std.testing.expectEqual(case.expected, actual) catch |err| {
            std.log.err("Test failure: {s}\n", .{case.name});
            return err;
        };
    }
}
