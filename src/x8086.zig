const std = @import("std");
const assert = @import("std").debug.assert;
const native_endian = @import("builtin").target.cpu.arch.endian();

pub const OpCode = enum {
    Unknown,
    mov_rm_to_from_rm,
    mov_imm_to_r,
};

pub fn decodeOpcode(byte: u8) OpCode {
    // Table 4-12: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    const mask_4 = 0b11110000;
    const mask_6 = 0b11111100;

    if (byte & mask_4 == 0b10110000) {
        return OpCode.mov_imm_to_r;
    } else if (byte & mask_6 == 0b10001000) {
        return OpCode.mov_rm_to_from_rm;
    } else {
        @branchHint(.cold);
        return OpCode.Unknown;
    }
}

test "decodeOpcode" {
    const TestCase = struct {
        in: u8,
        expected: OpCode,
    };
    const test_cases = [_]TestCase{
        .{ .in = 0b10011000, .expected = .Unknown },
        .{ .in = 0b10001100, .expected = .Unknown },
        .{ .in = 0b10001000, .expected = .mov_rm_to_from_rm },
        .{ .in = 0b10001010, .expected = .mov_rm_to_from_rm },
        .{ .in = 0b10001011, .expected = .mov_rm_to_from_rm },
        .{ .in = 0b10110000, .expected = .mov_imm_to_r },
        .{ .in = 0b10111011, .expected = .mov_imm_to_r },
    };

    for (test_cases) |case| {
        const actual = decodeOpcode(case.in);
        if (actual != case.expected) {
            std.debug.print("0b{b}: want: {}, got: {}\n", .{ case.in, case.expected, actual });
            return error.TestExpectedEqual;
        }
    }
}

const Direction = enum {
    ToRegister, // Instruction destination is specified in REG field
    FromRegister, // Instruction source is specified in REG field
};

fn decodeDirectionBit(mask_dir_bit: u8, byte: u8) Direction {
    // Table 4-7: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    const masked = byte & mask_dir_bit;
    if (masked == 0b00000000) {
        return Direction.FromRegister;
    } else {
        return Direction.ToRegister;
    }
}

const Wide = enum {
    Byte,
    Word,
};

fn decodeWideBit(mask_wide_bit: u8, byte: u8) Wide {
    // Table 4-7: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    const masked = byte & mask_wide_bit;
    if (masked == 0b00000000) {
        return Wide.Byte;
    } else {
        return Wide.Word;
    }
}

const Mode = enum {
    Mem,
    Mem8BitDisplacement,
    Mem16BitDisplacement,
    Reg,
};

fn decodeMode(byte: u8) Mode {
    // Table 4-8: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    const mask_mode = 0b11000000;
    const masked = byte & mask_mode;
    if (masked == 0b00000000) return Mode.Mem;
    if (masked == 0b01000000) return Mode.Mem8BitDisplacement;
    if (masked == 0b10000000) return Mode.Mem16BitDisplacement;
    return Mode.Reg;
}

const Register = enum { AL, AH, AX, BL, BH, BX, CL, CH, CX, DL, DH, DX, SI, DI, BP, SP };

fn lookupRegisterByte(raw: u8) Register {
    assert(raw <= 0b111);

    // Table 4-9: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    if (raw == 0b000) return Register.AL;
    if (raw == 0b001) return Register.CL;
    if (raw == 0b010) return Register.DL;
    if (raw == 0b011) return Register.BL;
    if (raw == 0b100) return Register.AH;
    if (raw == 0b101) return Register.CH;
    if (raw == 0b110) return Register.DH;
    if (raw == 0b111) return Register.BH;
    unreachable;
}

fn lookupRegisterWord(raw: u8) Register {
    assert(raw <= 0b111);

    // Table 4-9: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    if (raw == 0b000) return Register.AX;
    if (raw == 0b001) return Register.CX;
    if (raw == 0b010) return Register.DX;
    if (raw == 0b011) return Register.BX;
    if (raw == 0b100) return Register.SP;
    if (raw == 0b101) return Register.BP;
    if (raw == 0b110) return Register.SI;
    if (raw == 0b111) return Register.DI;
    unreachable;
}

fn lookupRegister(wide: Wide, byte: u8) Register {
    switch (wide) {
        Wide.Byte => {
            return lookupRegisterByte(byte);
        },
        Wide.Word => {
            return lookupRegisterWord(byte);
        },
    }
    unreachable;
}

fn decodeRM(mode: Mode, wide: Wide, byte: u8) Register {
    // Table 4-10: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    assert(mode == Mode.Reg);
    const mask_reg = 0b00000111;
    const masked = byte & mask_reg;
    return lookupRegister(wide, masked);
}

test "lookupRegister" {
    const TestCase = struct {
        in: u8,
        wide: Wide,
        expected: Register,
    };
    const test_cases = [_]TestCase{
        // Byte
        .{ .in = 0b00000000, .wide = Wide.Byte, .expected = .AL },
        .{ .in = 0b00000001, .wide = Wide.Byte, .expected = .CL },
        .{ .in = 0b00000010, .wide = Wide.Byte, .expected = .DL },
        .{ .in = 0b00000011, .wide = Wide.Byte, .expected = .BL },
        .{ .in = 0b00000100, .wide = Wide.Byte, .expected = .AH },
        .{ .in = 0b00000101, .wide = Wide.Byte, .expected = .CH },
        .{ .in = 0b00000110, .wide = Wide.Byte, .expected = .DH },
        .{ .in = 0b00000111, .wide = Wide.Byte, .expected = .BH },
        // Word
        .{ .in = 0b00000000, .wide = Wide.Word, .expected = .AX },
        .{ .in = 0b00000001, .wide = Wide.Word, .expected = .CX },
        .{ .in = 0b00000010, .wide = Wide.Word, .expected = .DX },
        .{ .in = 0b00000011, .wide = Wide.Word, .expected = .BX },
        .{ .in = 0b00000100, .wide = Wide.Word, .expected = .SP },
        .{ .in = 0b00000101, .wide = Wide.Word, .expected = .BP },
        .{ .in = 0b00000110, .wide = Wide.Word, .expected = .SI },
        .{ .in = 0b00000111, .wide = Wide.Word, .expected = .DI },
    };

    for (test_cases) |case| {
        const actual = lookupRegister(case.wide, case.in);
        if (actual != case.expected) {
            std.debug.print("0b{b}: want: {}, got: {}\n", .{ case.in, case.expected, actual });
            return error.TestExpectedEqual;
        }
    }
}

const ImmediateTypeTag = enum {
    byte,
    word,
};

const Immediate = union(ImmediateTypeTag) {
    byte: u8,
    word: u16,
};

const SrcTypeTag = enum {
    register,
    immediate,
};

const DstTypeTag = enum {
    register,
};

const SrcType = union(SrcTypeTag) {
    register: Register,
    immediate: Immediate,

    pub fn format(
        src_type: SrcType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (src_type) {
            .register => |reg| return writer.print("{s}", .{std.enums.tagName(Register, reg).?}),
            .immediate => |i| switch (i) {
                .byte => |b| return writer.print("{d}", .{b}),
                .word => |w| return writer.print("{d}", .{w}),
            },
        }
        unreachable;
    }
};

const DstType = union(DstTypeTag) {
    register: Register,

    pub fn format(
        dst_type: DstType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (dst_type) {
            .register => |reg| return writer.print("{s}", .{std.enums.tagName(Register, reg).?}),
        }
        unreachable;
    }
};

const Instruction = struct {
    op: OpCode,
    src: SrcType,
    dst: DstType,

    pub fn format(
        instruction: Instruction,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const op = switch (instruction.op) {
            OpCode.mov_rm_to_from_rm => "mov",
            OpCode.mov_imm_to_r => "mov",
            OpCode.Unknown => "<unknown>",
        };
        return writer.print("{s} {}, {}", .{ op, instruction.dst, instruction.src });
    }
};

fn decodeInstruction(reader: *std.io.AnyReader) !Instruction {
    const byte_1 = try reader.readByte();
    const op = decodeOpcode(byte_1);

    switch (op) {
        OpCode.mov_rm_to_from_rm => {
            const dir = decodeDirectionBit(0b00000010, byte_1);
            const wide = decodeWideBit(0b00000001, byte_1);

            const byte_2 = try reader.readByte();
            const mode = decodeMode(byte_2);
            assert(mode == Mode.Reg);

            const mask_reg = 0b00111000;
            const shifted = (byte_2 & mask_reg) >> 3;
            const reg = lookupRegister(wide, shifted);

            const rm = decodeRM(mode, wide, byte_2);

            return Instruction{
                .op = op,
                .src = SrcType{
                    .register = if (dir == Direction.FromRegister) reg else rm,
                },
                .dst = DstType{
                    .register = if (dir == Direction.ToRegister) reg else rm,
                },
            };
        },
        OpCode.mov_imm_to_r => {
            const wide = decodeWideBit(0b00001000, byte_1);

            const mask_reg = 0b00000111;
            const reg = lookupRegister(wide, byte_1 & mask_reg);

            switch (wide) {
                Wide.Byte => {
                    return Instruction{
                        .op = op,
                        .src = SrcType{
                            .immediate = Immediate{
                                .byte = try reader.readByte(),
                            },
                        },
                        .dst = DstType{
                            .register = reg,
                        },
                    };
                },
                Wide.Word => {
                    return Instruction{
                        .op = op,
                        .src = SrcType{
                            .immediate = Immediate{
                                .word = try reader.readInt(u16, native_endian),
                            },
                        },
                        .dst = DstType{
                            .register = reg,
                        },
                    };
                },
            }
            unreachable;
        },
        OpCode.Unknown => {
            return error.UnknownInstruction;
        },
    }
    unreachable;
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
                .op = OpCode.mov_rm_to_from_rm,
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
                .op = OpCode.mov_rm_to_from_rm,
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
                .op = OpCode.mov_rm_to_from_rm,
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
                .op = OpCode.mov_imm_to_r,
                .src = SrcType{
                    .immediate = Immediate{
                        .byte = 42,
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
                .op = OpCode.mov_imm_to_r,
                .src = SrcType{
                    .immediate = Immediate{
                        .byte = 256 - 42,
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
                .op = OpCode.mov_imm_to_r,
                .src = SrcType{
                    .immediate = Immediate{
                        .word = 256,
                    },
                },
                .dst = DstType{
                    .register = Register.BX,
                },
            },
        },
    };
    std.debug.print("\n", .{});
    for (test_cases) |case| {
        var stream = std.io.fixedBufferStream(case.in[0..]);
        var reader = stream.reader().any();
        const actual = decodeInstruction(&reader);

        std.testing.expectEqual(case.expected, actual) catch |err| {
            std.debug.print("Test failure: {s}\n", .{case.name});
            return err;
        };
    }
}

pub fn decode(alloc: std.mem.Allocator, bin: []const u8) ![]const u8 {
    var result = std.ArrayList(u8).init(alloc);
    defer result.deinit();

    var stream = std.io.fixedBufferStream(bin);
    var reader = stream.reader().any();
    try result.appendSlice("bits 16\n");

    var lineBuffer: [1024]u8 = undefined;
    while (true) {
        const instruction = decodeInstruction(&reader) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        const instruction_string = try std.fmt.bufPrint(&lineBuffer, "{}\n", .{instruction});
        try result.appendSlice(std.ascii.lowerString(instruction_string, instruction_string));
    }

    return result.toOwnedSlice();
}
