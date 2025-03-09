const std = @import("std");
const assert = @import("std").debug.assert;
const native_endian = @import("builtin").target.cpu.arch.endian();

pub const OpCode = enum {
    Unknown,
    mov_rm_to_from_rm,
    mov_imm_to_rm,
    mov_imm_to_r,
    mov_mem_to_accumulator,
    mov_accumulator_to_mem,
};

pub fn decodeOpcode(byte: u8) OpCode {
    // Table 4-12: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    const mask_4 = 0b11110000;
    const mask_6 = 0b11111100;
    const mask_7 = 0b11111110;

    if (byte & mask_4 == 0b10110000) {
        return OpCode.mov_imm_to_r;
    } else if (byte & mask_6 == 0b10001000) {
        return OpCode.mov_rm_to_from_rm;
    } else if (byte & mask_7 == 0b11000110) {
        return OpCode.mov_imm_to_rm;
    } else if (byte & mask_7 == 0b10100000) {
        return OpCode.mov_mem_to_accumulator;
    } else if (byte & mask_7 == 0b10100010) {
        return OpCode.mov_accumulator_to_mem;
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
        .{ .in = 0b11000110, .expected = .mov_imm_to_rm },
        .{ .in = 0b11000111, .expected = .mov_imm_to_rm },
        .{ .in = 0b10100000, .expected = .mov_mem_to_accumulator },
        .{ .in = 0b10100001, .expected = .mov_mem_to_accumulator },
        .{ .in = 0b10100010, .expected = .mov_accumulator_to_mem },
        .{ .in = 0b10100011, .expected = .mov_accumulator_to_mem },
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

const EffectiveAddressCalculation = enum {
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

fn lookupEffectiveAddressCalcuation(mode: Mode, raw: u8) EffectiveAddressCalculation {
    assert(raw <= 0b111);

    // Table 4-9: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    if (raw == 0b000) return EffectiveAddressCalculation.BX_PLUS_SI;
    if (raw == 0b001) return EffectiveAddressCalculation.BX_PLUS_DI;
    if (raw == 0b010) return EffectiveAddressCalculation.BP_PLUS_SI;
    if (raw == 0b011) return EffectiveAddressCalculation.BP_PLUS_DI;
    if (raw == 0b100) return EffectiveAddressCalculation.SI;
    if (raw == 0b101) return EffectiveAddressCalculation.DI;
    if (raw == 0b110) {
        if (mode == Mode.Mem) {
            return EffectiveAddressCalculation.DIRECT_ADDRESS;
        }
        return EffectiveAddressCalculation.BP;
    }
    if (raw == 0b111) return EffectiveAddressCalculation.BX;
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

const Memory = struct {
    calc: EffectiveAddressCalculation,
    displacement: ?Displacement,

    pub fn format(
        mem: Memory,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const str = switch (mem.calc) {
            EffectiveAddressCalculation.BX_PLUS_SI => "bx + si",
            EffectiveAddressCalculation.BX_PLUS_DI => "bx + di",
            EffectiveAddressCalculation.BP_PLUS_SI => "bp + si",
            EffectiveAddressCalculation.BP_PLUS_DI => "bp + di",
            EffectiveAddressCalculation.SI => "si",
            EffectiveAddressCalculation.DI => "di",
            EffectiveAddressCalculation.BP => "bp",
            EffectiveAddressCalculation.DIRECT_ADDRESS => "",
            EffectiveAddressCalculation.BX => "bx",
        };

        if (mem.displacement) |d| {
            switch (d) {
                .byte => |b| {
                    if (b == 0) {
                        try writer.print("[{s}]", .{str});
                        return;
                    } else if (b > 127) {
                        // Displacements in ASM are written as signed and used to decide
                        // if 16-bit displacment is needed. So despite it being ambiguous
                        // what the original ASM was, we opt for the version that will
                        // produce smaller binaries if assembled again.
                        const signed: i8 = @bitCast(b);
                        try writer.print("[{s} - {d}]", .{ str, @abs(signed) });
                        return;
                    } else {
                        try writer.print("[{s} + {d}]", .{ str, b });
                        return;
                    }
                },
                .word => |w| {
                    if (mem.calc == EffectiveAddressCalculation.DIRECT_ADDRESS) {
                        try writer.print("[{d}]", .{w});
                        return;
                    } else if (w == 0) {
                        try writer.print("[{s}]", .{str});
                        return;
                    } else if (w > 32768) {
                        const signed: i16 = @bitCast(w);
                        try writer.print("[{s} - {d}]", .{ str, @abs(signed) });
                    } else {
                        try writer.print("[{s} + {d}]", .{ str, w });
                        return;
                    }
                },
            }
        } else {
            try writer.print("[{s}]", .{str});
            return;
        }
    }
};

const RegMemFieldTag = enum {
    register,
    memory,
};

const RegMemField = union(RegMemFieldTag) {
    register: Register,
    memory: Memory,
};

fn decodeRM(mode: Mode, wide: Wide, byte: u8, reader: *std.io.AnyReader) !RegMemField {
    // Table 4-10: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    switch (mode) {
        Mode.Reg => {
            return RegMemField{
                .register = lookupRegister(wide, byte),
            };
        },
        else => {
            const calc = lookupEffectiveAddressCalcuation(mode, byte);
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

const ImmediateValue = union(ImmediateValueTag) {
    byte: u8,
    word: u16,
};

const ImmediateField = struct {
    value: ImmediateValue,
    explicit_size: bool,

    pub fn format(
        field: ImmediateField,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (field.value) {
            .byte => |b| {
                if (field.explicit_size) {
                    try writer.print("byte {d}", .{b});
                } else {
                    try writer.print("{d}", .{b});
                }
            },
            .word => |w| {
                if (field.explicit_size) {
                    try writer.print("word {d}", .{w});
                } else {
                    try writer.print("{d}", .{w});
                }
            },
        }
    }
};

const SrcTypeTag = enum {
    register,
    immediate,
    memory,
};

const DstTypeTag = enum {
    register,
    memory,
};

const SrcType = union(SrcTypeTag) {
    register: Register,
    immediate: ImmediateField,
    memory: Memory,

    pub fn format(
        src_type: SrcType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (src_type) {
            .register => |reg| try writer.print("{s}", .{std.enums.tagName(Register, reg).?}),
            .immediate => |i| try writer.print("{}", .{i}),
            .memory => |mem| try writer.print("{}", .{mem}),
        }
    }
};

const DstType = union(DstTypeTag) {
    register: Register,
    memory: Memory,

    pub fn format(
        dst_type: DstType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (dst_type) {
            .register => |reg| return writer.print("{s}", .{std.enums.tagName(Register, reg).?}),
            .memory => |mem| return writer.print("{}", .{mem}),
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
            OpCode.mov_imm_to_rm => "mov",
            OpCode.mov_accumulator_to_mem => "mov",
            OpCode.mov_mem_to_accumulator => "mov",
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

            const mask_reg = 0b00111000;
            const shifted = (byte_2 & mask_reg) >> 3;
            const reg = lookupRegister(wide, shifted);

            const rm = try decodeRM(mode, wide, byte_2 & 0b00000111, reader);
            switch (rm) {
                .register => |r| {
                    return Instruction{
                        .op = op,
                        .src = SrcType{
                            .register = if (dir == Direction.FromRegister) reg else r,
                        },
                        .dst = DstType{
                            .register = if (dir == Direction.ToRegister) reg else r,
                        },
                    };
                },
                .memory => |mem| {
                    switch (dir) {
                        Direction.FromRegister => return Instruction{
                            .op = op,
                            .src = SrcType{
                                .register = reg,
                            },
                            .dst = DstType{
                                .memory = mem,
                            },
                        },
                        Direction.ToRegister => return Instruction{
                            .op = op,
                            .src = SrcType{
                                .memory = mem,
                            },
                            .dst = DstType{
                                .register = reg,
                            },
                        },
                    }
                },
            }

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
                            .immediate = ImmediateField{
                                .value = ImmediateValue{
                                    .byte = try reader.readByte(),
                                },
                                .explicit_size = false,
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
                            .immediate = ImmediateField{
                                .value = ImmediateValue{
                                    .word = try reader.readInt(u16, native_endian),
                                },
                                .explicit_size = false,
                            },
                        },
                        .dst = DstType{
                            .register = reg,
                        },
                    };
                },
            }
        },
        OpCode.mov_imm_to_rm => {
            const wide = decodeWideBit(0b00000001, byte_1);
            const byte_2 = try reader.readByte();
            const mode = decodeMode(byte_2);
            assert(mode != Mode.Reg);
            const rm = try decodeRM(mode, wide, byte_2 & 0b00000111, reader);
            if (mode == Mode.Mem8BitDisplacement) {
                // DISP is always 16bit before data begins
                _ = try reader.readByte();
            }

            var dst: DstType = undefined;
            switch (rm) {
                .register => |r| {
                    dst = DstType{
                        .register = r,
                    };
                },
                .memory => |m| {
                    dst = DstType{
                        .memory = m,
                    };
                },
            }
            var src: SrcType = undefined;
            switch (wide) {
                Wide.Byte => {
                    src = SrcType{
                        .immediate = ImmediateField{
                            .value = ImmediateValue{
                                .byte = try reader.readByte(),
                            },
                            .explicit_size = true,
                        },
                    };
                },
                Wide.Word => {
                    src = SrcType{
                        .immediate = ImmediateField{
                            .value = ImmediateValue{
                                .word = try reader.readInt(u16, native_endian),
                            },
                            .explicit_size = true,
                        },
                    };
                },
            }
            return Instruction{
                .op = op,
                .src = src,
                .dst = dst,
            };
        },
        OpCode.mov_accumulator_to_mem => {
            const wide = decodeWideBit(0b00000001, byte_1);
            const src = SrcType{
                .register = if (wide == Wide.Byte) .AL else .AX,
            };
            var displacement: Displacement = undefined;
            if (wide == Wide.Byte) {
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
        OpCode.mov_mem_to_accumulator => {
            const wide = decodeWideBit(0b00000001, byte_1);
            const dst = DstType{
                .register = if (wide == Wide.Byte) .AL else .AX,
            };
            var displacement: Displacement = undefined;
            if (wide == Wide.Byte) {
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
            const src = SrcType{
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
                .op = OpCode.mov_imm_to_r,
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
                .op = OpCode.mov_imm_to_r,
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
                .op = OpCode.mov_imm_to_rm,
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
