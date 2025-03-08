const std = @import("std");
const assert = @import("std").debug.assert;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const alloc = gpa.allocator();

    var args = try std.process.argsWithAllocator(alloc);
    while (args.next()) |arg| {
        std.debug.print("{s}\n", .{arg});
    }
}

const OpCode = enum {
    Unknown,
    MOV,
};

fn decodeOpcode(byte: u8) OpCode {
    const mask_opcode = 0b11111100;

    // Register/memory to/from register
    const opcode_mov = 0b10001000;
    if (byte & mask_opcode == opcode_mov) {
        return OpCode.MOV;
    }
    return OpCode.Unknown;
}

test "decodeOpcode" {
    const TestCase = struct {
        in: u8,
        expected: OpCode,
    };
    const test_cases = [_]TestCase{
        .{ .in = 0b10011000, .expected = .Unknown },
        .{ .in = 0b10001100, .expected = .Unknown },
        // MOV
        .{ .in = 0b10001000, .expected = .MOV },
        .{ .in = 0b10001010, .expected = .MOV },
        .{ .in = 0b10001011, .expected = .MOV },
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

fn decodeDirection(byte: u8) Direction {
    // Table 4-7: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    const mask_mode = 0b00000010;
    const masked = byte & mask_mode;
    if (masked == 0b00000000) return Direction.FromRegister;
    if (masked == 0b00000010) return Direction.ToRegister;
    unreachable;
}

const OperatesOn = enum {
    Byte,
    Word,
};

fn decodeOperatesOn(byte: u8) OperatesOn {
    // Table 4-7: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    const mask_mode = 0b00000001;
    const masked = byte & mask_mode;
    if (masked == 0b00000000) return OperatesOn.Byte;
    if (masked == 0b00000001) return OperatesOn.Word;
    unreachable;
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

fn lookupRegister(operand: OperatesOn, byte: u8) Register {
    switch (operand) {
        OperatesOn.Byte => {
            return lookupRegisterByte(byte);
        },
        OperatesOn.Word => {
            return lookupRegisterWord(byte);
        },
    }
    unreachable;
}

fn decodeREG(operand: OperatesOn, byte: u8) Register {
    const mask_reg = 0b00111000;
    const masked = byte & mask_reg;
    const shifted = masked >> 3;
    return lookupRegister(operand, shifted);
}

fn decodeRM(mode: Mode, operand: OperatesOn, byte: u8) Register {
    // Table 4-10: https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf
    assert(mode == Mode.Reg);
    const mask_reg = 0b00000111;
    const masked = byte & mask_reg;
    return lookupRegister(operand, masked);
}

test "decodeREG" {
    const TestCase = struct {
        in: u8,
        operand: OperatesOn,
        expected: Register,
    };
    const test_cases = [_]TestCase{
        // Byte
        .{ .in = 0b00000000, .operand = OperatesOn.Byte, .expected = .AL },
        .{ .in = 0b00001000, .operand = OperatesOn.Byte, .expected = .CL },
        .{ .in = 0b00010000, .operand = OperatesOn.Byte, .expected = .DL },
        .{ .in = 0b00011000, .operand = OperatesOn.Byte, .expected = .BL },
        .{ .in = 0b00100000, .operand = OperatesOn.Byte, .expected = .AH },
        .{ .in = 0b00101000, .operand = OperatesOn.Byte, .expected = .CH },
        .{ .in = 0b00110000, .operand = OperatesOn.Byte, .expected = .DH },
        .{ .in = 0b00111000, .operand = OperatesOn.Byte, .expected = .BH },
        // Word
        .{ .in = 0b00000000, .operand = OperatesOn.Word, .expected = .AX },
        .{ .in = 0b00001000, .operand = OperatesOn.Word, .expected = .CX },
        .{ .in = 0b00010000, .operand = OperatesOn.Word, .expected = .DX },
        .{ .in = 0b00011000, .operand = OperatesOn.Word, .expected = .BX },
        .{ .in = 0b00100000, .operand = OperatesOn.Word, .expected = .SP },
        .{ .in = 0b00101000, .operand = OperatesOn.Word, .expected = .BP },
        .{ .in = 0b00110000, .operand = OperatesOn.Word, .expected = .SI },
        .{ .in = 0b00111000, .operand = OperatesOn.Word, .expected = .DI },
    };

    for (test_cases) |case| {
        const actual = decodeREG(case.operand, case.in);
        if (actual != case.expected) {
            std.debug.print("0b{b}: want: {}, got: {}\n", .{ case.in, case.expected, actual });
            return error.TestExpectedEqual;
        }
    }
}

const Instruction = struct {
    Op: OpCode,
    Src: Register,
    Dst: Register,

    pub fn format(
        instruction: Instruction,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const op = std.enums.tagName(OpCode, instruction.Op);
        const srcReg = std.enums.tagName(Register, instruction.Src);
        const dstReg = std.enums.tagName(Register, instruction.Dst);

        return writer.print("{s} {s}, {s}", .{ op.?, dstReg.?, srcReg.? });
    }
};

fn decodeInstruction(byte_1: u8, byte_2: u8) Instruction {
    const dir = decodeDirection(byte_1);
    const operand = decodeOperatesOn(byte_1);

    const mode = decodeMode(byte_2);
    assert(mode == Mode.Reg);

    const reg = decodeREG(operand, byte_2);
    const rm = decodeRM(mode, operand, byte_2);

    return Instruction{
        .Op = decodeOpcode(byte_1),
        .Src = if (dir == Direction.FromRegister) reg else rm,
        .Dst = if (dir == Direction.ToRegister) reg else rm,
    };
}

test "decodeInstruction" {
    const TestCase = struct {
        in: [2]u8,
        expected: Instruction,
    };
    const test_cases = [_]TestCase{
        .{
            .in = [2]u8{ 0b10001001, 0b11000011 },
            .expected = .{
                .Op = OpCode.MOV,
                .Src = Register.AX,
                .Dst = Register.BX,
            },
        },
        .{
            .in = [2]u8{ 0b10001011, 0b11000011 },
            .expected = .{
                .Op = OpCode.MOV,
                .Src = Register.BX,
                .Dst = Register.AX,
            },
        },
        .{
            .in = [2]u8{ 0b10001000, 0b11000011 },
            .expected = .{
                .Op = OpCode.MOV,
                .Src = Register.AL,
                .Dst = Register.BL,
            },
        },
    };
    for (test_cases) |case| {
        const actual = decodeInstruction(case.in[0], case.in[1]);
        try std.testing.expectEqual(case.expected, actual);
    }
}

fn decode(alloc: std.mem.Allocator, bin: []const u8) ![]const u8 {
    var result = std.ArrayList(u8).init(alloc);
    defer result.deinit();

    var stream = std.io.fixedBufferStream(bin);
    var reader = stream.reader();
    try result.appendSlice("bits 16\n");

    var lineBuffer: [1024]u8 = undefined;
    while (true) {
        const byte_1 = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        // If we have a first byte, there has to be a second byte
        const byte_2 = try reader.readByte();

        const instruction = decodeInstruction(byte_1, byte_2);
        const instruction_string = try std.fmt.bufPrint(&lineBuffer, "{}\n", .{instruction});
        try result.appendSlice(std.ascii.lowerString(instruction_string, instruction_string));
    }

    return result.toOwnedSlice();
}

const data_dir = "data";

fn withoutComments(
    alloc: std.mem.Allocator,
    data: []const u8,
) !std.ArrayList(u8) {
    var result = std.ArrayList(u8).init(alloc);
    var it = std.mem.tokenizeScalar(u8, data, '\n');
    while (it.next()) |line| {
        if (std.mem.startsWith(u8, line, ";")) {
            continue;
        }
        try result.appendSlice(line);
        try result.append('\n');
    }
    return result;
}

test "withoutComments" {
    const listing_0037_asm = @embedFile(std.fmt.comptimePrint("{s}/listing_0037_single_register_mov.asm", .{data_dir}));
    const expected = "bits 16\nmov cx, bx\n";
    const actual = try withoutComments(std.testing.allocator, listing_0037_asm);
    defer actual.deinit();
    try std.testing.expectEqualSlices(u8, expected, actual.items);
}

// Part of homework for https://www.computerenhance.com/p/instruction-decoding-on-the-8086
test "Homework Part 1 - Decode Listing 37" {
    const alloc = std.testing.allocator;
    const listing_0037_bin = @embedFile(std.fmt.comptimePrint("{s}/listing_0037_single_register_mov", .{data_dir}));
    const listing_0037_asm = @embedFile(std.fmt.comptimePrint("{s}/listing_0037_single_register_mov.asm", .{data_dir}));

    const decompiled = try decode(alloc, listing_0037_bin[0..]);
    defer alloc.free(decompiled);

    const expected = try withoutComments(alloc, listing_0037_asm);
    defer expected.deinit();

    try std.testing.expectEqualSlices(u8, expected.items, decompiled);
}

// Part of homework for https://www.computerenhance.com/p/instruction-decoding-on-the-8086
test "Homework Part 1 - Decode Listing 38" {
    const alloc = std.testing.allocator;
    const listing_0037_bin = @embedFile(std.fmt.comptimePrint("{s}/listing_0038_many_register_mov", .{data_dir}));
    const listing_0037_asm = @embedFile(std.fmt.comptimePrint("{s}/listing_0038_many_register_mov.asm", .{data_dir}));

    const decompiled = try decode(alloc, listing_0037_bin[0..]);
    defer alloc.free(decompiled);

    const expected = try withoutComments(alloc, listing_0037_asm);
    defer expected.deinit();

    try std.testing.expectEqualSlices(u8, expected.items, decompiled);
}
