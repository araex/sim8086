const std = @import("std");

const decodeDirection = @import("fields.zig").decodeDirection;
const decodeImmediate = @import("operands.zig").decodeImmediate;
const decodeImmediateWithSignExtension = @import("operands.zig").decodeImmediateWithSignExtension;
const decodeJumpDestination = @import("operands.zig").decodeJumpDestination;
const decodeMode = @import("fields.zig").decodeMode;
const decodeOpcode = @import("opcodes.zig").decodeOpcode;
const decodeOperatesOn = @import("fields.zig").decodeOperatesOn;
const decodeRegister = @import("operands.zig").decodeRegister;
const decodeRM = @import("operands.zig").decodeRM;
const decodeSegmentRegister = @import("operands.zig").decodeSegmentRegister;
const Direction = @import("fields.zig").Direction;
const Displacement = @import("fields.zig").Displacement;
const DstType = @import("operands.zig").DstType;
const EffectiveAddressCalculation = @import("fields.zig").EffectiveAddressCalculation;
const getOpcodeSize = @import("opcodes.zig").getOpcodeSize;
const makeDst = @import("operands.zig").makeDst;
const makeSrc = @import("operands.zig").makeSrc;
const Memory = @import("operands.zig").Memory;
const Mode = @import("fields.zig").Mode;
const Opcode = @import("opcodes.zig").Opcode;
const OpcodeSize = @import("opcodes.zig").OpcodeSize;
const OperatesOn = @import("fields.zig").OperatesOn;
const Register = @import("operands.zig").Register;
const SrcType = @import("operands.zig").SrcType;

pub const Instruction = struct {
    op: Opcode,
    wide: OperatesOn,
    src: ?SrcType,
    dst: DstType,

    // Computes the total size of an instruction in bytes
    pub fn size(self: *const Instruction) u4 {
        const opsize = getOpcodeSize(self.op);
        var total_size = opsize;

        switch (self.dst) {
            .register => {},
            .memory => |mem| {
                switch (mem.calc) {
                    .DIRECT_ADDRESS => total_size += if (self.wide == .Byte) 1 else 2,
                    else => {
                        if (mem.displacement) |disp| {
                            switch (disp) {
                                .byte => total_size += 1,
                                .word => total_size += 2,
                            }
                        }
                    },
                }
            },
            .jump => total_size += 1, // 8-bit signed displacement
        }

        if (self.src) |src| {
            switch (src) {
                .register => {},
                .memory => |mem| {
                    switch (mem.calc) {
                        .DIRECT_ADDRESS => total_size += if (self.wide == .Byte) 1 else 2,
                        else => {
                            if (mem.displacement) |disp| {
                                switch (disp) {
                                    .byte => total_size += 1,
                                    .word => total_size += 2,
                                }
                            }
                        },
                    }
                },
                .immediate => |imm| {
                    switch (imm.value) {
                        .byte => total_size += 1,
                        .word => total_size += 2,
                    }
                },
            }
        }

        return total_size;
    }
};

pub fn makeInstruction(op: Opcode, operates_on: OperatesOn, dst: anytype, src: anytype) Instruction {
    return Instruction{
        .op = op,
        .wide = operates_on,
        .dst = makeDst(dst),
        .src = makeSrc(src),
    };
}

pub fn decodeInstruction(reader: *std.io.AnyReader) !Instruction {
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

fn decodeRegMemToFromReg(op: Opcode, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP D W | MOD REG R/M | [DISP] | D=direction, W=width
    const operates_on = decodeOperatesOn(0b00000001, byte_1);
    const dir = decodeDirection(0b00000010, byte_1);
    const mode = decodeMode(0b11000000, byte_2);
    const reg = decodeRegister(operates_on, 0b00111000, byte_2);
    const rm = try decodeRM(mode, operates_on, 0b00000111, byte_2, reader);

    return switch (dir) {
        Direction.FromRegister => makeInstruction(op, operates_on, rm, reg),
        Direction.ToRegister => makeInstruction(op, operates_on, reg, rm),
    };
}

fn decodeMovSegmentRegister(op: Opcode, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
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

fn decodeImmediateToRegister(op: Opcode, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP + reg | IMM | W encoded in opcode
    const operates_on = decodeOperatesOn(0b00001000, byte_1);
    const reg = decodeRegister(operates_on, 0b00000111, byte_1);
    const immediate = try decodeImmediate(operates_on, byte_2, reader);

    return makeInstruction(op, operates_on, reg, immediate);
}

fn decodeImmediateToRegMem(op: Opcode, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP W | MOD 000 R/M | [DISP] | IMM | W=width
    const operates_on = decodeOperatesOn(0b00000001, byte_1);
    const mode = decodeMode(0b11000000, byte_2);
    std.debug.assert(mode != Mode.Reg);
    const rm = try decodeRM(mode, operates_on, 0b00000111, byte_2, reader);

    // Special case for 8-bit displacement
    if (mode == Mode.Mem8BitDisplacement) {
        // Skip any padding bytes
        _ = try reader.readByte();
    }

    const immediate = try decodeImmediate(operates_on, try reader.readByte(), reader);
    return makeInstruction(op, operates_on, rm, immediate);
}

fn decodeMemToFromAccumulator(op: Opcode, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP W | ADDR-LO | [ADDR-HI] | W=width
    const operates_on = decodeOperatesOn(0b00000001, byte_1);
    const reg = if (operates_on == OperatesOn.Byte) Register.AL else Register.AX;

    // Read address (displacement)
    const displacement = if (operates_on == OperatesOn.Byte)
        Displacement{ .byte = byte_2 }
    else
        Displacement{ .word = @as(u16, byte_2) | (@as(u16, try reader.readByte()) << 8) };

    const mem = Memory{
        .calc = EffectiveAddressCalculation.DIRECT_ADDRESS,
        .displacement = displacement,
    };

    // Direction is encoded in the opcode itself
    return switch (op) {
        .mov_accumulator_to_mem => makeInstruction(op, operates_on, makeDst(mem), reg),
        .mov_mem_to_accumulator => makeInstruction(op, operates_on, reg, makeSrc(mem)),
        else => unreachable,
    };
}

fn decodeImmediateToRegMemWithExtensionInstruction(op: Opcode, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP S W | MOD 000 R/M | [DISP] | IMM | S=sign ext, W=width
    const operates_on = decodeOperatesOn(0b00000001, byte_1);
    const sign_extension = (byte_1 & 0b00000010) != 0;
    const mode = decodeMode(0b11000000, byte_2);
    const rm = try decodeRM(mode, operates_on, 0b00000111, byte_2, reader);

    // Special case for 8-bit displacement
    if (mode == Mode.Mem8BitDisplacement) {
        // Skip any padding bytes
        _ = try reader.readByte();
    }

    // Handle sign extension cases
    const immediate = if (sign_extension)
        try decodeImmediateWithSignExtension(try reader.readByte())
    else
        try decodeImmediate(operates_on, try reader.readByte(), reader);

    return makeInstruction(op, operates_on, rm, immediate);
}

fn decodeImmediateToAccumulatorInstruction(op: Opcode, byte_1: u8, byte_2: u8, reader: *std.io.AnyReader) !Instruction {
    // Format: | OP W | IMM | W=width
    const operates_on = decodeOperatesOn(0b00000001, byte_1);
    const reg = if (operates_on == OperatesOn.Byte) Register.AL else Register.AX;
    const immediate = try decodeImmediate(operates_on, byte_2, reader);

    return makeInstruction(op, operates_on, reg, immediate);
}

fn decodeJumpInstruction(op: Opcode, byte_2: u8) Instruction {
    // Format: | OP | DISP | 8-bit signed displacement
    // No need for pattern here as it's very simple
    const jump = decodeJumpDestination(byte_2);
    return makeInstruction(op, .Byte, jump, null);
}
