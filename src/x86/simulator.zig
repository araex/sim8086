const assert = @import("std").debug.assert;
const std = @import("std");

const Instruction = @import("instruction.zig").Instruction;
const Opcode = @import("opcodes.zig").Opcode;
const OperatesOn = @import("fields.zig").OperatesOn;
const RegisterType = @import("operands.zig").Register;
const SrcType = @import("operands.zig").SrcType;

const InstructionError = error{
    InvalidOperands,
    MissingSrcOperand,
    NotImplemented,
};

pub const Simulator = struct {
    instructions: []const Instruction,
    cur_instruction_idx: usize,
    program_length: u16,

    registers: Registers,

    pub fn init(instructions: []const Instruction) !Simulator {
        var byte_count: u16 = 0;
        for (instructions) |i| {
            byte_count += i.size();
        }
        return Simulator{
            .instructions = instructions,
            .cur_instruction_idx = 0,
            .registers = .{},
            .program_length = byte_count,
        };
    }

    pub fn step(self: *Simulator) !void {
        const i = self.getCurrentInstruction();
        self.registers.setWord(.IP, self.registers.getWord(.IP) + i.size());
        self.cur_instruction_idx += 1;
        if (toJump(i, self.registers)) |jump| {
            return self.execJump(jump);
        }
        if (opcodeToBinaryOp(i.op)) |bin_op| {
            const src = i.src orelse return InstructionError.MissingSrcOperand;
            switch (i.dst) {
                .jump => return InstructionError.InvalidOperands,
                .memory => return InstructionError.NotImplemented,
                .register => |reg_dst| switch (i.wide) {
                    .Byte => try self.exec(bin_op, u8, reg_dst, src),
                    .Word => try self.exec(bin_op, u16, reg_dst, src),
                },
            }
            return;
        }

        switch (i.op) {
            .mov_imm_to_r, .mov_rm_to_from_r, .mov_sr_to_rm, .mov_rm_to_sr => {
                const src = i.src orelse return InstructionError.MissingSrcOperand;
                switch (i.dst) {
                    .jump => return InstructionError.InvalidOperands,
                    .memory => return InstructionError.NotImplemented,
                    .register => |reg_dst| switch (i.wide) {
                        .Byte => self.registers.setByte(reg_dst, try getValue(u8, self.registers, src)),
                        .Word => self.registers.setWord(reg_dst, try getValue(u16, self.registers, src)),
                    },
                }
            },
            else => {
                std.log.err("Instruction '{s}'' not implemented ", .{@tagName(i.op)});
                return InstructionError.NotImplemented;
            },
        }
    }

    pub fn reset(self: *Simulator) void {
        self.cur_instruction_idx = 0;
        self.registers = .{};
    }

    pub fn isDone(self: *const Simulator) bool {
        return !self.isValidInstructionPointer();
    }

    pub fn getCurrentInstruction(self: *const Simulator) Instruction {
        assert(isValidInstructionPointer(self));
        return self.instructions[self.cur_instruction_idx];
    }

    fn isValidInstructionPointer(self: *const Simulator) bool {
        return self.cur_instruction_idx < self.instructions.len;
    }
    fn execJump(self: *Simulator, jump: Jump) !void {
        const signed_offset: i8 = jump.signed_offset;
        if (signed_offset == 0) {
            return;
        }
        const current_ip = self.registers.getWord(.IP);

        //  @as(i32, signed_offset)
        const new_ip_signed = @as(i32, @intCast(current_ip)) + signed_offset;
        if (new_ip_signed < 0 or new_ip_signed > self.program_length) {
            return error.JumpedOutsideValidRange;
        }

        // Apply the jump offset
        const new_ip = @as(u16, @intCast(new_ip_signed));
        self.registers.setWord(.IP, new_ip);

        // Find the instruction that corresponds to this new IP
        var idx: usize = self.cur_instruction_idx;
        var ip_tracker: i32 = current_ip;
        while (ip_tracker != new_ip) {
            if (signed_offset < 0) {
                idx -= 1;
                ip_tracker -= self.instructions[idx].size();
            } else {
                idx += 1;
                ip_tracker += self.instructions[idx].size();
            }

            if (ip_tracker < 0 or ip_tracker > self.program_length) {
                return error.JumpedOutsideValidRange;
            }
        }

        self.cur_instruction_idx = idx;
    }

    fn exec(self: *Simulator, op: BinaryOperation, T: type, dst: RegisterType, src: SrcType) !void {
        const operand_1 = self.registers.getAs(T, dst);
        const operand_2 = try getValue(T, self.registers, src);
        const result = switch (op) {
            .Add => @addWithOverflow(operand_1, operand_2),
            .Sub => @subWithOverflow(operand_1, operand_2),
            .Cmp => @subWithOverflow(operand_1, operand_2),
        };

        if (op != .Cmp) {
            self.registers.setAs(T, dst, result[0]);
        }

        const msb = (@as(T, 1) << (@bitSizeOf(T) - 1));
        const op1_sign = (operand_1 & msb) != 0;
        const op2_sign = (operand_2 & msb) != 0;
        const result_sign = (result[0] & msb) != 0;

        self.registers.flags = .{
            .Carry = switch (op) {
                .Add => result[1] == 1, // unsigned overflow
                .Sub, .Cmp => operand_1 < operand_2,
            },
            // "the parity flag reflects the parity of only the least significant byte"
            // https://en.wikipedia.org/wiki/Parity_flag#x86_processors
            .Parity = @popCount(@as(u8, @truncate(result[0]))) % 2 == 0,
            .AuxCarry = switch (op) {
                .Add => ((operand_1 & 0xF) + (operand_2 & 0xF)) > 0xF,
                .Sub, .Cmp => (operand_1 & 0xF) < (operand_2 & 0xF),
            },
            .Zero = (result[0] == 0),
            .Sign = result_sign,
            .Overflow = switch (op) {
                .Add => (op1_sign == op2_sign) and (result_sign != op1_sign),
                .Sub, .Cmp => (op1_sign != op2_sign) and (result_sign != op1_sign),
            },
        };
    }
};

const BinaryOperation = enum { Add, Sub, Cmp };

fn opcodeToBinaryOp(opcode: Opcode) ?BinaryOperation {
    switch (opcode) {
        .add_rm_with_r_to_either, .add_imm_to_rm, .add_imm_to_acc => return .Add,
        .sub_rm_and_r_to_either, .sub_imm_to_rm, .sub_imm_to_acc => return .Sub,
        .cmp_rm_with_r, .cmp_imm_with_rm, .cmp_imm_with_acc => return .Cmp,
        else => return null,
    }
}

const Jump = struct {
    signed_offset: i8,
};

fn toJump(instruction: Instruction, regs: Registers) ?Jump {
    const flags = regs.flags;

    const offset = switch (instruction.dst) {
        .jump => |jump| jump.increment,
        else => return null,
    };

    switch (instruction.op) {
        // Conditional jumps based on flags
        .jo => if (flags.Overflow) return Jump{ .signed_offset = offset },
        .jno => if (!flags.Overflow) return Jump{ .signed_offset = offset },
        .jb_jnae => if (flags.Carry) return Jump{ .signed_offset = offset },
        .jnb_jae => if (!flags.Carry) return Jump{ .signed_offset = offset },
        .je_jz => if (flags.Zero) return Jump{ .signed_offset = offset },
        .jne_jnz => if (!flags.Zero) return Jump{ .signed_offset = offset },
        .jbe_jna => if (flags.Carry or flags.Zero) return Jump{ .signed_offset = offset },
        .jnbe_ja => if (!flags.Carry and !flags.Zero) return Jump{ .signed_offset = offset },
        .js => if (flags.Sign) return Jump{ .signed_offset = offset },
        .jns => if (!flags.Sign) return Jump{ .signed_offset = offset },
        .jp_jpe => if (flags.Parity) return Jump{ .signed_offset = offset },
        .jnp_jpo => if (!flags.Parity) return Jump{ .signed_offset = offset },
        .jl_jnge => if (flags.Sign != flags.Overflow) return Jump{ .signed_offset = offset },
        .jnl_jge => if (flags.Sign == flags.Overflow) return Jump{ .signed_offset = offset },
        .jle_jng => if (flags.Zero or (flags.Sign != flags.Overflow)) return Jump{ .signed_offset = offset },
        .jnle_jg => if (!flags.Zero and (flags.Sign == flags.Overflow)) return Jump{ .signed_offset = offset },
        .loopnz_loopne => {
            const cx = regs.getWord(.CX);
            if (cx > 1 and !flags.Zero) return Jump{ .signed_offset = offset };
        },
        .loopz_loope => {
            const cx = regs.getWord(.CX);
            if (cx > 1 and flags.Zero) return Jump{ .signed_offset = offset };
        },
        .loop => {
            const cx = regs.getWord(.CX);
            if (cx > 1) return Jump{ .signed_offset = offset };
        },
        .jcxz => if (regs.getWord(.CX) == 0) return Jump{ .signed_offset = offset },
        else => return null, // no jump condition met
    }

    return Jump{ .signed_offset = 0 };
}

fn getValue(T: type, regs: Registers, src: SrcType) !T {
    switch (src) {
        .memory => return InstructionError.NotImplemented,
        .immediate => |imm| return imm.as(T),
        .register => |reg_src| return switch (T) {
            u8 => regs.getByte(reg_src),
            u16 => regs.getWord(reg_src),
            else => unreachable,
        },
    }
}

pub const Registers = struct {
    // [AX_LO, AX_HI, BX_LO, BX_HI, CX_LO, CX_HI, DX_LO, DX_HI, SP_LO, SP_HI, BP_LO, BP_HI, SI_LO, SI_HI, DI_LO, DI_HI, ES, CS, SS, DS, IP]
    data: [26]u8 = [_]u8{0} ** 26,

    flags: struct {
        Carry: bool = false,
        Parity: bool = false,
        AuxCarry: bool = false,
        Zero: bool = false,
        Sign: bool = false,
        Overflow: bool = false,
    } = .{},

    // Register byte offsets
    const AX_OFFSET = 0;
    const BX_OFFSET = 2;
    const CX_OFFSET = 4;
    const DX_OFFSET = 6;
    const SP_OFFSET = 8;
    const BP_OFFSET = 10;
    const SI_OFFSET = 12;
    const DI_OFFSET = 14;
    const ES_OFFSET = 16;
    const CS_OFFSET = 18;
    const SS_OFFSET = 20;
    const DS_OFFSET = 22;
    const IP_OFFSET = 24;

    // Set register value based on operation type
    pub fn set(self: *Registers, operates_on: OperatesOn, dst: RegisterType, value: u16) void {
        switch (operates_on) {
            .Byte => self.setByte(dst, @intCast(value & 0xFF)),
            .Word => self.setWord(dst, value),
        }
    }

    // Set 8-bit register
    pub fn setByte(self: *Registers, dst: RegisterType, value: u8) void {
        switch (dst) {
            .AL => self.data[AX_OFFSET] = value,
            .AH => self.data[AX_OFFSET + 1] = value,
            .BL => self.data[BX_OFFSET] = value,
            .BH => self.data[BX_OFFSET + 1] = value,
            .CL => self.data[CX_OFFSET] = value,
            .CH => self.data[CX_OFFSET + 1] = value,
            .DL => self.data[DX_OFFSET] = value,
            .DH => self.data[DX_OFFSET + 1] = value,
            .AX => self.data[AX_OFFSET] = value,
            .BX => self.data[BX_OFFSET] = value,
            .CX => self.data[CX_OFFSET] = value,
            .DX => self.data[DX_OFFSET] = value,
            .SP => self.data[SP_OFFSET] = value,
            .BP => self.data[BP_OFFSET] = value,
            .SI => self.data[SI_OFFSET] = value,
            .DI => self.data[DI_OFFSET] = value,
            .ES => self.data[ES_OFFSET] = value,
            .CS => self.data[CS_OFFSET] = value,
            .SS => self.data[SS_OFFSET] = value,
            .DS => self.data[DS_OFFSET] = value,
            .IP => self.data[IP_OFFSET] = value,
        }
    }

    // Set 16-bit register
    pub fn setWord(self: *Registers, dst: RegisterType, value: u16) void {
        assert(isWideRegister(dst));
        const lo: u8 = @intCast(value & 0xFF);
        const hi: u8 = @intCast((value >> 8) & 0xFF);

        switch (dst) {
            .AX => {
                self.data[AX_OFFSET] = lo;
                self.data[AX_OFFSET + 1] = hi;
            },
            .BX => {
                self.data[BX_OFFSET] = lo;
                self.data[BX_OFFSET + 1] = hi;
            },
            .CX => {
                self.data[CX_OFFSET] = lo;
                self.data[CX_OFFSET + 1] = hi;
            },
            .DX => {
                self.data[DX_OFFSET] = lo;
                self.data[DX_OFFSET + 1] = hi;
            },
            .SP => {
                self.data[SP_OFFSET] = lo;
                self.data[SP_OFFSET + 1] = hi;
            },
            .BP => {
                self.data[BP_OFFSET] = lo;
                self.data[BP_OFFSET + 1] = hi;
            },
            .SI => {
                self.data[SI_OFFSET] = lo;
                self.data[SI_OFFSET + 1] = hi;
            },
            .DI => {
                self.data[DI_OFFSET] = lo;
                self.data[DI_OFFSET + 1] = hi;
            },
            .ES => {
                self.data[ES_OFFSET] = lo;
                self.data[ES_OFFSET + 1] = hi;
            },
            .CS => {
                self.data[CS_OFFSET] = lo;
                self.data[CS_OFFSET + 1] = hi;
            },
            .SS => {
                self.data[SS_OFFSET] = lo;
                self.data[SS_OFFSET + 1] = hi;
            },
            .DS => {
                self.data[DS_OFFSET] = lo;
                self.data[DS_OFFSET + 1] = hi;
            },
            .IP => {
                self.data[IP_OFFSET] = lo;
                self.data[IP_OFFSET + 1] = hi;
            },
            else => unreachable,
        }
    }

    pub fn setAs(self: *Registers, T: type, dst: RegisterType, value: anytype) void {
        switch (T) {
            u8 => return self.setByte(dst, @as(T, value)),
            u16 => return self.setWord(dst, @as(T, value)),
            else => unreachable,
        }
    }

    // Get 8-bit register value
    pub fn getByte(self: *const Registers, reg: RegisterType) u8 {
        switch (reg) {
            .AL => return self.data[AX_OFFSET],
            .AH => return self.data[AX_OFFSET + 1],
            .BL => return self.data[BX_OFFSET],
            .BH => return self.data[BX_OFFSET + 1],
            .CL => return self.data[CX_OFFSET],
            .CH => return self.data[CX_OFFSET + 1],
            .DL => return self.data[DX_OFFSET],
            .DH => return self.data[DX_OFFSET + 1],
            .AX => return self.data[AX_OFFSET],
            .BX => return self.data[BX_OFFSET],
            .CX => return self.data[CX_OFFSET],
            .DX => return self.data[DX_OFFSET],
            .SP => return self.data[SP_OFFSET],
            .BP => return self.data[BP_OFFSET],
            .SI => return self.data[SI_OFFSET],
            .DI => return self.data[DI_OFFSET],
            .ES => return self.data[ES_OFFSET],
            .CS => return self.data[CS_OFFSET],
            .SS => return self.data[SS_OFFSET],
            .DS => return self.data[DS_OFFSET],
            .IP => return self.data[IP_OFFSET],
        }
    }

    // Get 16-bit register value
    pub fn getWord(self: *const Registers, reg: RegisterType) u16 {
        assert(isWideRegister(reg));
        var offset: usize = 0;

        switch (reg) {
            .AX => offset = AX_OFFSET,
            .BX => offset = BX_OFFSET,
            .CX => offset = CX_OFFSET,
            .DX => offset = DX_OFFSET,
            .SP => offset = SP_OFFSET,
            .BP => offset = BP_OFFSET,
            .SI => offset = SI_OFFSET,
            .DI => offset = DI_OFFSET,
            .ES => offset = ES_OFFSET,
            .CS => offset = CS_OFFSET,
            .SS => offset = SS_OFFSET,
            .DS => offset = DS_OFFSET,
            .IP => offset = IP_OFFSET,
            else => unreachable,
        }

        return @as(u16, self.data[offset]) | (@as(u16, self.data[offset + 1]) << 8);
    }

    pub fn getAs(self: *const Registers, T: type, reg: RegisterType) T {
        switch (T) {
            u8 => return self.getByte(reg),
            u16 => return self.getWord(reg),
            else => unreachable,
        }
    }
};

fn isByteRegister(reg: RegisterType) bool {
    switch (reg) {
        .AL, .AH, .BL, .BH, .CL, .CH, .DL, .DH => return true,
        else => return false,
    }
}

fn isWideRegister(reg: RegisterType) bool {
    return !isByteRegister(reg);
}
