const std = @import("std");

const DstOperand = @import("instruction.zig").DstOperand;
const EffectiveAddressCalculation = @import("memory.zig").EffectiveAddressCalculation;
const Instruction = @import("instruction.zig").Instruction;
const Memory = @import("memory.zig").Memory;
const MemoryOperand = @import("memory.zig").MemoryOperand;
const Operation = @import("instruction.zig").Operation;
const RegisterName = @import("register.zig").RegisterName;
const Registers = @import("register.zig").Registers;
const size = @import("size.zig");
const SrcOperand = @import("instruction.zig").SrcOperand;
const Width = @import("instruction.zig").Width;

const SimError = error{
    InvalidOperands,
    MissingSrcOperand,
    NotImplemented,
};

pub const Simulator = struct {
    instructions: []const Instruction,
    cur_instruction_idx: usize,
    program_length: u16,

    registers: Registers,
    memory: Memory,

    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator, binary: []const u8) !Simulator {
        const decode = @import("decode.zig");

        var mem = Memory{};
        std.debug.assert(binary.len <= mem.data.len);
        for (0..binary.len) |i| {
            mem.data[i] = binary[i];
        }

        var instruction_list = try decode.instructionList(alloc, mem.data[0..binary.len]);

        return Simulator{
            .instructions = try instruction_list.toOwnedSlice(),
            .cur_instruction_idx = 0,
            .program_length = @intCast(binary.len),
            .registers = .{},
            .memory = mem,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Simulator) void {
        self.alloc.free(self.instructions);
    }

    pub fn step(self: *Simulator) !void {
        const i = self.getCurrentInstruction();
        self.registers.setWord(.IP, self.registers.getWord(.IP) + size.instruction(i));
        self.cur_instruction_idx += 1;
        if (toJump(i, self.registers)) |jump| {
            return self.execJump(jump);
        }
        if (toBinaryOp(i.op)) |bin_op| {
            const src = i.src orelse return SimError.MissingSrcOperand;
            switch (i.wide) {
                .Byte => try self.exec(bin_op, u8, i.dst, src),
                .Word => try self.exec(bin_op, u16, i.dst, src),
            }
            return;
        }

        std.log.err("Instruction '{s}'' not implemented ", .{@tagName(i.op)});
        return SimError.NotImplemented;
    }

    pub fn reset(self: *Simulator) void {
        self.cur_instruction_idx = 0;
        self.registers = .{};
    }

    pub fn isDone(self: *const Simulator) bool {
        return !self.isValidInstructionPointer();
    }

    pub fn getCurrentInstruction(self: *const Simulator) Instruction {
        std.debug.assert(isValidInstructionPointer(self));
        return self.instructions[self.cur_instruction_idx];
    }

    pub fn isValidInstructionPointer(self: *const Simulator) bool {
        return self.cur_instruction_idx < self.instructions.len;
    }
    fn execJump(self: *Simulator, jump: Jump) !void {
        if (jump.should_dec_cx) {
            self.registers.setWord(.CX, self.registers.getWord(.CX) - 1);
        }

        const signed_offset: i8 = jump.signed_offset;
        if (signed_offset == 0) {
            return;
        }
        const current_ip = self.registers.getWord(.IP);
        const new_ip_signed = @as(i32, @intCast(current_ip)) + signed_offset;
        if (new_ip_signed < 0 or new_ip_signed >= self.program_length) {
            return error.JumpedOutsideValidRange;
        }
        const new_ip = @as(u16, @intCast(new_ip_signed));
        self.registers.setWord(.IP, new_ip);

        // Should really consider storing the IP -> instruction. But good enough for now.
        var offset: u16 = 0;
        for (self.instructions, 0..) |instr, i| {
            if (offset == new_ip) {
                self.cur_instruction_idx = i;
                return;
            }
            offset += size.instruction(instr);
        }
        return error.JumpedOutsideValidRange;
    }

    fn exec(self: *Simulator, op: BinaryOperation, T: type, dst: DstOperand, src: SrcOperand) !void {
        const operand_1 = try self.getDstValue(T, dst);
        const operand_2 = try self.getSrcValue(T, src);
        const result = switch (op) {
            .Add => @addWithOverflow(operand_1, operand_2),
            .Sub => @subWithOverflow(operand_1, operand_2),
            .Cmp => @subWithOverflow(operand_1, operand_2),
            .Mov => {
                return self.setValue(T, dst, operand_2);
            },
        };

        if (op != .Cmp) {
            try self.setValue(T, dst, result[0]);
        }

        const msb = (@as(T, 1) << (@bitSizeOf(T) - 1));
        const op1_sign = (operand_1 & msb) != 0;
        const op2_sign = (operand_2 & msb) != 0;
        const result_sign = (result[0] & msb) != 0;

        self.registers.flags = .{
            .Carry = switch (op) {
                .Add => result[1] == 1, // unsigned overflow
                .Sub, .Cmp => operand_1 < operand_2,
                .Mov => unreachable,
            },
            // "the parity flag reflects the parity of only the least significant byte"
            // https://en.wikipedia.org/wiki/Parity_flag#x86_processors
            .Parity = @popCount(@as(u8, @truncate(result[0]))) % 2 == 0,
            .AuxCarry = switch (op) {
                .Add => ((operand_1 & 0xF) + (operand_2 & 0xF)) > 0xF,
                .Sub, .Cmp => (operand_1 & 0xF) < (operand_2 & 0xF),
                .Mov => unreachable,
            },
            .Zero = (result[0] == 0),
            .Sign = result_sign,
            .Overflow = switch (op) {
                .Add => (op1_sign == op2_sign) and (result_sign != op1_sign),
                .Sub, .Cmp => (op1_sign != op2_sign) and (result_sign != op1_sign),
                .Mov => unreachable,
            },
        };
    }

    fn getSrcValue(self: *const Simulator, T: type, src: SrcOperand) !T {
        switch (src) {
            .memory => |mem| return self.memory.getAs(T, calcMemAddress(mem, self.registers)),
            .immediate => |imm| return imm.as(T),
            .register => |reg_src| return switch (T) {
                u8 => self.registers.getByte(reg_src),
                u16 => self.registers.getWord(reg_src),
                else => unreachable,
            },
        }
    }

    fn getDstValue(self: *const Simulator, T: type, dst: DstOperand) !T {
        switch (dst) {
            .register => |reg| return switch (T) {
                u8 => self.registers.getByte(reg),
                u16 => self.registers.getWord(reg),
                else => unreachable,
            },
            .memory => |mem| return self.memory.getAs(T, calcMemAddress(mem, self.registers)),
            .jump => return error.InvalidOperands,
        }
    }

    fn setValue(self: *Simulator, T: type, dst: DstOperand, value: T) !void {
        switch (dst) {
            .register => |reg| return self.registers.setAs(T, reg, value),
            .memory => |mem| return self.memory.setAs(T, calcMemAddress(mem, self.registers), value),
            .jump => return error.InvalidOperands,
        }
    }
};

const BinaryOperation = enum { Add, Sub, Cmp, Mov };

fn toBinaryOp(opcode: Operation) ?BinaryOperation {
    switch (opcode) {
        .add_rm_with_r_to_either, .add_imm_to_rm, .add_imm_to_acc => return .Add,
        .sub_rm_and_r_to_either, .sub_imm_to_rm, .sub_imm_to_acc => return .Sub,
        .cmp_rm_with_r, .cmp_imm_with_rm, .cmp_imm_with_acc => return .Cmp,
        .mov_imm_to_r, .mov_rm_to_from_r, .mov_sr_to_rm, .mov_rm_to_sr, .mov_imm_to_rm, .mov_mem_to_accumulator, .mov_accumulator_to_mem => return .Mov,
        else => return null,
    }
}

const Jump = struct {
    signed_offset: i8 = 0,
    should_dec_cx: bool = false,
};

fn toJump(instruction: Instruction, regs: Registers) ?Jump {
    const flags = regs.flags;

    const offset = switch (instruction.dst) {
        .jump => |jump| jump.increment,
        else => return null,
    };

    switch (instruction.op) {
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
            return Jump{
                .signed_offset = if (cx > 1 and !flags.Zero) offset else 0,
                .should_dec_cx = true,
            };
        },
        .loopz_loope => {
            const cx = regs.getWord(.CX);
            return Jump{
                .signed_offset = if (cx > 1 and flags.Zero) offset else 0,
                .should_dec_cx = true,
            };
        },
        .loop => {
            const cx = regs.getWord(.CX);
            return Jump{
                .signed_offset = if (cx > 1) offset else 0,
                .should_dec_cx = true,
            };
        },
        .jcxz => if (regs.getWord(.CX) == 0) return Jump{ .signed_offset = offset },
        else => return null,
    }

    return Jump{ .signed_offset = 0 };
}

fn calcMemAddress(mem: MemoryOperand, regs: Registers) u16 {
    const offset = if (mem.displacement) |d| switch (d) {
        .byte => d.byte,
        .word => d.word,
    } else 0;
    switch (mem.calc) {
        .BX_PLUS_SI => return regs.getWord(.BX) + regs.getWord(.SI) + offset,
        .BX_PLUS_DI => return regs.getWord(.BX) + regs.getWord(.DI) + offset,
        .BP_PLUS_SI => return regs.getWord(.BP) + regs.getWord(.SI) + offset,
        .BP_PLUS_DI => return regs.getWord(.BP) + regs.getWord(.DI) + offset,
        .SI => return regs.getWord(.SI) + offset,
        .DI => return regs.getWord(.DI) + offset,
        .BP => return regs.getWord(.BP) + offset,
        .DIRECT_ADDRESS => return offset,
        .BX => return regs.getWord(.BX) + offset,
    }
}
