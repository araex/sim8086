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
    cur_instruction: usize,

    registers: Registers,

    pub fn init(instructions: []const Instruction) !Simulator {
        return Simulator{
            .instructions = instructions,
            .cur_instruction = 0,
            .registers = .{},
        };
    }

    pub fn step(self: *Simulator) !void {
        const i = self.getCurrentInstruction();
        defer self.cur_instruction += 1;
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
        self.cur_instruction = 0;
        self.registers = .{};
    }

    pub fn isDone(self: *const Simulator) bool {
        return !self.isValidInstructionPointer();
    }

    pub fn getCurrentInstruction(self: *const Simulator) Instruction {
        assert(isValidInstructionPointer(self));
        return self.instructions[self.cur_instruction];
    }

    fn isValidInstructionPointer(self: *const Simulator) bool {
        return self.cur_instruction < self.instructions.len;
    }

    const BinaryOperation = enum { Add, Sub, Cmp };

    fn opcodeToBinaryOp(opcode: Opcode) ?BinaryOperation {
        switch (opcode) {
            .add_rm_with_r_to_either, .add_imm_to_rm, .add_imm_to_acc => return .Add,
            .sub_rm_and_r_to_either, .sub_imm_to_rm, .sub_imm_to_acc => return .Sub,
            .cmp_rm_with_r, .cmp_imm_with_rm, .cmp_imm_with_acc => return .Cmp,
            else => return null,
        }
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

        const msb = (@as(T, 0) >> 1);
        self.registers.flags = .{
            // "the parity flag reflects the parity of only the least significant byte"
            // https://en.wikipedia.org/wiki/Parity_flag#x86_processors
            .Parity = @popCount(@as(u8, @truncate(result[0]))) % 2 == 0,
            .Zero = (result[0] == 0),
            .Sign = (result[0] & msb) != 0,
            .Overflow = (result[1] == 1),
        };
    }
};

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
    // Register data is stored in a flat byte array
    // The layout is [AX_LO, AX_HI, BX_LO, BX_HI, CX_LO, CX_HI, DX_LO, DX_HI, SP_LO, SP_HI, BP_LO, BP_HI, SI_LO, SI_HI, DI_LO, DI_HI, ES, CS, SS, DS]
    data: [24]u8 = [_]u8{0} ** 24,

    flags: struct {
        Parity: bool = false,
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
