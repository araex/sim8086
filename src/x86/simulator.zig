const assert = @import("std").debug.assert;
const std = @import("std");

const Instruction = @import("instruction.zig").Instruction;
const OperatesOn = @import("fields.zig").OperatesOn;
const RegisterType = @import("operands.zig").Register;
const SrcType = @import("operands.zig").SrcType;

const InstructionError = error{
    InvalidOperands,
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
        switch (i.op) {
            .mov_imm_to_r => {
                const src = i.src orelse return InstructionError.InvalidOperands;
                switch (i.dst) {
                    .jump => return InstructionError.InvalidOperands,
                    .memory => return InstructionError.NotImplemented,
                    .register => |reg| switch (i.wide) {
                        .Byte => self.registers.setByte(reg, try src.as(u8)),
                        .Word => self.registers.setWord(reg, try src.as(u16)),
                    },
                }
            },
            else => return InstructionError.NotImplemented,
        }
        self.cur_instruction += 1;
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
};

pub const Registers = struct {
    // Register data is stored in a flat byte array
    // The layout is [AX_LO, AX_HI, BX_LO, BX_HI, CX_LO, CX_HI, DX_LO, DX_HI, SP_LO, SP_HI, BP_LO, BP_HI, SI_LO, SI_HI, DI_LO, DI_HI]
    data: [16]u8 = [_]u8{0} ** 16,

    // Register byte offsets
    const AX_OFFSET = 0;
    const BX_OFFSET = 2;
    const CX_OFFSET = 4;
    const DX_OFFSET = 6;
    const SP_OFFSET = 8;
    const BP_OFFSET = 10;
    const SI_OFFSET = 12;
    const DI_OFFSET = 14;

    // Set register value based on operation type
    pub fn set(self: *Registers, operates_on: OperatesOn, dst: RegisterType, value: u16) void {
        switch (operates_on) {
            .Byte => self.setByte(dst, @intCast(value & 0xFF)),
            .Word => self.setWord(dst, value),
        }
    }

    // Set 8-bit register
    pub fn setByte(self: *Registers, dst: RegisterType, value: u8) void {
        assert(isByteRegister(dst));
        switch (dst) {
            .AL => self.data[AX_OFFSET] = value,
            .AH => self.data[AX_OFFSET + 1] = value,
            .BL => self.data[BX_OFFSET] = value,
            .BH => self.data[BX_OFFSET + 1] = value,
            .CL => self.data[CX_OFFSET] = value,
            .CH => self.data[CX_OFFSET + 1] = value,
            .DL => self.data[DX_OFFSET] = value,
            .DH => self.data[DX_OFFSET + 1] = value,
            else => unreachable,
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
            else => unreachable,
        }
    }

    // Get 8-bit register value
    pub fn getByte(self: *const Registers, reg: RegisterType) u8 {
        assert(isByteRegister(reg));
        switch (reg) {
            .AL => return self.data[AX_OFFSET],
            .AH => return self.data[AX_OFFSET + 1],
            .BL => return self.data[BX_OFFSET],
            .BH => return self.data[BX_OFFSET + 1],
            .CL => return self.data[CX_OFFSET],
            .CH => return self.data[CX_OFFSET + 1],
            .DL => return self.data[DX_OFFSET],
            .DH => return self.data[DX_OFFSET + 1],
            else => unreachable,
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
            else => unreachable,
        }

        return @as(u16, self.data[offset]) | (@as(u16, self.data[offset + 1]) << 8);
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
