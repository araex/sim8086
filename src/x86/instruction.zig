const std = @import("std");

const EffectiveAddressCalculation = @import("memory.zig").EffectiveAddressCalculation;
const MemoryOperand = @import("memory.zig").MemoryOperand;
const RegisterName = @import("register.zig").RegisterName;

pub const Instruction = struct {
    op: Operation,
    wide: Width,
    src: ?SrcOperand,
    dst: DstOperand,
};

pub const Operation = enum { Unknown, mov_rm_to_from_r, mov_imm_to_rm, mov_imm_to_r, mov_mem_to_accumulator, mov_accumulator_to_mem, mov_rm_to_sr, mov_sr_to_rm, add_rm_with_r_to_either, add_imm_to_rm, add_imm_to_acc, sub_rm_and_r_to_either, sub_imm_to_rm, sub_imm_to_acc, cmp_rm_with_r, cmp_imm_with_rm, cmp_imm_with_acc, jo, jno, jb_jnae, jnb_jae, je_jz, jne_jnz, jbe_jna, jnbe_ja, js, jns, jp_jpe, jnp_jpo, jl_jnge, jnl_jge, jle_jng, jnle_jg, loopnz_loopne, loopz_loope, loop, jcxz };

pub const Width = enum {
    Byte,
    Word,
};

pub const SrcOperand = union(enum) {
    register: RegisterName,
    immediate: ImmediateOperand,
    memory: MemoryOperand,

    pub fn as(self: SrcOperand, t: type) !t {
        switch (t) {
            u8, u16 => {
                const imm = try self.asImmediateOperand();
                return imm.as(t);
            },
            ImmediateOperand => return self.asImmediateOperand(),
            RegisterName => return self.asRegister(),
            MemoryOperand => return self.asMemory(),
            else => return error.InvalidAcces,
        }
    }

    fn asImmediateOperand(self: SrcOperand) !ImmediateOperand {
        switch (self) {
            .immediate => |imm| return imm,
            else => return error.InvalidAcces,
        }
    }

    fn asRegister(self: SrcOperand) !RegisterName {
        switch (self) {
            .register => |r| return r,
            else => return error.InvalidAcces,
        }
    }

    fn asMemory(self: SrcOperand) !MemoryOperand {
        switch (self) {
            .memory => |m| return m,
            else => return error.InvalidAcces,
        }
    }
};

pub const DstOperand = union(enum) {
    register: RegisterName,
    memory: MemoryOperand,
    jump: JumpDestination,
};

pub const JumpDestination = struct {
    increment: i8,
};

pub const AddressingMode = enum(u3) {
    Mem,
    Mem8BitDisplacement,
    Mem16BitDisplacement,
    Reg,
};

pub const Direction = enum {
    ToRegister, // Instruction destination is specified in REG field
    FromRegister, // Instruction source is specified in REG field
};

pub const RegOrMem = union(enum) {
    register: RegisterName,
    memory: MemoryOperand,
};

pub const ImmediateOperand = struct {
    value: union(enum) {
        byte: u8,
        word: u16,
    },

    pub fn as(self: ImmediateOperand, t: type) !t {
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

pub fn makeInstruction(op: Operation, width: Width, dst: anytype, src: anytype) Instruction {
    return Instruction{
        .op = op,
        .wide = width,
        .dst = makeDst(dst),
        .src = makeSrc(src),
    };
}

pub fn makeSrc(val: anytype) ?SrcOperand {
    switch (@TypeOf(val)) {
        SrcOperand => return val,
        ?SrcOperand => return val,
        RegisterName => {
            return SrcOperand{
                .register = val,
            };
        },
        MemoryOperand => {
            return SrcOperand{
                .memory = val,
            };
        },
        RegOrMem => {
            switch (val) {
                .register => |r| return makeSrc(r),
                .memory => |m| return makeSrc(m),
            }
        },
        ImmediateOperand => {
            return SrcOperand{
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

pub fn makeDst(val: anytype) DstOperand {
    switch (@TypeOf(val)) {
        DstOperand => return val,
        RegisterName => {
            return DstOperand{
                .register = val,
            };
        },
        MemoryOperand => {
            return DstOperand{
                .memory = val,
            };
        },
        RegOrMem => {
            switch (val) {
                .register => |r| return makeDst(r),
                .memory => |m| return makeDst(m),
            }
        },
        JumpDestination => {
            return DstOperand{
                .jump = val,
            };
        },
        else => {
            std.log.err("unhandled type '{}' with value '{any}'", .{ @TypeOf(val), val });
        },
    }
}

pub fn makeImmediate(val: anytype) ImmediateOperand {
    return ImmediateOperand{
        .value = switch (@TypeOf(val)) {
            u8 => .{ .byte = val },
            u16 => .{ .word = val },
            ImmediateOperand => return val,
            else => unreachable,
        },
    };
}
