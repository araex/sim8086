const EffectiveAddressCalculation = @import("memory.zig").EffectiveAddressCalculation;
const Instruction = @import("instruction.zig").Instruction;
const MemoryOperand = @import("memory.zig").MemoryOperand;
const Register = @import("register.zig");
const SimError = @import("simulator.zig").SimError;

pub fn estimate(instruction: Instruction) !u8 {
    // Table 2-20 https://archive.org/details/bitsavers_intel80869lyUsersManualOct79_62967963/page/n72/mode/1up?view=theater
    switch (instruction.op) {
        .add_imm_to_acc, .add_imm_to_rm, .add_rm_with_r_to_either, .sub_imm_to_acc, .sub_imm_to_rm, .sub_rm_and_r_to_either => {
            const src = instruction.src orelse return SimError.MissingSrcOperand;
            switch (instruction.dst) {
                .register => {
                    switch (src) {
                        .register => return 3,
                        .immediate => return 4,
                        .memory => |src_mem| return 9 + eac(src_mem) + transfers(src_mem, 1),
                    }
                },
                .memory => |dst_mem| {
                    switch (src) {
                        .register => return 16 + eac(dst_mem) + transfers(dst_mem, 2),
                        .immediate => return 17 + eac(dst_mem) + transfers(dst_mem, 2),
                        .memory => return SimError.InvalidOperands,
                    }
                },
                .jump => return SimError.InvalidOperands,
            }
        },
        .cmp_imm_with_acc, .cmp_imm_with_rm, .cmp_rm_with_r => {
            const src = instruction.src orelse return SimError.MissingSrcOperand;
            switch (instruction.dst) {
                .register => {
                    switch (src) {
                        .register => return 3,
                        .immediate => return 4,
                        .memory => |src_mem| return 9 + eac(src_mem) + transfers(src_mem, 1),
                    }
                },
                .memory => |dst_mem| {
                    switch (src) {
                        .register => return 9 + eac(dst_mem) + transfers(dst_mem, 1),
                        .immediate => return 10 + eac(dst_mem) + transfers(dst_mem, 1),
                        .memory => return SimError.InvalidOperands,
                    }
                },
                .jump => return SimError.InvalidOperands,
            }
        },
        .mov_accumulator_to_mem, .mov_imm_to_r, .mov_imm_to_rm, .mov_mem_to_accumulator, .mov_rm_to_from_r, .mov_rm_to_sr, .mov_sr_to_rm => {
            const src = instruction.src orelse return SimError.MissingSrcOperand;
            switch (instruction.dst) {
                .register => |dst_reg| {
                    switch (src) {
                        .register => return 2,
                        .immediate => return 4,
                        .memory => |src_mem| {
                            if (Register.isAccumulator(dst_reg)) {
                                return 10 + transfers(src_mem, 1);
                            }
                            return 8 + eac(src_mem) + transfers(src_mem, 1);
                        },
                    }
                },
                .memory => |dst_mem| {
                    switch (src) {
                        .register => |src_reg| {
                            if (Register.isAccumulator(src_reg)) {
                                return 10 + transfers(dst_mem, 1);
                            }
                            return 9 + eac(dst_mem) + transfers(dst_mem, 1);
                        },
                        .immediate => return 10 + eac(dst_mem) + transfers(dst_mem, 1),
                        .memory => return SimError.InvalidOperands,
                    }
                },
                .jump => return SimError.InvalidOperands,
            }
        },
        else => return SimError.NotImplemented,
    }
}

fn eac(op: MemoryOperand) u8 {
    const displacement_cost: u8 = blk: {
        if (op.displacement) |d| {
            switch (d) {
                .byte => |b| {
                    break :blk if (b == 0) 0 else 4;
                },
                .word => |w| {
                    break :blk if (w == 0) 0 else 4;
                },
            }
        }
        break :blk 0;
    };

    switch (op.calc) {
        .DIRECT_ADDRESS => return 6,
        .SI, .DI, .BP, .BX => return 5 + displacement_cost,
        .BP_PLUS_DI, .BX_PLUS_SI => return 7 + displacement_cost,
        .BP_PLUS_SI, .BX_PLUS_DI => return 8 + displacement_cost,
    }
}

fn transfers(op: MemoryOperand, num_transfers: u8) u8 {
    if (num_transfers == 0) {
        return 0;
    }

    const at_odd_address: bool = blk: {
        if (op.displacement) |d| {
            switch (d) {
                .byte => break :blk false,
                .word => |w| break :blk @mod(w, 2) == 1,
            }
        }
        break :blk false;
    };
    if (!at_odd_address) {
        return 0;
    }

    return num_transfers * 4;
}
