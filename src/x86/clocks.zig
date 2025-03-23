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
                        .memory => |src_mem| return 9 + eac(src_mem),
                    }
                },
                .memory => |dst_mem| {
                    switch (src) {
                        .register => return 16 + eac(dst_mem),
                        .immediate => return 17 + eac(dst_mem),
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
                        .memory => |src_mem| return 9 + eac(src_mem),
                    }
                },
                .memory => |dst_mem| {
                    switch (src) {
                        .register => return 9 + eac(dst_mem),
                        .immediate => return 10 + eac(dst_mem),
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
                                return 10;
                            }
                            return 8 + eac(src_mem);
                        },
                    }
                },
                .memory => |dst_mem| {
                    switch (src) {
                        .register => |src_reg| {
                            if (Register.isAccumulator(src_reg)) {
                                return 10;
                            }
                            return 9 + eac(dst_mem);
                        },
                        .immediate => return 10 + eac(dst_mem),
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
    const displacement_base_cost: u8 = if (op.displacement) |d| if (d.isZero()) 0 else 4 else 0;
    switch (op.calc) {
        .DIRECT_ADDRESS => return 6,
        .SI, .DI, .BP, .BX => return 5 + displacement_base_cost,
        .BP_PLUS_DI, .BX_PLUS_SI => return 7 + displacement_base_cost,
        .BP_PLUS_SI, .BX_PLUS_DI => return 8 + displacement_base_cost,
    }
}
