const std = @import("std");

const Instruction = @import("instruction.zig").Instruction;
const makeInstruction = @import("instruction.zig").makeInstruction;
const Operation = @import("instruction.zig").Operation;
const RegisterName = @import("register.zig").RegisterName;
const Width = @import("instruction.zig").Width;

pub fn operation(op: Operation) u4 {
    return switch (op) {
        // Register/memory to/from register instructions: 1 byte opcode + ModR/M
        .add_rm_with_r_to_either,
        .sub_rm_and_r_to_either,
        .cmp_rm_with_r,
        .mov_rm_to_from_r,
        .mov_sr_to_rm,
        .mov_rm_to_sr,
        .add_imm_to_rm,
        .sub_imm_to_rm,
        .cmp_imm_with_rm,
        .mov_imm_to_rm,
        => return 2,

        // compact mov
        .mov_mem_to_accumulator, .mov_accumulator_to_mem, .mov_imm_to_r => return 1,

        // Compact arithmetic
        .add_imm_to_acc, .sub_imm_to_acc, .cmp_imm_with_acc => return 1,

        // Jumps
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
        => return 1,

        // Unknown opcode
        .Unknown => return 0,
    };
}

pub fn instruction(i: Instruction) u4 {
    const opsize = operation(i.op);
    var total_size = opsize;

    switch (i.dst) {
        .register => {},
        .memory => |mem| {
            switch (mem.calc) {
                .DIRECT_ADDRESS => total_size += if (i.wide == .Byte) 1 else 2,
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

    if (i.src) |src| {
        switch (src) {
            .register => {},
            .memory => |mem| {
                switch (mem.calc) {
                    .DIRECT_ADDRESS => total_size += if (i.wide == .Byte) 1 else 2,
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
                    .word => {
                        // Check if this could have been a sign-extended immediate
                        // For certain instructionss with word operands, immediates in the range -128 to 127
                        // would have been encoded as a single byte with sign extension
                        const could_be_sign_extended = switch (i.op) {
                            .add_imm_to_rm, .sub_imm_to_rm, .cmp_imm_with_rm => true,
                            else => false,
                        };

                        if (could_be_sign_extended and i.wide == .Word) {
                            const value = imm.value.word;
                            // Check if value fits in signed byte range (-128 to 127)
                            if (value <= 127 or value >= 0xFF80) { // 0xFF80 is -128 as u16
                                total_size += 1; // Sign-extended immediate takes 1 byte
                            } else {
                                total_size += 2; // Regular word immediate
                            }
                        } else {
                            total_size += 2; // Regular word immediate
                        }
                    },
                }
            },
        }
    }

    return total_size;
}

test "instruction size with sign-extended immediates" {
    const testing = std.testing;
    const TestCase = struct {
        inst: Instruction,
        expected_size: u4,
    };

    const tests = [_]TestCase{
        .{
            .inst = makeInstruction(
                Operation.add_imm_to_rm,
                Width.Word,
                RegisterName.AX,
                @as(u16, 5),
            ),
            .expected_size = 3,
        },
        .{
            .inst = makeInstruction(
                Operation.add_imm_to_rm,
                Width.Word,
                RegisterName.AX,
                @as(u16, 300),
            ),
            .expected_size = 4,
        },
        .{
            .inst = makeInstruction(
                Operation.add_imm_to_rm,
                Width.Word,
                RegisterName.AX,
                @as(u16, 0xFFF0),
            ),
            .expected_size = 3,
        },
        .{
            .inst = makeInstruction(
                Operation.mov_imm_to_r,
                Width.Word,
                RegisterName.AX,
                @as(u16, 5),
            ),
            .expected_size = 3,
        },
    };

    for (tests) |tc| {
        try testing.expectEqual(tc.expected_size, instruction(tc.inst));
    }
}
