const std = @import("std");

const ImmediateField = @import("operands.zig").ImmediateField;
const instructions = @import("instruction.zig");
const JumpDestination = @import("operands.zig").JumpDestination;
const Memory = @import("operands.zig").Memory;
const Register = @import("operands.zig").Register;

pub fn fmt(to_format: instructions.Instruction) std.fmt.Formatter(asmFormatter.instruction) {
    return .{ .data = to_format };
}

const asmFormatter = struct {
    const EffectiveAddressCalculation = @import("fields.zig").EffectiveAddressCalculation;

    fn memory(
        mem: Memory,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const str = switch (mem.calc) {
            EffectiveAddressCalculation.BX_PLUS_SI => "bx + si",
            EffectiveAddressCalculation.BX_PLUS_DI => "bx + di",
            EffectiveAddressCalculation.BP_PLUS_SI => "bp + si",
            EffectiveAddressCalculation.BP_PLUS_DI => "bp + di",
            EffectiveAddressCalculation.SI => "si",
            EffectiveAddressCalculation.DI => "di",
            EffectiveAddressCalculation.BP => "bp",
            EffectiveAddressCalculation.DIRECT_ADDRESS => "",
            EffectiveAddressCalculation.BX => "bx",
        };

        if (mem.displacement) |d| {
            switch (d) {
                .byte => |b| {
                    if (b == 0) {
                        try writer.print("[{s}]", .{str});
                        return;
                    } else if (b > 127) {
                        // Displacements in ASM are written as signed and used to decide
                        // if 16-bit displacment is needed. So despite it being ambiguous
                        // what the original ASM was, we opt for the version that will
                        // produce smaller binaries if assembled again.

                        const signed: i8 = @bitCast(b);
                        try writer.print("[{s} - {d}]", .{ str, @abs(signed) });
                        return;
                    } else {
                        try writer.print("[{s} + {d}]", .{ str, b });
                        return;
                    }
                },
                .word => |w| {
                    if (mem.calc == EffectiveAddressCalculation.DIRECT_ADDRESS) {
                        try writer.print("[{d}]", .{w});
                        return;
                    } else if (w == 0) {
                        try writer.print("[{s}]", .{str});
                        return;
                    } else if (w > 32768) {
                        const signed: i16 = @bitCast(w);
                        try writer.print("[{s} - {d}]", .{ str, @abs(signed) });
                    } else {
                        try writer.print("[{s} + {d}]", .{ str, w });
                        return;
                    }
                },
            }
        } else {
            try writer.print("[{s}]", .{str});
            return;
        }
    }

    fn immediateField(
        field: ImmediateField,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (field.value) {
            .byte => |b| {
                try writer.print("{d}", .{b});
            },
            .word => |w| {
                try writer.print("{d}", .{w});
            },
        }
    }

    fn jumpDestination(
        jump: JumpDestination,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        // nasm can assemble relative offset with $+ syntax.
        // jumps are relative to next instruction, so +2 in
        // our case. $+0 generates a -2 constant to compensate
        // for that.
        // See https://www.computerenhance.com/p/opcode-patterns-in-8086-arithmetic/comment/13475922
        if ((jump.increment + 2) > 0) {
            try writer.print("$+{d}+0", .{jump.increment + 2});
        } else if ((jump.increment + 2) == 0) {
            try writer.print("$+0", .{});
        } else {
            try writer.print("${d}+0", .{jump.increment + 2});
        }
    }

    fn instruction(
        to_format: instructions.Instruction,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const mnemonic = switch (to_format.op) {
            .mov_rm_to_from_r,
            .mov_imm_to_r,
            .mov_imm_to_rm,
            .mov_accumulator_to_mem,
            .mov_mem_to_accumulator,
            => "mov",
            .add_rm_with_r_to_either,
            .add_imm_to_acc,
            .add_imm_to_rm,
            => "add",
            .sub_imm_to_acc,
            .sub_imm_to_rm,
            .sub_rm_and_r_to_either,
            => "sub",
            .cmp_imm_with_acc,
            .cmp_imm_with_rm,
            .cmp_rm_with_r,
            => "cmp",
            .jo => "jo",
            .jno => "jno",
            .jb_jnae => "jb",
            .jnb_jae => "jnb",
            .je_jz => "je",
            .jne_jnz => "jne",
            .jbe_jna => "jbe",
            .jnbe_ja => "jnbe",
            .js => "js",
            .jns => "jns",
            .jp_jpe => "jp",
            .jnp_jpo => "jnp",
            .jl_jnge => "jl",
            .jnl_jge => "jnl",
            .jle_jng => "jle",
            .jnle_jg => "jnle",
            .loopnz_loopne => "loopnz",
            .loopz_loope => "loopz",
            .loop => "loop",
            .jcxz => "jcxz",
            .Unknown => "<unknown>",
        };

        try writer.print("{s}", .{mnemonic});

        switch (getExplicitSizeSpecifier(to_format)) {
            .None => {},
            .Byte => try writer.print(" byte", .{}),
            .Word => try writer.print(" word", .{}),
        }

        if (to_format.src) |src| {
            switch (to_format.dst) {
                .register => |reg| try writer.print(" {s},", .{std.enums.tagName(Register, reg).?}),
                .memory => |mem| try writer.print(" {s},", .{fmtMemory(mem)}),
                .jump => unreachable,
            }

            switch (src) {
                .register => |reg| try writer.print(" {s}", .{std.enums.tagName(Register, reg).?}),
                .immediate => |i| try writer.print(" {s}", .{fmtImmediate(i)}),
                .memory => |mem| try writer.print(" {s}", .{fmtMemory(mem)}),
            }
        } else {
            switch (to_format.dst) {
                .jump => |jump| try writer.print(" {s}", .{fmtJumpDestination(jump)}),
                .register => unreachable,
                .memory => unreachable,
            }
        }
    }
};

fn fmtMemory(to_format: Memory) std.fmt.Formatter(asmFormatter.memory) {
    return .{ .data = to_format };
}

fn fmtImmediate(to_format: ImmediateField) std.fmt.Formatter(asmFormatter.immediateField) {
    return .{ .data = to_format };
}

fn fmtJumpDestination(to_format: JumpDestination) std.fmt.Formatter(asmFormatter.jumpDestination) {
    return .{ .data = to_format };
}

const SizeSpecifier = enum {
    None,
    Byte,
    Word,
};

fn getExplicitSizeSpecifier(instruction: instructions.Instruction) SizeSpecifier {
    // Check if the destination is a memory operand.
    const dst_is_memory = switch (instruction.dst) {
        .memory => true,
        else => false,
    };

    if (!dst_is_memory) {
        return .None;
    }

    if (instruction.src == null) {
        // Its a jump
        return .None;
    }

    // If destination is memory, check the source. If the source is an immediate,
    // we need to check if the size is ambiguous.
    const src_is_immediate = switch (instruction.src.?) {
        .immediate => true,
        else => false,
    };

    if (!src_is_immediate) {
        return .None;
    }

    switch (instruction.wide) {
        .Byte => return .Byte,
        .Word => return .Word,
    }
    return .None;
}
