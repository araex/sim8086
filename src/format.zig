const std = @import("std");
const x8086 = @import("x8086.zig");
const opcode = @import("opcode.zig");

const asmFormatter = struct {
    fn memory(
        mem: x8086.Memory,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const str = switch (mem.calc) {
            x8086.EffectiveAddressCalculation.BX_PLUS_SI => "bx + si",
            x8086.EffectiveAddressCalculation.BX_PLUS_DI => "bx + di",
            x8086.EffectiveAddressCalculation.BP_PLUS_SI => "bp + si",
            x8086.EffectiveAddressCalculation.BP_PLUS_DI => "bp + di",
            x8086.EffectiveAddressCalculation.SI => "si",
            x8086.EffectiveAddressCalculation.DI => "di",
            x8086.EffectiveAddressCalculation.BP => "bp",
            x8086.EffectiveAddressCalculation.DIRECT_ADDRESS => "",
            x8086.EffectiveAddressCalculation.BX => "bx",
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
                    if (mem.calc == x8086.EffectiveAddressCalculation.DIRECT_ADDRESS) {
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
        field: x8086.ImmediateField,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (field.value) {
            .byte => |b| {
                if (field.explicit_size) {
                    try writer.print("byte {d}", .{b});
                } else {
                    try writer.print("{d}", .{b});
                }
            },
            .word => |w| {
                if (field.explicit_size) {
                    try writer.print("word {d}", .{w});
                } else {
                    try writer.print("{d}", .{w});
                }
            },
        }
    }

    fn srcType(
        src_type: x8086.SrcType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (src_type) {
            .register => |reg| try writer.print("{s}", .{std.enums.tagName(x8086.Register, reg).?}),
            .immediate => |i| try writer.print("{s}", .{x8086Immediate(i)}),
            .memory => |mem| try writer.print("{s}", .{x8086Memory(mem)}),
        }
    }

    fn dstType(
        dst_type: x8086.DstType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (dst_type) {
            .register => |reg| return writer.print("{s}", .{std.enums.tagName(x8086.Register, reg).?}),
            .memory => |mem| return writer.print("{s}", .{x8086Memory(mem)}),
        }
        unreachable;
    }

    fn instruction(
        to_format: x8086.Instruction,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const op = switch (to_format.op) {
            opcode.Mnemonic.mov_rm_to_from_r => "mov",
            opcode.Mnemonic.mov_imm_to_r => "mov",
            opcode.Mnemonic.mov_imm_to_rm => "mov",
            opcode.Mnemonic.mov_accumulator_to_mem => "mov",
            opcode.Mnemonic.mov_mem_to_accumulator => "mov",
            opcode.Mnemonic.Unknown => "<unknown>",
        };
        return writer.print("{s} {s}, {}", .{
            op,
            x8086Dst(to_format.dst),
            x8086Src(to_format.src),
        });
    }
};

pub fn x8086Memory(to_format: x8086.Memory) std.fmt.Formatter(asmFormatter.memory) {
    return .{ .data = to_format };
}

pub fn x8086Immediate(to_format: x8086.ImmediateField) std.fmt.Formatter(asmFormatter.immediateField) {
    return .{ .data = to_format };
}

pub fn x8086Src(to_format: x8086.SrcType) std.fmt.Formatter(asmFormatter.srcType) {
    return .{ .data = to_format };
}

pub fn x8086Dst(to_format: x8086.DstType) std.fmt.Formatter(asmFormatter.dstType) {
    return .{ .data = to_format };
}

pub fn x8086Instruction(to_format: x8086.Instruction) std.fmt.Formatter(asmFormatter.instruction) {
    return .{ .data = to_format };
}

pub fn toAsm(alloc: std.mem.Allocator, instructions: []const x8086.Instruction) !std.ArrayList(u8) {
    var result = std.ArrayList(u8).init(alloc);
    try result.appendSlice("bits 16\n");

    var lineBuffer: [1024]u8 = undefined;
    for (instructions) |instruction| {
        // std.fmt.Formatter(comptime formatFn: anytype)
        const instruction_string = try std.fmt.bufPrint(&lineBuffer, "{}\n", .{x8086Instruction(instruction)});
        try result.appendSlice(std.ascii.lowerString(instruction_string, instruction_string));
    }
    return result;
}
