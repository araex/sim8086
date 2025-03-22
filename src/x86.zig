const std = @import("std");

pub const EffectiveAddressCalculation = @import("x86/fields.zig").EffectiveAddressCalculation;
pub const Displacement = @import("x86/fields.zig").Displacement;
pub const fmt = @import("x86/format.zig").fmt;
pub const Instruction = @import("x86/instruction.zig").Instruction;
pub const decodeInstruction = @import("x86/instruction.zig").decodeInstruction;
pub const makeInstruction = @import("x86/instruction.zig").makeInstruction;
pub const makeDst = @import("x86/operands.zig").makeDst;
pub const makeSrc = @import("x86/operands.zig").makeSrc;
pub const makeImmediate = @import("x86/operands.zig").makeImmediate;
pub const Register = @import("x86/operands.zig").Register;
pub const Memory = @import("x86/operands.zig").Memory;
pub const Simulator = @import("x86/simulator.zig").Simulator;
pub const SimMemory = @import("x86/simulator.zig").Memory;

pub fn decode(alloc: std.mem.Allocator, bin: []const u8) !std.ArrayList(Instruction) {
    var result = std.ArrayList(Instruction).init(alloc);
    errdefer result.deinit();
    var stream = std.io.fixedBufferStream(bin);
    var reader = stream.reader().any();
    while (true) {
        const instruction = decodeInstruction(&reader) catch |err| switch (err) {
            error.EndOfStream => break,
            error.UnknownInstruction => {
                std.log.err("Got unknown instruction. Parsed {d} sucessfully:\n", .{result.items.len});
                for (result.items) |item| {
                    std.log.err("{}", .{fmt(item)});
                }
                return error.UnknownInstruction;
            },
            else => return err,
        };
        try result.append(instruction);
    }

    return result;
}

pub fn toAsm(alloc: std.mem.Allocator, insts: []const Instruction) !std.ArrayList(u8) {
    var result = std.ArrayList(u8).init(alloc);
    errdefer result.deinit();
    try result.appendSlice("bits 16\n");

    var lineBuffer: [1024]u8 = undefined;
    for (insts) |instruction| {
        const instruction_string = try std.fmt.bufPrint(
            &lineBuffer,
            "{}\n",
            .{fmt(instruction)},
        );
        try result.appendSlice(std.ascii.lowerString(instruction_string, instruction_string));
    }
    return result;
}
