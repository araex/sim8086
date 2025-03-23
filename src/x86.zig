const std = @import("std");

pub const decode = @import("x86/decode.zig");
pub const fmt = @import("x86/format.zig").fmt;
pub const Instruction = @import("x86/instruction.zig").Instruction;
pub const makeInstruction = @import("x86/instruction.zig").makeInstruction;
pub const makeDst = @import("x86/instruction.zig").makeDst;
pub const makeSrc = @import("x86/instruction.zig").makeSrc;
pub const makeImmediate = @import("x86/instruction.zig").makeImmediate;
pub const Displacement = @import("x86/memory.zig").Displacement;
pub const EffectiveAddressCalculation = @import("x86/memory.zig").EffectiveAddressCalculation;
pub const MemoryName = @import("x86/memory.zig").MemoryOperand;
pub const Memory = @import("x86/memory.zig").Memory;
pub const RegisterName = @import("x86/register.zig").RegisterName;
pub const Registers = @import("x86/register.zig").Registers;
pub const Simulator = @import("x86/simulator.zig").Simulator;
pub const size = @import("x86/size.zig");

pub fn toInstructionList(alloc: std.mem.Allocator, bin: []const u8) !std.ArrayList(Instruction) {
    var result = std.ArrayList(Instruction).init(alloc);
    errdefer result.deinit();
    var stream = std.io.fixedBufferStream(bin);
    var reader = stream.reader().any();
    while (true) {
        const instruction = decode.instruction(&reader) catch |err| switch (err) {
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
