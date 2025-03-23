const std = @import("std");
const builtin = @import("builtin");

const app = @import("app.zig");
const logger = @import("logger.zig");
const x86 = @import("x86.zig");

pub const std_options = std.Options{
    .logFn = logger.coloredLog,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.log.err("Memory leak detected", .{});
    }

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len != 2) {
        std.log.err("Expected exactly one argument: path to x8086 machine code file", .{});
        return;
    }

    const file_path = args[1];
    const file_content = try readFile(alloc, file_path);
    defer alloc.free(file_content);

    const decoded = try x86.toInstructionList(alloc, file_content);
    defer decoded.deinit();
    std.log.info("Decoded '{d}' bytes from '{s}'", .{ file_content.len, file_path });

    var simulator = try x86.Simulator.init(decoded.items);
    try app.run(alloc, &simulator);
}

fn readFile(alloc: std.mem.Allocator, file_path: []u8) ![]u8 {
    const max_file_size = 1024 * 1024;
    return std.fs.cwd().readFileAlloc(
        alloc,
        file_path,
        max_file_size,
    ) catch |err| {
        std.log.err("openFile('{s}'): {}", .{ file_path, err });
        return err;
    };
}
