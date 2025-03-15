const std = @import("std");
const builtin = @import("builtin");

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

    if (args.len < 2) {
        std.log.err("Usage: {s} <input_file_path>", .{args[0]});
        std.process.exit(1);
    }

    const file_path = args[1];
    std.log.debug("reading '{s}'", .{file_path});
    const file_content = try readFile(alloc, file_path);
    defer alloc.free(file_content);

    // decode
    const decode_start = std.time.nanoTimestamp();
    const decoded = try x86.decode(alloc, file_content);
    const decode_end = std.time.nanoTimestamp();
    const decode_time_ns = decode_end - decode_start;
    std.log.info("decoded {d} bytes in {d:.3} ms", .{
        file_content.len,
        @as(f64, @floatFromInt(decode_time_ns)) / 1_000_000.0,
    });
    defer decoded.deinit();

    // output to asm
    var out = std.io.bufferedWriter(std.io.getStdOut().writer());
    std.log.debug("output ASM", .{});
    const asm_start = std.time.nanoTimestamp();
    for (decoded.items) |instruction| {
        try std.fmt.format(out.writer(), "{}\n", .{x86.fmt(instruction)});
    }
    const asm_end = std.time.nanoTimestamp();
    try out.flush();
    const asm_time_ns = asm_end - asm_start;
    std.log.info("generated ASM for {d} instructions in {d:.3} ms", .{
        decoded.items.len,
        @as(f64, @floatFromInt(asm_time_ns)) / 1_000_000.0,
    });
    std.log.debug("done", .{});
}

fn readFile(allocator: std.mem.Allocator, file_path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);
    errdefer allocator.free(buffer);

    const bytes_read = try file.readAll(buffer);
    if (bytes_read != file_size) {
        return error.IncompleteRead;
    }

    return buffer;
}
