const std = @import("std");

const nasm_exe = "nasm";
// Too much of a pain to deal with cwd & running tests within vscode. Hardcoding the path is good enough for now.
const out_dir = "C:\\dev\\computer_enhance\\zig-out\\nasm";

pub fn assemble(alloc: std.mem.Allocator, buf: []const u8, file_name: []const u8) ![]u8 {
    std.fs.makeDirAbsoluteZ(out_dir) catch |e| switch (e) {
        error.PathAlreadyExists => {},
        else => return e,
    };

    var working_dir = try std.fs.openDirAbsoluteZ(out_dir, .{});
    const asm_path = try std.fmt.allocPrint(alloc, "{s}\\{s}.asm", .{ out_dir, file_name });
    defer alloc.free(asm_path);
    std.debug.print("-> write asm to file '{s}'...\n", .{asm_path});
    try working_dir.writeFile(std.fs.Dir.WriteFileOptions{
        .sub_path = asm_path,
        .data = buf,
        .flags = .{
            .truncate = true,
        },
    });

    const bin_path = try std.fmt.allocPrint(alloc, "{s}\\{s}", .{ out_dir, file_name });
    defer alloc.free(bin_path);
    const argv = &[_][]const u8{
        nasm_exe,
        "-f",
        "bin",
        "-o",
        bin_path,
        asm_path,
    };
    std.debug.print("-> {s}...\n", .{argv});
    const result = try std.process.Child.run(.{
        .allocator = alloc,
        .argv = argv,
    });
    _ = try std.io.getStdOut().write(result.stdout);
    _ = try std.io.getStdErr().write(result.stderr);

    std.debug.print("-> read back '{s}'...\n", .{bin_path});
    const reassembled_bin = try working_dir.readFileAlloc(alloc, bin_path, 1024 * 1024);
    return reassembled_bin;
}
