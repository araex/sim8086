const assert = @import("std").debug.assert;
const std = @import("std");

const nasm = @import("nasm.zig");
const x86 = @import("x86.zig");
const x86_SimRegisters = @import("x86/simulator.zig").Registers;

test "Homework - Listing 37" {
    try testDecodeEncodeListing("listing_0037_single_register_mov");
}

test "Homework - Listing 38" {
    try testDecodeEncodeListing("listing_0038_many_register_mov");
}

test "Homework - Listing 39" {
    try testDecodeEncodeListing("listing_0039_more_movs");
}

test "Homework - Listing 40" {
    try testDecodeEncodeListing("listing_0040_challenge_movs");
}

test "Homework - Listing 41" {
    try testDecodeEncodeListing("listing_0041_add_sub_cmp_jnz");
}

// Didn't do Listing 42

test "Homework - Listing 43" {
    const expected =
        \\ax: 0x0001 (1)
        \\bx: 0x0002 (2)
        \\cx: 0x0003 (3)
        \\dx: 0x0004 (4)
        \\sp: 0x0005 (5)
        \\bp: 0x0006 (6)
        \\si: 0x0007 (7)
        \\di: 0x0008 (8)
    ;
    try simulateListing("listing_0043_immediate_movs", parseHomeworkResults(expected));
}

test "Homework - Listing 44" {
    const expected =
        \\ax: 0x0004 (4)
        \\bx: 0x0003 (3)
        \\cx: 0x0002 (2)
        \\dx: 0x0001 (1)
        \\sp: 0x0001 (1)
        \\bp: 0x0002 (2)
        \\si: 0x0003 (3)
        \\di: 0x0004 (4)
    ;
    try simulateListing("listing_0044_register_movs", parseHomeworkResults(expected));
}

test "Homework - Listing 45" {
    const expected =
        \\ax: 0x4411 (17425)
        \\bx: 0x3344 (13124)
        \\cx: 0x6677 (26231)
        \\dx: 0x7788 (30600)
        \\sp: 0x4411 (17425)
        \\bp: 0x3344 (13124)
        \\si: 0x6677 (26231)
        \\di: 0x7788 (30600)
        \\es: 0x6677 (26231)
        \\ss: 0x4411 (17425)
        \\ds: 0x3344 (13124)
    ;
    try simulateListing("listing_0045_challenge_register_movs", parseHomeworkResults(expected));
}

fn parseHomeworkResults(comptime in: []const u8) x86_SimRegisters {
    var result = x86_SimRegisters{};

    var lines = std.mem.tokenizeScalar(u8, in, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Format is "xx: 0xYYYY (Z)"
        const colon_pos = std.mem.indexOfScalar(u8, line, ':') orelse continue;
        if (colon_pos < 2) continue;

        const reg_name = line[0..colon_pos];

        // Find the hex value starting with "0x"
        const hex_start = std.mem.indexOf(u8, line, "0x") orelse continue;
        const hex_end = std.mem.indexOfScalar(u8, line[hex_start..], ' ') orelse line.len;
        const hex_value = line[hex_start..(hex_start + hex_end)];

        // Parse the hex value
        const value = std.fmt.parseInt(u16, hex_value[2..], 16) catch continue;

        // Set the appropriate register
        if (std.mem.eql(u8, reg_name, "ax")) {
            result.setWord(.AX, value);
        } else if (std.mem.eql(u8, reg_name, "bx")) {
            result.setWord(.BX, value);
        } else if (std.mem.eql(u8, reg_name, "cx")) {
            result.setWord(.CX, value);
        } else if (std.mem.eql(u8, reg_name, "dx")) {
            result.setWord(.DX, value);
        } else if (std.mem.eql(u8, reg_name, "sp")) {
            result.setWord(.SP, value);
        } else if (std.mem.eql(u8, reg_name, "bp")) {
            result.setWord(.BP, value);
        } else if (std.mem.eql(u8, reg_name, "si")) {
            result.setWord(.SI, value);
        } else if (std.mem.eql(u8, reg_name, "di")) {
            result.setWord(.DI, value);
        } else if (std.mem.eql(u8, reg_name, "es")) {
            result.setWord(.ES, value);
        } else if (std.mem.eql(u8, reg_name, "cs")) {
            result.setWord(.CS, value);
        } else if (std.mem.eql(u8, reg_name, "ss")) {
            result.setWord(.SS, value);
        } else if (std.mem.eql(u8, reg_name, "ds")) {
            result.setWord(.DS, value);
        }
    }

    return result;
}

fn simulateListing(comptime listing_file_name: []const u8, expected_regs: x86_SimRegisters) !void {
    const data_dir = "data";
    const in_bin_path = std.fmt.comptimePrint("{s}/{s}", .{ data_dir, listing_file_name });
    const file_content = @embedFile(in_bin_path);
    const instructions = try x86.decode(std.testing.allocator, file_content);
    defer instructions.deinit();

    var sim = try x86.Simulator.init(instructions.items);
    while (!sim.isDone()) {
        try sim.step();
    }
    try expectEqualRegisters(expected_regs, sim.registers);
}

test "Simulator" {
    const asm_instructions =
        \\bits 16
        \\mov al, 5
        \\mov bx, 3
    ;

    const alloc = std.testing.allocator;
    const file_name = getUniqueFileName(asm_instructions);
    const bin = try runNasm(alloc, asm_instructions, file_name);
    defer alloc.free(bin);

    const instructions = try x86.decode(alloc, bin);
    defer instructions.deinit();

    var expected_registers = x86_SimRegisters{};
    var sim = try x86.Simulator.init(instructions.items);
    try std.testing.expectEqual(0, sim.cur_instruction);
    try expectEqualRegisters(expected_registers, sim.registers);

    expected_registers.setByte(.AL, 5);
    try sim.step();
    try expectEqualRegisters(expected_registers, sim.registers);

    expected_registers.setWord(.BX, 3);
    try sim.step();
}

fn testEncodeDecodeWithAsmDiff(comptime asm_instruction: [:0]const u8) !void {
    std.log.info("Encode->Decode '{s}'...", .{asm_instruction});

    const file_name = getUniqueFileName(asm_instruction);

    const prefix = "bits 16\n";
    const postfix = "\n";
    const want_asm = std.fmt.comptimePrint("{s}{s}{s}", .{ prefix, asm_instruction, postfix });

    // 1. Assemble given ASM to bin
    const alloc = std.testing.allocator;
    const got_bin = try runNasm(alloc, want_asm, file_name);
    defer alloc.free(got_bin);

    // 2. Decode assembled bin
    const instructions = try x86.decode(alloc, got_bin);
    defer instructions.deinit();
    const got_asm = try x86.toAsm(alloc, instructions.items);
    defer got_asm.deinit();

    // 3. Diff decoded ASM to original ASM
    try std.testing.expectEqualSlices(u8, want_asm, got_asm.items);

    // 4. Encode the decoded ASM again
    const reassembled_bin = try runNasm(alloc, got_asm.items, file_name);
    defer alloc.free(reassembled_bin);

    // 5. Diff binaries from 1 & 4
    try std.testing.expectEqualSlices(u8, got_bin, reassembled_bin);

    std.log.info("Success!", .{});
}

fn runNasm(alloc: std.mem.Allocator, asm_content: []const u8, file_name: []const u8) ![]u8 {
    std.log.debug("Assemble decoded asm...\n", .{});
    return try nasm.assemble(alloc, asm_content, file_name);
}

// 1. Read given bin from data folder
// 2. Decode bin to ASM
// 3. Assemble decoded ASM to new bin
// 4. Diff reassembled bin to original bin
fn testDecodeEncodeListing(comptime listing_file_name: []const u8) !void {
    const data_dir = "data";
    const in_asm_path = std.fmt.comptimePrint("{s}/{s}.asm", .{ data_dir, listing_file_name });
    const in_bin_path = std.fmt.comptimePrint("{s}/{s}", .{ data_dir, listing_file_name });
    std.log.debug("Decode '{s}'...", .{in_bin_path});

    const alloc = std.testing.allocator;
    const want_asm_with_comments = @embedFile(in_asm_path);
    const want_asm = try withoutCommentsAndEmptyLines(alloc, want_asm_with_comments);
    defer alloc.free(want_asm);
    const want_bin = @embedFile(in_bin_path);

    const instructions = try x86.decode(alloc, want_bin[0..]);
    defer instructions.deinit();

    const got_asm = try x86.toAsm(alloc, instructions.items);
    defer got_asm.deinit();

    const got_bin = try runNasm(alloc, got_asm.items, listing_file_name);
    defer alloc.free(got_bin);

    std.log.debug("Compare assembled binary to ground truth...\n", .{});
    std.testing.expectEqualSlices(u8, want_bin, got_bin) catch |e| {
        std.log.err("- Compiled binary differs from ground truth. Diffing decoded ASM...\n", .{});
        std.testing.expectEqualSlices(u8, want_asm, got_asm.items) catch {};
        return e;
    };

    std.log.info("Success!\n", .{});
}

fn expectEqualRegisters(expected: x86_SimRegisters, actual: x86_SimRegisters) !void {
    errdefer {
        std.log.err("Printing register...\nAL AH BL BH CL CH DL DH|-SP--|-BP--|-SI--|-DI--|", .{});
        std.testing.expectEqualSlices(u8, expected.data[0..16], actual.data[0..16]) catch {};

        std.log.err("Printing segment register...\n-ES--|-CS--|-SS--|-DS--|", .{});
        std.testing.expectEqualSlices(u8, expected.data[16..], actual.data[16..]) catch {};
    }
    try std.testing.expectEqual(expected.getByte(.AL), actual.getByte(.AL));
    try std.testing.expectEqual(expected.getByte(.AH), actual.getByte(.AH));
    try std.testing.expectEqual(expected.getByte(.BL), actual.getByte(.BL));
    try std.testing.expectEqual(expected.getByte(.BH), actual.getByte(.BH));
    try std.testing.expectEqual(expected.getByte(.CL), actual.getByte(.CL));
    try std.testing.expectEqual(expected.getByte(.CH), actual.getByte(.CH));
    try std.testing.expectEqual(expected.getByte(.DL), actual.getByte(.DL));
    try std.testing.expectEqual(expected.getByte(.DH), actual.getByte(.DH));

    try std.testing.expectEqual(expected.getWord(.AX), actual.getWord(.AX));
    try std.testing.expectEqual(expected.getWord(.BX), actual.getWord(.BX));
    try std.testing.expectEqual(expected.getWord(.CX), actual.getWord(.CX));
    try std.testing.expectEqual(expected.getWord(.DX), actual.getWord(.DX));
    try std.testing.expectEqual(expected.getWord(.SP), actual.getWord(.SP));
    try std.testing.expectEqual(expected.getWord(.BP), actual.getWord(.BP));
    try std.testing.expectEqual(expected.getWord(.SI), actual.getWord(.SI));
    try std.testing.expectEqual(expected.getWord(.DI), actual.getWord(.DI));
}

fn getUniqueFileName(comptime asm_instruction: [:0]const u8) []const u8 {
    const hash = comptime std.hash.CityHash64.hash(asm_instruction);
    return &std.fmt.comptimePrint("{d}", .{hash}).*;
}

fn withoutCommentsAndEmptyLines(
    alloc: std.mem.Allocator,
    data: []const u8,
) ![]u8 {
    var result = std.ArrayList(u8).init(alloc);
    var it = std.mem.tokenizeScalar(u8, data, '\n');
    while (it.next()) |line| {
        if (std.mem.startsWith(u8, line, ";")) {
            continue;
        }
        if (line.len == 1 and line[0] == '\n') {
            continue;
        }
        if (result.items.len > 0) {
            try result.append('\n');
        }
        try result.appendSlice(line);
    }
    if (data[data.len - 1] == '\n') {
        try result.append('\n');
    }
    return result.toOwnedSlice();
}
