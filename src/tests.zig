const assert = @import("std").debug.assert;
const std = @import("std");

const nasm = @import("nasm.zig");
const x86 = @import("x86.zig");

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
        \\ip: 0x0018 (24) 
        \\
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
        \\ip: 0x001c (28)
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
        \\ip: 0x002C (44)
    ;
    try simulateListing("listing_0045_challenge_register_movs", parseHomeworkResults(expected));
}

test "Homework - Listing 46" {
    const expected =
        \\bx: 0xe102 (57602)
        \\cx: 0x0f01 (3841)
        \\sp: 0x03e6 (998)
        \\ip: 0x0018 (24)
    ;
    var expected_regs = parseHomeworkResults(expected);
    expected_regs.flags = .{
        .Parity = true,
        .Zero = true,
    };
    try simulateListing("listing_0046_add_sub_cmp", expected_regs);
}

test "Homework - Listing 47" {
    const expected =
        \\bx: 0x9ca5 (40101)
        \\dx: 0x000a (10)
        \\sp: 0x0063 (99)
        \\bp: 0x0062 (98)
        \\ip: 0x002c (44)
    ;
    var expected_regs = parseHomeworkResults(expected);
    expected_regs.flags = .{
        .Carry = true,
        .Parity = true,
        .AuxCarry = true,
        .Sign = true,
    };
    try simulateListing("listing_0047_challenge_flags", expected_regs);
}

test "Homework - Listing 48" {
    const expected =
        \\bx: 0x07d0 (2000)
        \\cx: 0xfce0 (64736)
        \\ip: 0x000e (14)
    ;
    var expected_regs = parseHomeworkResults(expected);
    expected_regs.flags = .{
        .Carry = true,
        .Sign = true,
    };
    try simulateListing("listing_0048_ip_register", expected_regs);
}

test "Homework - Listing 49" {
    const expected =
        \\bx: 0x0406 (1030)
        \\ip: 0x000e (14)
    ;
    var expected_regs = parseHomeworkResults(expected);
    expected_regs.flags = .{
        .Parity = true,
        .Zero = true,
    };
    try simulateListing("listing_0049_conditional_jumps", expected_regs);
}

test "Homework - Listing 50" {
    const expected =
        \\ax: 0x000d (13)
        \\bx: 0xfffb (65531)
        \\ip: 0x001c (28)
    ;
    var expected_regs = parseHomeworkResults(expected);
    expected_regs.flags = .{
        .Carry = true,
        .AuxCarry = true,
        .Sign = true,
    };
    try simulateListing("listing_0050_challenge_jumps", expected_regs);
}

test "Homework - Listing 51" {
    const expected =
        \\bx: 0x0001 (1)
        \\cx: 0x0002 (2)
        \\dx: 0x000a (10)
        \\bp: 0x0004 (4)
        \\ip: 0x0030 (48)
    ;
    var expected_regs = parseHomeworkResults(expected);
    expected_regs.flags = .{};
    try simulateListing("listing_0051_memory_mov", expected_regs);
}

test "Homework - Listing 52" {
    const expected =
        \\bx: 0x0006 (6)
        \\cx: 0x0004 (4)
        \\dx: 0x0006 (6)
        \\bp: 0x03e8 (1000)
        \\si: 0x0006 (6)
        \\ip: 0x0023 (35)
    ;
    var expected_regs = parseHomeworkResults(expected);
    expected_regs.flags = .{
        .Parity = true,
        .Zero = true,
    };
    try simulateListing("listing_0052_memory_add_loop", expected_regs);
}

test "Homework - Listing 53" {
    const expected =
        \\bx: 0x0006 (6)
        \\dx: 0x0006 (6)
        \\bp: 0x03e6 (998)
        \\ip: 0x0021 (33)
    ;
    var expected_regs = parseHomeworkResults(expected);
    expected_regs.flags = .{
        .Parity = true,
        .Zero = true,
    };
    try simulateListing("listing_0053_add_loop_challenge", expected_regs);
}

test "Homework - Listing 54" {
    const expected =
        \\cx: 0x0040 (64)
        \\dx: 0x0040 (64)
        \\bp: 0x4100 (16640)
        \\ip: 0x0026 (38)
    ;
    var expected_regs = parseHomeworkResults(expected);
    expected_regs.flags = .{
        .Parity = true,
        .Zero = true,
    };
    try simulateListing("listing_0054_draw_rectangle", expected_regs);
}

fn parseHomeworkResults(comptime in: []const u8) x86.Registers {
    var result = x86.Registers{};

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
        } else if (std.mem.eql(u8, reg_name, "ip")) {
            result.setWord(.IP, value);
        }
    }

    return result;
}

fn simulateListing(comptime listing_file_name: []const u8, expected_regs: x86.Registers) !void {
    const data_dir = "data";
    const in_bin_path = std.fmt.comptimePrint("{s}/{s}", .{ data_dir, listing_file_name });
    const file_content = @embedFile(in_bin_path);

    var sim = try x86.Simulator.init(std.testing.allocator, file_content);
    defer sim.deinit();
    while (!sim.isDone()) {
        try sim.step();
    }
    try expectEqualRegisters(expected_regs, sim.registers);
}

test "Simulator Testbench" {
    const asm_instructions =
        \\bits 16
        \\add al, 7
        \\sub al, 7
    ;

    const alloc = std.testing.allocator;
    const file_name = getUniqueFileName(asm_instructions);
    const bin = try runNasm(alloc, asm_instructions, file_name);
    defer alloc.free(bin);

    var expected_registers = x86.Registers{};
    var sim = try x86.Simulator.init(alloc, bin);
    defer sim.deinit();
    try std.testing.expectEqual(0, sim.cur_instruction_idx);
    try expectEqualRegisters(expected_registers, sim.registers);

    expected_registers.setByte(.AL, 7);
    expected_registers.setWord(.IP, 2);
    expected_registers.flags = .{};
    try sim.step();
    try expectEqualRegisters(expected_registers, sim.registers);

    expected_registers.setByte(.AL, 0);
    expected_registers.setWord(.IP, 4);
    expected_registers.flags = .{
        .Parity = true,
        .Zero = true,
    };
    try sim.step();
    try expectEqualRegisters(expected_registers, sim.registers);
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
    const instructions = try x86.decode.instructionList(alloc, got_bin);
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

    const instructions = try x86.decode.instructionList(alloc, want_bin[0..]);
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

fn expectEqualRegisters(expected: x86.Registers, actual: x86.Registers) !void {
    if (!std.meta.eql(expected.data, actual.data)) {
        std.log.err("Found register diff. Printing register diff...\nAL AH BL BH CL CH DL DH|-SP--|-BP--|-SI--|-DI--|", .{});
        std.testing.expectEqualSlices(u8, expected.data[0..16], actual.data[0..16]) catch {};

        std.log.err("Printing segment register diff...\n-ES--|-CS--|-SS--|-DS--|", .{});
        std.testing.expectEqualSlices(u8, expected.data[16..24], actual.data[16..24]) catch {};

        std.log.err("Printing instruction pointer diff...", .{});
        // std.testing.expectEqualSlices(u8, expected.data[24..], actual.data[24..]) catch {};
        std.testing.expectEqual(expected.getWord(.IP), actual.getWord(.IP)) catch {};
        return error.TestExpectedEqual;
    }

    var any_flag_diff = false;
    if (expected.flags.Parity != actual.flags.Parity) {
        std.log.err("Expected Parity flag {}, found {}", .{ expected.flags.Parity, actual.flags.Parity });
        any_flag_diff = true;
    }
    if (expected.flags.Zero != actual.flags.Zero) {
        std.log.err("Expected Zero flag {}, found {}", .{ expected.flags.Zero, actual.flags.Zero });
        any_flag_diff = true;
    }
    if (expected.flags.Sign != actual.flags.Sign) {
        std.log.err("Expected Sign flag {}, found {}", .{ expected.flags.Sign, actual.flags.Sign });
        any_flag_diff = true;
    }
    if (expected.flags.Overflow != actual.flags.Overflow) {
        std.log.err("Expected Overflow flag {}, found {}", .{ expected.flags.Overflow, actual.flags.Overflow });
        any_flag_diff = true;
    }

    if (any_flag_diff) {
        return error.TestExpectedEqual;
    }
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
