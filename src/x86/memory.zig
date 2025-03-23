const std = @import("std");

pub const Memory = struct {
    // Not going to do segmented memory, 64k is all we can address
    data: [65536]u8 = [_]u8{0} ** 65536,

    pub fn setByte(self: *Memory, address: u16, value: u8) void {
        self.data[address] = value;
    }

    pub fn setWord(self: *Memory, address: u16, value: u16) void {
        const lo: u8 = @intCast(value & 0xFF);
        const hi: u8 = @intCast((value >> 8) & 0xFF);
        self.data[address] = lo;
        self.data[address + 1] = hi;
    }

    pub fn setAs(self: *Memory, T: type, address: u16, value: T) void {
        switch (T) {
            u8 => self.data[address] = value,
            u16 => self.setWord(address, value),
            else => unreachable,
        }
    }

    pub fn getByte(self: *const Memory, address: u16) u8 {
        return self.data[address];
    }

    pub fn getWord(self: *const Memory, address: u16) u16 {
        const lo: u16 = @as(u16, self.data[address]);
        const hi: u16 = @as(u16, self.data[address + 1]) << 8;
        return lo | hi;
    }

    pub fn getAs(self: *const Memory, T: type, address: u16) T {
        switch (T) {
            u8 => return self.data[address],
            u16 => return self.getWord(address),
            else => unreachable,
        }
    }
};

pub const MemoryOperand = struct {
    calc: EffectiveAddressCalculation,
    displacement: ?Displacement,
};

pub const EffectiveAddressCalculation = enum {
    BX_PLUS_SI,
    BX_PLUS_DI,
    BP_PLUS_SI,
    BP_PLUS_DI,
    SI,
    DI,
    BP,
    DIRECT_ADDRESS,
    BX,
};

pub const Displacement = union(enum) {
    byte: u8,
    word: u16,
};
