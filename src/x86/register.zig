const std = @import("std");

const Width = @import("instruction.zig").Width;

pub const RegisterName = enum { AL, CL, DL, BL, AH, CH, DH, BH, AX, CX, DX, BX, SP, BP, SI, DI, ES, CS, SS, DS, IP };

pub const Registers = struct {
    // [AX_LO, AX_HI, BX_LO, BX_HI, CX_LO, CX_HI, DX_LO, DX_HI, SP_LO, SP_HI, BP_LO, BP_HI, SI_LO, SI_HI, DI_LO, DI_HI, ES, CS, SS, DS, IP]
    data: [26]u8 = [_]u8{0} ** 26,

    flags: struct {
        Carry: bool = false,
        Parity: bool = false,
        AuxCarry: bool = false,
        Zero: bool = false,
        Sign: bool = false,
        Overflow: bool = false,
    } = .{},

    // Register byte offsets
    const AX_OFFSET = 0;
    const BX_OFFSET = 2;
    const CX_OFFSET = 4;
    const DX_OFFSET = 6;
    const SP_OFFSET = 8;
    const BP_OFFSET = 10;
    const SI_OFFSET = 12;
    const DI_OFFSET = 14;
    const ES_OFFSET = 16;
    const CS_OFFSET = 18;
    const SS_OFFSET = 20;
    const DS_OFFSET = 22;
    const IP_OFFSET = 24;

    // Set register value based on operation type
    pub fn set(self: *Registers, width: Width, dst: RegisterName, value: u16) void {
        switch (width) {
            .Byte => self.setByte(dst, @intCast(value & 0xFF)),
            .Word => self.setWord(dst, value),
        }
    }

    // Set 8-bit register
    pub fn setByte(self: *Registers, dst: RegisterName, value: u8) void {
        switch (dst) {
            .AL => self.data[AX_OFFSET] = value,
            .AH => self.data[AX_OFFSET + 1] = value,
            .BL => self.data[BX_OFFSET] = value,
            .BH => self.data[BX_OFFSET + 1] = value,
            .CL => self.data[CX_OFFSET] = value,
            .CH => self.data[CX_OFFSET + 1] = value,
            .DL => self.data[DX_OFFSET] = value,
            .DH => self.data[DX_OFFSET + 1] = value,
            .AX => self.data[AX_OFFSET] = value,
            .BX => self.data[BX_OFFSET] = value,
            .CX => self.data[CX_OFFSET] = value,
            .DX => self.data[DX_OFFSET] = value,
            .SP => self.data[SP_OFFSET] = value,
            .BP => self.data[BP_OFFSET] = value,
            .SI => self.data[SI_OFFSET] = value,
            .DI => self.data[DI_OFFSET] = value,
            .ES => self.data[ES_OFFSET] = value,
            .CS => self.data[CS_OFFSET] = value,
            .SS => self.data[SS_OFFSET] = value,
            .DS => self.data[DS_OFFSET] = value,
            .IP => self.data[IP_OFFSET] = value,
        }
    }

    // Set 16-bit register
    pub fn setWord(self: *Registers, dst: RegisterName, value: u16) void {
        std.debug.assert(isWide(dst));
        const lo: u8 = @intCast(value & 0xFF);
        const hi: u8 = @intCast((value >> 8) & 0xFF);

        switch (dst) {
            .AX => {
                self.data[AX_OFFSET] = lo;
                self.data[AX_OFFSET + 1] = hi;
            },
            .BX => {
                self.data[BX_OFFSET] = lo;
                self.data[BX_OFFSET + 1] = hi;
            },
            .CX => {
                self.data[CX_OFFSET] = lo;
                self.data[CX_OFFSET + 1] = hi;
            },
            .DX => {
                self.data[DX_OFFSET] = lo;
                self.data[DX_OFFSET + 1] = hi;
            },
            .SP => {
                self.data[SP_OFFSET] = lo;
                self.data[SP_OFFSET + 1] = hi;
            },
            .BP => {
                self.data[BP_OFFSET] = lo;
                self.data[BP_OFFSET + 1] = hi;
            },
            .SI => {
                self.data[SI_OFFSET] = lo;
                self.data[SI_OFFSET + 1] = hi;
            },
            .DI => {
                self.data[DI_OFFSET] = lo;
                self.data[DI_OFFSET + 1] = hi;
            },
            .ES => {
                self.data[ES_OFFSET] = lo;
                self.data[ES_OFFSET + 1] = hi;
            },
            .CS => {
                self.data[CS_OFFSET] = lo;
                self.data[CS_OFFSET + 1] = hi;
            },
            .SS => {
                self.data[SS_OFFSET] = lo;
                self.data[SS_OFFSET + 1] = hi;
            },
            .DS => {
                self.data[DS_OFFSET] = lo;
                self.data[DS_OFFSET + 1] = hi;
            },
            .IP => {
                self.data[IP_OFFSET] = lo;
                self.data[IP_OFFSET + 1] = hi;
            },
            else => unreachable,
        }
    }

    pub fn setAs(self: *Registers, T: type, dst: RegisterName, value: anytype) void {
        switch (T) {
            u8 => return self.setByte(dst, @as(T, value)),
            u16 => return self.setWord(dst, @as(T, value)),
            else => unreachable,
        }
    }

    // Get 8-bit register value
    pub fn getByte(self: *const Registers, reg: RegisterName) u8 {
        switch (reg) {
            .AL => return self.data[AX_OFFSET],
            .AH => return self.data[AX_OFFSET + 1],
            .BL => return self.data[BX_OFFSET],
            .BH => return self.data[BX_OFFSET + 1],
            .CL => return self.data[CX_OFFSET],
            .CH => return self.data[CX_OFFSET + 1],
            .DL => return self.data[DX_OFFSET],
            .DH => return self.data[DX_OFFSET + 1],
            .AX => return self.data[AX_OFFSET],
            .BX => return self.data[BX_OFFSET],
            .CX => return self.data[CX_OFFSET],
            .DX => return self.data[DX_OFFSET],
            .SP => return self.data[SP_OFFSET],
            .BP => return self.data[BP_OFFSET],
            .SI => return self.data[SI_OFFSET],
            .DI => return self.data[DI_OFFSET],
            .ES => return self.data[ES_OFFSET],
            .CS => return self.data[CS_OFFSET],
            .SS => return self.data[SS_OFFSET],
            .DS => return self.data[DS_OFFSET],
            .IP => return self.data[IP_OFFSET],
        }
    }

    // Get 16-bit register value
    pub fn getWord(self: *const Registers, reg: RegisterName) u16 {
        std.debug.assert(isWide(reg));
        var offset: usize = 0;

        switch (reg) {
            .AX => offset = AX_OFFSET,
            .BX => offset = BX_OFFSET,
            .CX => offset = CX_OFFSET,
            .DX => offset = DX_OFFSET,
            .SP => offset = SP_OFFSET,
            .BP => offset = BP_OFFSET,
            .SI => offset = SI_OFFSET,
            .DI => offset = DI_OFFSET,
            .ES => offset = ES_OFFSET,
            .CS => offset = CS_OFFSET,
            .SS => offset = SS_OFFSET,
            .DS => offset = DS_OFFSET,
            .IP => offset = IP_OFFSET,
            else => unreachable,
        }

        return @as(u16, self.data[offset]) | (@as(u16, self.data[offset + 1]) << 8);
    }

    pub fn getAs(self: *const Registers, T: type, reg: RegisterName) T {
        switch (T) {
            u8 => return self.getByte(reg),
            u16 => return self.getWord(reg),
            else => unreachable,
        }
    }
};

fn isByteRegister(reg: RegisterName) bool {
    switch (reg) {
        .AL, .AH, .BL, .BH, .CL, .CH, .DL, .DH => return true,
        else => return false,
    }
}

fn isWide(reg: RegisterName) bool {
    return !isByteRegister(reg);
}
