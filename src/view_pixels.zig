const std = @import("std");

const dvui = @import("dvui");

const theme = @import("theme.zig");
const x86 = @import("x86.zig");

pub const MemAsTexture = struct {
    mem: x86.Memory,
    texture: ?dvui.Texture,

    start: u32,
    width: u32,
    height: u32,
    scale: f32,
    // alloc: std.mem.Allocator,

    pub fn init(mem: x86.Memory, start: u32, w: u32, h: u32, scale: f32) MemAsTexture {
        return .{
            .mem = mem,
            .texture = null,
            .start = start,
            .width = w,
            .height = h,
            .scale = scale,
        };
    }

    pub fn deinit(self: *MemAsTexture) void {
        if (self.texture) |tex| {
            dvui.textureDestroyLater(tex);
        }
    }

    pub fn draw(self: *MemAsTexture) !void {
        const pixels: [*]u8 = @ptrCast(self.mem.data[self.start .. self.width * self.height]);
        const texture = if (self.texture) |tex| tex else dvui.textureCreate(@constCast(pixels), self.width, self.height, .nearest);
        var frame_box = try dvui.box(
            @src(),
            .horizontal,
            .{
                .min_size_content = .{
                    .w = @as(f32, @floatFromInt(self.width)) * self.scale,
                    .h = @as(f32, @floatFromInt(self.height)) * self.scale,
                },
                .margin = dvui.Rect.all(4),
            },
        );
        defer frame_box.deinit();
        try dvui.renderTexture(texture, frame_box.data().contentRectScale(), .{});
    }

    pub fn update(self: *MemAsTexture, mem: x86.Memory) void {
        // updating the existing texture lacks abstraction in dvui. Easier to recreate
        if (self.texture) |tex| {
            dvui.textureDestroyLater(tex);
        }
        self.texture = null;
        self.mem = mem;
    }
};
