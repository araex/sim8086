const dvui = @import("dvui");
const Color = dvui.Color;
const Font = dvui.Font;
const Theme = dvui.Theme;
const Options = dvui.Options;
const Window = dvui.Window;

const FiraCode = struct {
    const name = "FiraCode";
    const data = @embedFile("assets/font/FiraCode-Regular.ttf");
};
const FiraCodeBd = struct {
    const name = "FiraCodeBd";
    const data = @embedFile("assets/font/FiraCode-Bold.ttf");
};

pub fn install(win: *Window) !void {
    try win.begin(0);

    try dvui.addFont(FiraCode.name, FiraCode.data, null);
    try dvui.addFont(FiraCodeBd.name, FiraCodeBd.data, null);
    try win.themes.putNoClobber("sim8086", @import("theme.zig").dark);
    dvui.themeSet(&(win.themes.get("sim8086").?));

    _ = try win.end(.{});
}

const accent = Color{ .r = 0x35, .g = 0x84, .b = 0xe4 };
const accent_hsl = Color.HSLuv.fromColor(accent);
const err = Color{ .r = 0xe0, .g = 0x1b, .b = 0x24 };
const err_hsl = Color.HSLuv.fromColor(err);

const dark_fill = Color{ .r = 0x1e, .g = 0x1e, .b = 0x1e };
const dark_fill_hsl = Color.HSLuv.fromColor(dark_fill);
const dark_err = Color{ .r = 0xc0, .g = 0x1c, .b = 0x28 };
const dark_err_hsl = Color.HSLuv.fromColor(dark_err);

const dark_accent_accent = accent_hsl.lighten(12).color();
const dark_accent_fill_hover = accent_hsl.lighten(9).color();
const dark_accent_border = accent_hsl.lighten(17).color();

const dark_err_accent = dark_err_hsl.lighten(14).color();
const dark_err_fill_hover = err_hsl.lighten(9).color();
const dark_err_fill_press = err_hsl.lighten(16).color();
const dark_err_border = err_hsl.lighten(20).color();

pub const dark = dark: {
    @setEvalBranchQuota(3123);
    break :dark Theme{
        .name = "Dark",
        .dark = true,

        .font_body = .{ .size = 16, .name = FiraCode.name },
        .font_heading = .{ .size = 16, .name = FiraCodeBd.name },
        .font_caption = .{ .size = 13, .name = FiraCode.name, .line_height_factor = 1.1 },
        .font_caption_heading = .{ .size = 13, .name = FiraCodeBd.name, .line_height_factor = 1.1 },
        .font_title = .{ .size = 28, .name = FiraCode.name },
        .font_title_1 = .{ .size = 24, .name = FiraCodeBd.name },
        .font_title_2 = .{ .size = 22, .name = FiraCodeBd.name },
        .font_title_3 = .{ .size = 20, .name = FiraCodeBd.name },
        .font_title_4 = .{ .size = 18, .name = FiraCodeBd.name },

        .color_accent = accent_hsl.color(),
        .color_err = dark_err,
        .color_text = Color.white,
        .color_text_press = Color.white,
        .color_fill = dark_fill,
        .color_fill_window = .{ .r = 0x2b, .g = 0x2b, .b = 0x2b },
        .color_fill_control = .{ .r = 0x40, .g = 0x40, .b = 0x40 },
        .color_fill_hover = dark_fill_hsl.lighten(21).color(),
        .color_fill_press = dark_fill_hsl.lighten(30).color(),
        .color_border = dark_fill_hsl.lighten(39).color(),

        .style_accent = Options{
            .color_accent = .{ .color = dark_accent_accent },
            .color_text = .{ .color = Color.white },
            .color_text_press = .{ .color = Color.white },
            .color_fill = .{ .color = accent },
            .color_fill_hover = .{ .color = dark_accent_fill_hover },
            .color_fill_press = .{ .color = dark_accent_accent },
            .color_border = .{ .color = dark_accent_border },
        },

        .style_err = Options{
            .color_accent = .{ .color = dark_err_accent },
            .color_text = .{ .color = Color.white },
            .color_text_press = .{ .color = Color.white },
            .color_fill = .{ .color = dark_err },
            .color_fill_hover = .{ .color = dark_err_fill_hover },
            .color_fill_press = .{ .color = dark_err_fill_press },
            .color_border = .{ .color = dark_err_border },
        },
    };
};
