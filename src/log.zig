const std = @import("std");

// Returns global instance for the pretty debug printer
pub fn pretty() PrettyDebugPrinter {
    const state = struct {
        var printer: PrettyDebugPrinter = undefined;
        var is_initialized = false;
    };
    if (!state.is_initialized) {
        state.printer = PrettyDebugPrinter{
            .cfg = std.io.tty.detectConfig(std.io.getStdErr()),
            .color = null,
        };
    }
    return state.printer;
}

// Prints debug messages to stderr
const PrettyDebugPrinter = struct {
    cfg: std.io.tty.Config,
    color: ?std.io.tty.Color,

    pub fn withColor(
        self: PrettyDebugPrinter,
        color: std.io.tty.Color,
    ) PrettyDebugPrinter {
        return PrettyDebugPrinter{
            .cfg = self.cfg,
            .color = color,
        };
    }

    pub fn print(
        self: PrettyDebugPrinter,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        if (self.color != null) self.cfg.setColor(std.io.getStdErr(), self.color.?) catch return;
        std.debug.print(fmt, args);
        if (self.color != null) self.cfg.setColor(std.io.getStdErr(), .reset) catch return;
    }
};
