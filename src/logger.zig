const std = @import("std");

/// Configuration for logging colors
pub const LogColors = struct {
    err: std.io.tty.Color = .red,
    warn: std.io.tty.Color = .yellow,
    info: std.io.tty.Color = .white,
    debug: std.io.tty.Color = .dim,
};

/// Standard log function with color support
pub fn coloredLog(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const stderr = std.io.getStdErr();
    const config = std.io.tty.detectConfig(stderr);
    const colors = LogColors{};

    const color = switch (level) {
        .err => colors.err,
        .warn => colors.warn,
        .info => colors.info,
        .debug => colors.debug,
    };

    const scope_str = if (scope == .default)
        ""
    else
        "(" ++ @tagName(scope) ++ ") ";

    const prefix = switch (level) {
        .debug => "[dbg]  " ++ scope_str,
        .info => "[info] " ++ scope_str,
        .warn => "[warn] " ++ scope_str,
        .err => "[err]  " ++ scope_str,
    };

    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();

    // Set color
    config.setColor(stderr, color) catch {};
    defer config.setColor(stderr, .reset) catch {};

    // Print the message
    var bw = std.io.bufferedWriter(stderr.writer());
    const writer = bw.writer();

    nosuspend {
        writer.print(prefix ++ format ++ "\n", args) catch {};
        bw.flush() catch {};
    }
}
