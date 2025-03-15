const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Library modules
    const x86_module = b.createModule(.{
        .root_source_file = b.path("src/x86.zig"),
        .target = target,
        .optimize = optimize,
    });

    const app_module = b.createModule(.{
        .root_source_file = b.path("src/app.zig"),
        .target = target,
        .optimize = optimize,
    });
    app_module.addImport("x86", x86_module);

    // Exe modules
    const main_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    main_module.addImport("x86", x86_module);
    main_module.addImport("x86", app_module);

    const test_module = b.createModule(.{
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_module.addImport("x86", x86_module);

    // Main executable
    const main_exe = b.addExecutable(.{
        .name = "sim8086",
        .root_module = main_module,
    });
    const no_bin = b.option(bool, "no-bin", "skip emitting binary") orelse false;
    if (no_bin) {
        b.getInstallStep().dependOn(&main_exe.step);
    } else {
        b.installArtifact(main_exe);
    }
    addCheckStep(b, main_module);

    // run: main
    const main_cmd = b.addRunArtifact(main_exe);
    if (b.args) |args| {
        main_cmd.addArgs(args);
    }
    main_cmd.step.dependOn(b.getInstallStep());
    const main_step = b.step("sim8086", "Run the app");
    main_step.dependOn(&main_cmd.step);

    // run: test
    const test_cmd = b.addRunArtifact(b.addTest(.{
        .root_module = test_module,
    }));
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&test_cmd.step);
}

fn addCheckStep(b: *std.Build, module: ?*std.Build.Module) void {
    const exe_check = b.addExecutable(.{
        .name = "compile-check",
        .root_module = module,
    });
    const check = b.step("check", "Check if main compiles");
    check.dependOn(&exe_check.step);
}
