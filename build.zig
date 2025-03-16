const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // 3rd party dependencies
    const dvui_dep = b.dependency("dvui", .{
        .target = target,
        .optimize = optimize,
        .sdl3 = true,
    });

    // Library modules
    const x86_module = b.createModule(.{
        .root_source_file = b.path("src/x86.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Exe modules
    const main_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    main_module.addImport("x86", x86_module);
    main_module.addImport("dvui", dvui_dep.module("dvui_sdl"));

    const test_module = b.createModule(.{
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_module.addImport("x86", x86_module);

    // main exe
    const main_exe = b.addExecutable(.{
        .name = "sim8086",
        .root_module = main_module,
    });

    // install exe
    const install_exe = b.addInstallArtifact(main_exe, .{});
    b.getInstallStep().dependOn(&install_exe.step);
    addCheckStep(b, main_module);

    // run: main
    const main_cmd = b.addRunArtifact(main_exe);
    if (b.args) |args| {
        main_cmd.addArgs(args);
    }
    main_cmd.step.dependOn(&install_exe.step);
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
        .name = "check",
        .root_module = module,
    });
    const check = b.step("check", "Check if main compiles");
    check.dependOn(&exe_check.step);
}
