const std = @import("std");
const print = std.debug.print;
// TODO: consider dep injecting this, so i can use a test one too
const allocator = std.heap.page_allocator;
const expect = std.testing.expect;

const interpreter = @import("./vm.zig");
const Chunk = @import("./chunk.zig").Chunk;
const Value = @import("./value.zig").Value;

pub fn main() !void {
    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(usize).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };
    const vm = interpreter.VM{
        .chunk = chunk,
        .stack = &std.ArrayList(Value).init(allocator),
    };
    defer vm.free();

    // parse args
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const ally = &arena.allocator;

    const stdout = std.io.getStdOut().writer();

    const args = try std.process.argsAlloc(ally);
    if (args.len > 2) {
        print("Usage: zlox [path]\n", .{});
        std.os.exit(64);
        // try stdout.writeAll(usage);
        // return;
    } else if (args.len == 2) {
        try runFile(args[1]);
    } else {
        try repl();
    }
}

const maxFileSize: usize = 1024;
fn runFile(path: []u8) !void {
    // TODO: handle error: FileNotFound
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source: []u8 = try file.readToEndAlloc(allocator, maxFileSize);

    const result: interpreter.InterpretResult = try interpreter.interpret(source);

    if (result == interpreter.InterpretResult.InterpretCompileError) std.os.exit(65);
    if (result == interpreter.InterpretResult.InterpretRuntimeError) std.os.exit(70);
}

fn repl() !void {
    const line = try allocator.alloc(u8, 1024);
    while (true) {
        print("> ", .{});
        const stdin = std.io.getStdIn().reader();
        const optional_s = try stdin.readUntilDelimiterOrEof(line[0..], '\n'); // TODO: break if error
        if (optional_s) |s| {
            _ = try interpreter.interpret(s);
            print("\n", .{});
        } else {
            break;
        }
    }
    return;
}
