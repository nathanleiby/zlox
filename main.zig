const std = @import("std");
const print = std.debug.print;
// TODO: consider dep injecting this, so i can use a test one too
const allocator = std.heap.page_allocator;
const expect = std.testing.expect;

const DEBUG_TRACE_EXECUTION = true; // debug mode

const OpCode = enum (usize) {
    OpReturn,
    OpConstant,
    OpNegate,
};

const InterpretResult = enum {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
};

const Chunk = struct {
    code: *std.ArrayList(usize),
    lines: *std.ArrayList(i32),
    values: *std.ArrayList(f64),

    pub fn free(self: Chunk) void {
        self.code.deinit();
        self.lines.deinit();
    }

    pub fn write(self: Chunk, byte: usize, line: i32) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn addConstant(self: Chunk, value: f64) !usize {
        try self.values.append(value);
        return self.values.items.len - 1;
    }
};

const VM = struct {
    chunk: Chunk,
    stack: *std.ArrayList(f64),

    // ip is the instruction pointer. it points to the instruction about to be executed; not the one currently being handled
    // ip: usize = 0,
    // ticker: usize = 0,

    pub fn interpret(self: VM, _: *Chunk) !InterpretResult {
        // var foo = c;
        // self.ip = c.code;
        return self.run();
    }

    fn run (self: VM) !InterpretResult {
        var ip: usize = 0; // instruction pointer
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                // TODO: what is offset here?
                _ = disassembleInstruction(self.chunk, ip);
            }

            const byte = self.chunk.code.items[ip];
            const instruction = @intToEnum(OpCode, byte);
            ip += 1;
            switch (instruction) {
                OpCode.OpReturn => {
                    // TODO
                    print("{d}", .{self.stack.pop()});
                    print("\n", .{});
                    return InterpretResult.InterpretOk;
                },
                OpCode.OpNegate => {
                    try self.stack.append(-self.stack.pop());
                },
                OpCode.OpConstant => {
                    // READ_CONSTANT()
                    const constantIdx = self.chunk.code.items[ip];
                    ip += 1;
                    const constant = self.chunk.values.items[constantIdx];
                    try self.stack.append(constant);
                    // printValue(constant)
                    print("{d}", .{constant});
                    print("\n", .{});
                },
                // else => {
                //     return InterpretResult.InterpretCompileError;
                // }
            }
        }

        // TODO
        return InterpretResult.InterpretOk;
    }
};

fn disassembleChunk(chunk: Chunk) void {
    print("== {s} ==\n", .{"test chunk"});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
    const byte = chunk.code.items[offset];
    print("{:04} ", .{offset});
    // line
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{:04} ", .{chunk.lines.items[offset]});
    }

    const item = @intToEnum(OpCode, byte);
    switch (item) {
        OpCode.OpReturn => {
            print("OP_RETURN            ", .{});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpConstant=> {
            print("OP_CONSTANT          ", .{});
            // TODO
            const constantIdx = chunk.code.items[offset + 1];
            print("{:04} -- ", .{constantIdx});
            print("{d}", .{chunk.values.items[constantIdx]});
            print("\n", .{});
            return offset + 2;
        },
        OpCode.OpNegate=> {
            print("OP_NEGATE            ", .{});
            // TODO
            const constantIdx = chunk.code.items[offset + 1];
            print("{:04} -- ", .{constantIdx});
            print("{d}", .{chunk.values.items[constantIdx]});
            print("\n", .{});
            return offset + 2;
        },
    }
}

pub fn main() !void {


}

// TODO: Write a unit test for chunks

test "virtual machine can negate a value" {
    print("\n\n", .{}); // make space for test runner output

    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(i32).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };

    // free
    defer chunk.free();

    // write
    try chunk.write(@enumToInt(OpCode.OpConstant), 123);
    const constant = try chunk.addConstant(1.2);
    try chunk.write(constant, 1);

    try chunk.write(@enumToInt(OpCode.OpNegate), 123);

    try chunk.write(@enumToInt(OpCode.OpReturn), 123);

    // disassemble
    disassembleChunk(chunk);

    // interpret
    const vm = VM{
        .chunk = chunk,
        .stack = &std.ArrayList(f64).init(allocator),
    };
    const result = try vm.interpret(&chunk);
    print("Interpret result: {s}\n", .{result});
    try expect(result == InterpretResult.InterpretOk);
}
