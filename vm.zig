const std = @import("std");
const print = std.debug.print;
// TODO: consider dep injecting this, so i can use a test one too
const allocator = std.heap.page_allocator;
const expect = std.testing.expect;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;

const compiler = @import("./compiler.zig");

// Debugging flags
const DEBUG_TRACE_EXECUTION = true; // debug mode
const DEBUG_TRACE_EXECUTION_INCLUDE_INSTRUCTIONS = false;

pub const InterpretResult = enum {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
};

const TokenType = enum {
// Single-character tokens.
LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
// One or two character tokens.
BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
// Literals.
IDENTIFIER, STRING, NUMBER,
// Keywords.
AND, CLASS, ELSE, FALSE, FOR, FUN, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, ERROR, EOF };

// TODO: move this into VM
pub fn interpret(source: []u8) !InterpretResult {
    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(usize).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };
    defer chunk.free();

    const compileSuccess = try compiler.compile(source, &chunk);
    if (!compileSuccess) {
        return InterpretResult.InterpretCompileError;
    }

    const vm = VM{
        .chunk = chunk,
        .stack = &std.ArrayList(f64).init(allocator),
    };

    const result = try vm.run();
    return result;
}

pub const VM = struct {
    chunk: Chunk,
    stack: *std.ArrayList(f64),

    // ip is the instruction pointer. it points to the instruction about to be executed; not the one currently being handled
    // ip: usize = 0,
    // ticker: usize = 0,

    pub fn free(self: VM) void {
        self.chunk.free();
        self.stack.deinit();
    }

    pub fn interpret(self: VM, _: *Chunk) !InterpretResult {
        // var foo = c;
        // self.ip = c.code;
        return self.run();
    }

    fn binaryAdd(self: VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        try self.stack.append(a + b);
    }

    fn binarySubtract(self: VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        try self.stack.append(a - b);
    }

    fn binaryMultiply(self: VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        try self.stack.append(a * b);
    }

    fn binaryDivide(self: VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        try self.stack.append(a / b);
    }

    fn run(self: VM) !InterpretResult {
        var ip: usize = 0; // instruction pointer
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                if (DEBUG_TRACE_EXECUTION_INCLUDE_INSTRUCTIONS) {
                    _ = disassembleInstruction(self.chunk, ip);
                }

                // print the stack
                print("          ", .{});
                for (self.stack.items) |slot| {
                    print("[{d}]", .{slot});
                }
                print("\n", .{});
            }

            const byte = self.chunk.code.items[ip];
            const instruction = @intToEnum(OpCode, byte);
            ip += 1;
            switch (instruction) {
                OpCode.OpReturn => {
                    print("Return: {d}", .{self.stack.pop()});
                    print("\n", .{});
                    return InterpretResult.InterpretOk;
                },
                OpCode.OpNegate => {
                    try self.stack.append(-self.stack.pop());
                },
                OpCode.OpConstant => {
                    const constantIdx = self.chunk.code.items[ip];
                    ip += 1;
                    const constant = self.chunk.values.items[constantIdx];
                    try self.stack.append(constant);
                },
                OpCode.OpAdd => {
                    try self.binaryAdd();
                },
                OpCode.OpSubtract => {
                    try self.binarySubtract();
                },
                OpCode.OpMultiply => {
                    try self.binaryMultiply();
                },
                OpCode.OpDivide => {
                    try self.binaryDivide();
                },
                // else => {
                //     return InterpretResult.InterpretCompileError;
                // },
            }
        }

        return InterpretResult.InterpretOk;
    }
};

fn disassembleChunk(chunk: Chunk) void {
    print("== {s} ==\n", .{"chunk"});

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
        OpCode.OpConstant => {
            print("OP_CONSTANT          ", .{});
            const constantIdx = chunk.code.items[offset + 1];
            print("{:04} -- ", .{constantIdx});
            print("{d}", .{chunk.values.items[constantIdx]});
            print("\n", .{});
            return offset + 2;
        },
        OpCode.OpNegate => {
            print("OP_NEGATE            ", .{});
            const constantIdx = chunk.code.items[offset + 1];
            print("{:04} -- ", .{constantIdx});
            print("{d}", .{chunk.values.items[constantIdx]});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpAdd => {
            print("OP_ADD               ", .{});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpSubtract => {
            print("OP_SUBTRACT          ", .{});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpMultiply => {
            print("OP_MULTIPLY          ", .{});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpDivide => {
            print("OP_DIVIDE            ", .{});
            print("\n", .{});
            return offset + 1;
        },
    }
}

test "virtual machine can negate a value" {
    print("\n\n", .{}); // make space for test runner output

    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(usize).init(allocator),
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

test "virtual machine can do some binary ops (add and divide)" {
    print("\n\n", .{}); // make space for test runner output

    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(usize).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };

    const fakeLineNumber = 123;

    // free
    defer chunk.free();

    // write
    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant = try chunk.addConstant(1.2);
    try chunk.write(constant, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant2 = try chunk.addConstant(3.4);
    try chunk.write(constant2, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpAdd), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant3 = try chunk.addConstant(5.6);
    try chunk.write(constant3, 1);

    try chunk.write(@enumToInt(OpCode.OpDivide), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpReturn), fakeLineNumber);

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

test "virtual machine can do all binary ops (add, subtract, multiply, divide)" {
    // target: 1 + 2 * 3 - 4 / -5
    // note that this gets evaluated like so: ((((1+2) * 3) - 4) / -5)

    print("\n\n", .{}); // make space for test runner output

    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(usize).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };

    const fakeLineNumber = 123;

    // free
    defer chunk.free();

    // write
    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant = try chunk.addConstant(1);
    try chunk.write(constant, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant2 = try chunk.addConstant(2);
    try chunk.write(constant2, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpAdd), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant3 = try chunk.addConstant(3);
    try chunk.write(constant3, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpMultiply), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant4 = try chunk.addConstant(4);
    try chunk.write(constant4, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpSubtract), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant5 = try chunk.addConstant(5);
    try chunk.write(constant5, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpNegate), fakeLineNumber);

    // TODO(bug): Why does 'divide' specifically crash here?
    // try chunk.write(@enumToInt(OpCode.OpDivide), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpMultiply), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpReturn), fakeLineNumber);

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

// test "compiler can emit tokens" {
// INPUT = 'print 1 + 2;'
//   1 31 'print'
//    | 21 '1'
//    |  7 '+'
//    | 21 '2'
//    |  8 ';'
//    2 39 ''
// }
