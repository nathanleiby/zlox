const std = @import("std");
const print = std.debug.print;
// TODO: consider dep injecting this, so i can use a test one too
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const Chunk = @import("./chunk.zig").Chunk;
const disassembleChunk = @import("./chunk.zig").disassembleChunk;
const disassembleInstruction = @import("./chunk.zig").disassembleInstruction;
const OpCode = @import("./chunk.zig").OpCode;

const Value = @import("./value.zig").Value;
const valuesEqual = @import("./value.zig").valuesEqual;
const printValue = @import("./value.zig").printValue;

const ObjManager = @import("./object.zig").ObjManager;
const compiler = @import("./compiler.zig");
const concat = @import("./memory.zig").concat;

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
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    ERROR,
    EOF,
};

pub const VM = struct {
    chunk: *Chunk,
    objManager: *ObjManager,
    stack: std.ArrayList(Value),
    allocator: Allocator,

    pub fn init(a: Allocator) !VM {
        return VM{
            .chunk = try Chunk.init(a),
            .objManager = try ObjManager.init(a),
            .stack = std.ArrayList(Value).init(a),
            // to allow allocating more memory within compile(), e.g. to store strings
            .allocator = a,
        };
    }

    pub fn free(self: *VM) void {
        self.chunk.free();
        self.allocator.destroy(self.chunk);
        self.stack.deinit();
        self.objManager.free();
    }

    pub fn interpret(self: *VM, source: []u8) !InterpretResult {
        const compileSuccess = try compiler.compile(source, self.chunk, self.objManager);
        if (!compileSuccess) {
            return InterpretResult.InterpretCompileError;
        }

        const result = try self.run();
        return result;
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    // fn push(self: *VM, v: Value) !void {
    //     try self.stack.append(v);
    // }

    // fn pop(self: *VM) Value {
    //     return self.stack.pop();
    // }

    fn isValidBinaryOp(self: *VM) bool {
        if (!(self.peek(0).isNumber() and self.peek(1).isNumber())) {
            return false;
        }
        return true;
    }

    fn binaryOp(self: *VM, op: OpCode) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        switch (op) {
            OpCode.OpAdd => try self.stack.append(Value{ .number = a.number + b.number }),
            OpCode.OpSubtract => try self.stack.append(Value{ .number = a.number - b.number }),
            OpCode.OpMultiply => try self.stack.append(Value{ .number = a.number * b.number }),
            OpCode.OpDivide => try self.stack.append(Value{ .number = a.number / b.number }),
            OpCode.OpGreater => try self.stack.append(Value{ .boolean = a.number > b.number }),
            OpCode.OpLess => try self.stack.append(Value{ .boolean = a.number < b.number }),
            else => unreachable,
        }
    }

    // TODO: Find where this was defined in book
    fn resetStack(_: *VM) void {
        // TODO
        // ...
        // self.stack.deinit();
        // self.stack.init();
    }

    //static void runtimeError(const char* format, ...) {
    fn runtimeError(_: *VM, _: []const u8) void {
        // va_list args;
        // va_start(args, format);
        // vfprintf(stderr, format, args);
        // va_end(args);
        // fputs("\n", stderr);

        // size_t instruction = vm.ip - vm.chunk->code - 1;
        // int line = vm.chunk->lines[instruction];
        // fprintf(stderr, "[line %d] in script\n", line);
        // resetStack();
    }

    fn run(self: *VM) !InterpretResult {
        var ip: usize = 0; // instruction pointer
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                if (DEBUG_TRACE_EXECUTION_INCLUDE_INSTRUCTIONS) {
                    _ = disassembleInstruction(self.chunk, ip);
                }

                // print the stack
                print("          ", .{});
                for (self.stack.items) |slot| {
                    print("[", .{});
                    printValue(slot);
                    print("]", .{});
                }
                print("\n", .{});
            }

            const byte = self.chunk.code.items[ip];
            const instruction = @intToEnum(OpCode, byte);
            ip += 1;
            switch (instruction) {
                OpCode.OpReturn => {
                    var retVal = self.stack.pop();
                    print("Return: ", .{});
                    printValue(retVal);
                    print("\n", .{});
                    return InterpretResult.InterpretOk;
                },
                OpCode.OpNegate => {
                    if (!(@as(Value, self.peek(0)) == Value.number)) {
                        self.runtimeError("Negation operand must be a number.");
                        return InterpretResult.InterpretRuntimeError;
                    }
                    try self.stack.append(Value{ .number = -self.stack.pop().number });
                },
                OpCode.OpConstant => {
                    const constantIdx = self.chunk.code.items[ip];
                    ip += 1;
                    const constant = self.chunk.values.items[constantIdx];
                    try self.stack.append(constant);
                },
                OpCode.OpFalse => {
                    try self.stack.append(Value{ .boolean = false });
                },
                OpCode.OpTrue => {
                    try self.stack.append(Value{ .boolean = true });
                },
                OpCode.OpNil => {
                    try self.stack.append(Value{ .nil = undefined });
                },
                OpCode.OpAdd => {
                    // + supports adding numbers or concatenating strings
                    if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        try self.binaryOp(instruction);
                    } else if (self.peek(0).isString() and self.peek(1).isString()) {
                        const b = self.stack.pop();
                        const a = self.stack.pop();
                        try self.concatenate(a.asCString(), b.asCString());
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings.");
                        return InterpretResult.InterpretRuntimeError;
                    }
                },
                OpCode.OpSubtract, OpCode.OpMultiply, OpCode.OpDivide, OpCode.OpGreater, OpCode.OpLess => {
                    if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        try self.binaryOp(instruction);
                    } else {
                        self.runtimeError("Operands must be two numbers.");
                        return InterpretResult.InterpretRuntimeError;
                    }
                },
                OpCode.OpNot => {
                    try self.stack.append(Value{ .boolean = self.stack.pop().isFalsey() });
                },
                OpCode.OpEqual => {
                    var b = self.stack.pop();
                    var a = self.stack.pop();
                    try self.stack.append(Value{ .boolean = valuesEqual(a, b) });
                },
            }
        }

        return InterpretResult.InterpretOk;
    }

    fn concatenate(self: *VM, a: []const u8, b: []const u8) !void {
        const result = try concat(self.allocator, a, b);
        var s = try self.objManager.takeString(result);
        const v = Value{ .objString = s };
        try self.stack.append(v);
    }
};

test "virtual machine can negate a value" {
    const testAllocator = std.testing.allocator;
    print("\n\n", .{}); // make space for test runner output

    // Setup VM
    var vm = try VM.init(testAllocator);
    defer vm.free();

    // Manually modify chunk
    var chunk = vm.chunk;

    const fakeLineNum = 123;

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNum);
    const constant = try chunk.addConstant(Value{ .number = 1.2 });
    try chunk.write(constant, 1);

    try chunk.write(@enumToInt(OpCode.OpNegate), fakeLineNum);

    try chunk.write(@enumToInt(OpCode.OpReturn), fakeLineNum);

    // disassemble
    disassembleChunk(chunk.*, "chunk");

    // interpret
    const result = try vm.run();
    print("Interpret result: {s}\n", .{result});
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine can do some binary ops (add and divide)" {
    const testAllocator = std.testing.allocator;
    print("\n\n", .{}); // make space for test runner output

    // Setup VM
    var vm = try VM.init(testAllocator);
    defer vm.free();

    // Manually modify chunk
    var chunk = vm.chunk;

    const fakeLineNumber = 123;

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant = try chunk.addConstant(Value{ .number = 1.2 });
    try chunk.write(constant, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant2 = try chunk.addConstant(Value{ .number = 3.4 });
    try chunk.write(constant2, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpAdd), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant3 = try chunk.addConstant(Value{ .number = 5.6 });
    try chunk.write(constant3, 1);

    try chunk.write(@enumToInt(OpCode.OpDivide), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpReturn), fakeLineNumber);

    // disassemble
    disassembleChunk(chunk.*, "chunk");

    // interpret
    const result = try vm.run();
    print("Interpret result: {s}\n", .{result});
    try expect(result == InterpretResult.InterpretOk);
}

// // TODO
// // target: 1 + 2 * 3 - 4 / -5
// // note that this gets evaluated like so: ((((1+2) * 3) - 4) / -5)
test "virtual machine can do all binary ops (add, subtract, multiply, divide)" {
    const testAllocator = std.testing.allocator;
    print("\n\n", .{}); // make space for test runner output

    // Setup VM
    var vm = try VM.init(testAllocator);
    defer vm.free();

    // Manually modify chunk
    var chunk = vm.chunk;

    const fakeLineNumber = 123;

    // write
    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant = try chunk.addConstant(Value{ .number = 1 });
    try chunk.write(constant, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant2 = try chunk.addConstant(Value{ .number = 2 });
    try chunk.write(constant2, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpAdd), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpReturn), fakeLineNumber);

    // disassemble
    disassembleChunk(chunk.*, "chunk");

    // interpret
    const result = try vm.run();
    print("Interpret result: {s}\n", .{result});
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine can run minimal program" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\1;
    );
    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretOk);
}

// // TODO: figure out why trailing -5 doesn't work
// // target: 1 + 2 * 3 - 4 / -5
// // note that this gets evaluated like so: ((((1+2) * 3) - 4) / -5)
test "virtual machine can do arithmetic" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\1 + 2 * 3 - 4 / (5);
    );
    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine can work with strings" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\"hello";
    );
    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine can concat strings" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\"foo" + "bar" + "baz";
    );
    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretOk);
}
