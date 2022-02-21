const std = @import("std");
const print = std.debug.print;
// TODO: consider dep injecting this, so i can use a test one too
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;

const Value = @import("./value.zig").Value;
const valuesEqual = @import("./value.zig").valuesEqual;
const printValue = @import("./value.zig").printValue;

const ObjManager = @import("./object.zig").ObjManager;
const ObjString = @import("./object.zig").ObjString;
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

pub const VM = struct {
    chunk: *Chunk,
    objManager: *ObjManager,
    stack: std.ArrayList(Value),
    allocator: Allocator,
    ip: usize, // instruction pointer

    pub fn init(a: Allocator) !VM {
        return VM{
            .chunk = try Chunk.init(a),
            .objManager = try ObjManager.init(a),
            .stack = std.ArrayList(Value).init(a),
            // to allow allocating more memory within compile(), e.g. to store strings
            .allocator = a,
            .ip = 0,
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

    // peek(), push(), and pop() are the core operations on the VM's stack
    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    fn push(self: *VM, v: Value) !void {
        try self.stack.append(v);
    }

    fn pop(self: *VM) Value {
        return self.stack.pop();
    }

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
            .OpAdd => try self.stack.append(Value{ .number = a.number + b.number }),
            .OpSubtract => try self.stack.append(Value{ .number = a.number - b.number }),
            .OpMultiply => try self.stack.append(Value{ .number = a.number * b.number }),
            .OpDivide => try self.stack.append(Value{ .number = a.number / b.number }),
            .OpGreater => try self.stack.append(Value{ .boolean = a.number > b.number }),
            .OpLess => try self.stack.append(Value{ .boolean = a.number < b.number }),
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
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                if (DEBUG_TRACE_EXECUTION_INCLUDE_INSTRUCTIONS) {
                    _ = self.chunk.disassembleInstruction(self.ip);
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

            const byte = self.readByte();
            const instruction = @intToEnum(OpCode, byte);

            switch (instruction) {
                .OpReturn => {
                    return InterpretResult.InterpretOk;
                },
                .OpNegate => {
                    if (!(@as(Value, self.peek(0)) == Value.number)) {
                        self.runtimeError("Negation operand must be a number.");
                        return InterpretResult.InterpretRuntimeError;
                    }
                    try self.stack.append(Value{ .number = -self.stack.pop().number });
                },
                .OpConstant => {
                    try self.stack.append(self.readConstant());
                },
                .OpFalse => {
                    try self.stack.append(Value{ .boolean = false });
                },
                .OpTrue => {
                    try self.stack.append(Value{ .boolean = true });
                },
                .OpNil => {
                    try self.stack.append(Value{ .nil = undefined });
                },
                .OpAdd => {
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
                .OpSubtract, .OpMultiply, .OpDivide, .OpGreater, .OpLess => {
                    if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        try self.binaryOp(instruction);
                    } else {
                        self.runtimeError("Operands must be two numbers.");
                        return InterpretResult.InterpretRuntimeError;
                    }
                },
                .OpNot => {
                    try self.stack.append(Value{ .boolean = self.stack.pop().isFalsey() });
                },
                .OpEqual => {
                    var b = self.stack.pop();
                    var a = self.stack.pop();
                    try self.stack.append(Value{ .boolean = valuesEqual(a, b) });
                },
                .OpPrint => {
                    printValue(self.stack.pop());
                    print("\n", .{});
                },
                .OpPop => {
                    _ = self.stack.pop();
                },
                .OpDefineGlobal => {
                    const name = self.readString();
                    try self.objManager.globals.put(name, self.peek(0));
                    _ = self.stack.pop();
                },
                .OpGetGlobal => {
                    const name = self.readString();
                    if (self.objManager.globals.get(name)) |val| {
                        try self.stack.append(val);
                    } else {
                        // TODO: dynamically create a string and pass in name
                        self.runtimeError("Undefined variable 'TODO:passVarName'.");
                        return InterpretResult.InterpretRuntimeError;
                    }
                },
                .OpSetGlobal => {
                    const name = self.readString();
                    const old = try self.objManager.globals.fetchPut(name, self.peek(0));
                    if (old) |_| {
                        // we overwrote an existing value. All good!
                    } else {
                        // we created a new global variable, but this should only assign an existing one.
                        // delete the new value
                        _ = self.objManager.globals.remove(name);
                        // TODO: dynamically create a string and pass in name
                        self.runtimeError("Undefined variable 'TODO:passVarName'.");
                        return InterpretResult.InterpretRuntimeError;
                    }
                },
                .OpGetLocal => {
                    const slot = self.readByte();
                    try self.push(self.stack.items[slot]);
                },
                .OpSetLocal => {
                    const slot = self.readByte();
                    self.stack.items[slot] = self.peek(0);
                },
            }
        }

        return InterpretResult.InterpretOk;
    }

    fn readConstant(self: *VM) Value {
        const constantIdx = self.chunk.code.items[self.ip];
        self.ip += 1;
        return self.chunk.values.items[constantIdx];
    }

    fn readByte(self: *VM) usize {
        const b = self.chunk.code.items[self.ip];
        self.ip += 1;
        return b;
    }

    fn readString(self: *VM) []const u8 {
        return self.readConstant().asCString();
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
    chunk.disassemble("chunk");

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
    chunk.disassemble("chunk");

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
    chunk.disassemble("chunk");

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

test "virtual machine can print()" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\print "hello world";
    );

    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine can uses interned strings" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\"foo";
        \\"foo";
        \\"foobar";
        \\"foo" + "bar";
        \\"foobar";
    );

    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine can define a global var" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\var x = "123";
    );

    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine can define and set a local var" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\var x = "123";
        \\{ var y = 456; y = y+1; y = x; }
    );

    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine should error if local var is redeclared within same scope" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\{
        \\var a = "first";
        \\var a = "second";
        \\}
    );

    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretCompileError);
}

test "virtual machine should error if var references itself in its initializerd" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\{
        \\var a = "outer";
        \\{
        \\var a = a;
        \\}
        \\}
    );

    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.InterpretCompileError);
}
