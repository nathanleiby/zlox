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
const ObjFunction = @import("./object.zig").ObjFunction;

const compiler = @import("./compiler.zig");
const concat = @import("./memory.zig").concat;
const U8_COUNT = @import("./constants.zig").U8_COUNT;

// Debugging flags
const DEBUG_TRACE_EXECUTION = true; // debug mode
const DEBUG_TRACE_EXECUTION_INCLUDE_INSTRUCTIONS = false;
const DEBUG_TRACE_EXECUTION_SHOW_GET_SET_VARS = false;
const DEBUG_TRACE_EXECUTION_PRINT_STACK = false;

pub const InterpretResult = enum {
    Ok,
    CompileError,
    RuntimeError,
};

const CallFrame = struct {
    // function being called
    function: *ObjFunction,
    // caller stores it's own ip
    ip: usize,
    // points into the VM’s value stack at the first slot that this function can use
    // this is implemented as via a pointer in the book (`slots: *Value`); I'm using an index into the stack
    slotOffset: usize,
};

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * U8_COUNT;

pub const VM = struct {
    chunk: *Chunk,
    objManager: *ObjManager,
    stack: std.ArrayList(Value),
    frames: [FRAMES_MAX]CallFrame,
    frameCount: u8,
    frame: *CallFrame,
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
            .frames = [_]CallFrame{CallFrame{ .function = undefined, .ip = 0, .slotOffset = 0 }} ** FRAMES_MAX,
            .frameCount = 0,
            .frame = undefined,
        };
    }

    pub fn free(self: *VM) void {
        self.chunk.free();
        self.allocator.destroy(self.chunk);
        self.stack.deinit();
        self.objManager.free();
    }

    pub fn interpret(self: *VM, source: []u8) !InterpretResult {
        const function = compiler.compile(source, self.objManager) catch return InterpretResult.CompileError;

        const fnVal = Value{ .objFunction = function };
        try self.push(fnVal);

        var frame: *CallFrame = &self.frames[self.frameCount];
        self.frameCount += 1;
        frame.function = function;
        frame.ip = 0;
        frame.slotOffset = self.stack.items.len - 1; // TODO: not sure about this

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
            .Add => try self.stack.append(Value{ .number = a.number + b.number }),
            .Subtract => try self.stack.append(Value{ .number = a.number - b.number }),
            .Multiply => try self.stack.append(Value{ .number = a.number * b.number }),
            .Divide => try self.stack.append(Value{ .number = a.number / b.number }),
            .Greater => try self.stack.append(Value{ .boolean = a.number > b.number }),
            .Less => try self.stack.append(Value{ .boolean = a.number < b.number }),
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
    fn runtimeError(self: *VM, message: []const u8) void {
        print("runtimeError: {s}\n", .{message});
        const instruction = self.frame.ip;
        const line = self.chunk.lines.items[instruction];
        print("[line {d}] in script\n", .{line});
        // resetStack();
    }

    fn run(self: *VM) !InterpretResult {
        self.frame = &self.frames[self.frameCount - 1];
        self.chunk = self.frame.function.chunk; // TODO: Could also change how we refer to it below

        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                if (DEBUG_TRACE_EXECUTION_INCLUDE_INSTRUCTIONS) {
                    _ = self.frame.function.chunk.disassembleInstruction(self.frame.ip);
                }

                if (DEBUG_TRACE_EXECUTION_PRINT_STACK) {
                    print("          ", .{});
                    for (self.stack.items) |slot| {
                        print("[", .{});
                        printValue(slot);
                        print("]", .{});
                    }
                    print("\n", .{});
                }
            }

            const byte = self.readByte();
            const instruction = @intToEnum(OpCode, byte);

            switch (instruction) {
                .Return => {
                    return InterpretResult.Ok;
                },
                .Negate => {
                    if (!(@as(Value, self.peek(0)) == Value.number)) {
                        self.runtimeError("Negation operand must be a number.");
                        return InterpretResult.RuntimeError;
                    }
                    try self.stack.append(Value{ .number = -self.stack.pop().number });
                },
                .Constant => {
                    try self.stack.append(self.readConstant());
                },
                .False => {
                    try self.stack.append(Value{ .boolean = false });
                },
                .True => {
                    try self.stack.append(Value{ .boolean = true });
                },
                .Nil => {
                    try self.stack.append(Value{ .nil = undefined });
                },
                .Add => {
                    // + supports adding numbers or concatenating strings
                    if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        try self.binaryOp(instruction);
                    } else if (self.peek(0).isString() and self.peek(1).isString()) {
                        const b = self.stack.pop();
                        const a = self.stack.pop();
                        try self.concatenate(a.asCString(), b.asCString());
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings.");
                        return InterpretResult.RuntimeError;
                    }
                },
                .Subtract, .Multiply, .Divide, .Greater, .Less => {
                    if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        try self.binaryOp(instruction);
                    } else {
                        self.runtimeError("Operands must be two numbers.");
                        return InterpretResult.RuntimeError;
                    }
                },
                .Not => {
                    try self.stack.append(Value{ .boolean = self.stack.pop().isFalsey() });
                },
                .Equal => {
                    var b = self.stack.pop();
                    var a = self.stack.pop();
                    try self.stack.append(Value{ .boolean = valuesEqual(a, b) });
                },
                .Print => {
                    printValue(self.stack.pop());
                    print("\n", .{});
                },
                .Pop => {
                    _ = self.stack.pop();
                },
                .DefineGlobal => {
                    const name = self.readString();
                    try self.objManager.globals.put(name, self.peek(0));
                    _ = self.stack.pop();
                },
                .GetGlobal => {
                    const name = self.readString();
                    if (self.objManager.globals.get(name)) |val| {
                        try self.stack.append(val);
                    } else {
                        // TODO: dynamically create a string and pass in name
                        self.runtimeError("Undefined variable 'TODO:passVarName'.");
                        return InterpretResult.RuntimeError;
                    }
                },
                .SetGlobal => {
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
                        return InterpretResult.RuntimeError;
                    }
                },
                .GetLocal => {
                    const slot = self.readByte();
                    // TODO: remove debugging
                    if (DEBUG_TRACE_EXECUTION and DEBUG_TRACE_EXECUTION_SHOW_GET_SET_VARS) {
                        print(".GetLocal slot = {any}, slotOffset = {any}, stack.items.len = {any}\n", .{ slot, self.frame.slotOffset, self.stack.items.len });
                    }
                    try self.push(self.stack.items[self.frame.slotOffset + slot]);
                },
                .SetLocal => {
                    const slot = self.readByte();
                    if (DEBUG_TRACE_EXECUTION and DEBUG_TRACE_EXECUTION_SHOW_GET_SET_VARS) {
                        print(".SetLocal slot = {any}, slotOffset = {any}, stack.items.len = {any}\n", .{ slot, self.frame.slotOffset, self.stack.items.len });
                    }
                    self.stack.items[self.frame.slotOffset + slot] = self.peek(0);
                },
                .Jump => {
                    const offset = self.readShort();
                    // jump forward
                    self.frame.ip += offset;
                },
                .JumpIfFalse => {
                    const offset = self.readShort();
                    if (self.peek(0).isFalsey()) self.frame.ip += offset;
                },
                .Loop => {
                    const offset = self.readShort();
                    // jump backward
                    self.frame.ip -= offset;
                },
            }
        }

        return InterpretResult.Ok;
    }

    fn readConstant(self: *VM) Value {
        const constantIdx = self.chunk.code.items[self.frame.ip];
        self.frame.ip += 1;
        return self.chunk.values.items[constantIdx];
    }

    fn readByte(self: *VM) usize {
        const byte = self.chunk.code.items[self.frame.ip];
        self.frame.ip += 1;
        return byte;
    }

    fn readShort(self: *VM) usize {
        const short = @intCast(u16, (self.chunk.code.items[self.frame.ip] << 8) | self.chunk.code.items[self.frame.ip + 1]);
        self.frame.ip += 2;
        return short;
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

test "virtual machine can run minimal program" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\1;
    );
    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.Ok);
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
    try expect(result == InterpretResult.Ok);
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
    try expect(result == InterpretResult.Ok);
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
    try expect(result == InterpretResult.Ok);
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
    try expect(result == InterpretResult.Ok);
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
    try expect(result == InterpretResult.Ok);
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
    try expect(result == InterpretResult.Ok);
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
    try expect(result == InterpretResult.Ok);
}

// test "virtual machine should error if local var is redeclared within same scope" {
//     const testAllocator = std.heap.page_allocator;
//     var vm = try VM.init(testAllocator);

//     const chars: []const u8 = (
//         \\{
//         \\var a = "first";
//         \\var a = "second";
//         \\}
//     );

//     var source = try testAllocator.alloc(u8, chars.len);
//     std.mem.copy(u8, source, chars);

//     const result = try vm.interpret(source);
//     // TODO: this broke with new function wrapped
//     try expect(result == InterpretResult.CompileError);
// }

test "virtual machine should error if var references itself in its initialized" {
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
    try expect(result == InterpretResult.CompileError);
}
