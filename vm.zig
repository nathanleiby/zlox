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
const NativeFunction = @import("./object.zig").NativeFunction;

const compiler = @import("./compiler.zig");
const concat = @import("./memory.zig").concat;
const U8_COUNT = @import("./constants.zig").U8_COUNT;

// Debugging flags
const debug = @import("./debug.zig");

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

fn clockNative(_: u8) Value {
    const ts = std.time.timestamp();
    return Value{ .number = @intToFloat(f64, ts) };
}

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * U8_COUNT;

pub const VM = struct {
    objManager: *ObjManager,
    stack: std.ArrayList(Value),
    frames: [FRAMES_MAX]CallFrame,
    frameCount: u8,
    frame: *CallFrame,
    allocator: Allocator,
    ip: usize, // instruction pointer

    pub fn init(a: Allocator) !VM {
        var vm = VM{
            .objManager = try ObjManager.init(a),
            .stack = std.ArrayList(Value).init(a),
            // to allow allocating more memory within compile(), e.g. to store strings
            .allocator = a,
            .ip = 0,
            .frames = [_]CallFrame{CallFrame{ .function = undefined, .ip = 0, .slotOffset = 0 }} ** FRAMES_MAX,
            .frameCount = 0,
            .frame = undefined,
        };

        try vm.defineNative("clock", clockNative);

        return vm;
    }

    pub fn free(self: *VM) void {
        self.stack.deinit();
        self.objManager.free();
    }

    pub fn interpret(self: *VM, source: []u8) !InterpretResult {
        const function = compiler.compile(source, self.objManager) catch return InterpretResult.CompileError;

        const fnVal = Value{ .objFunction = function };
        try self.push(fnVal);
        _ = self.call(function, 0);

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

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) void {
        print("runtimeError: ", .{});
        print(fmt, args);
        print("\n", .{});

        var i = self.frameCount - 1;
        while (i >= 0) {
            const frame = &self.frames[i];
            print("[line {d}] in ", .{frame.function.chunk.lines.items[frame.ip]});
            if (frame.function.name) |fname| {
                print("{s}()\n", .{fname.chars});
            } else {
                print("script\n", .{});
            }

            if (i == 0) {
                break;
            }
            i -= 1;
        }

        // TODO: actually reset the stack
        // resetStack();
    }

    fn run(self: *VM) !InterpretResult {
        self.frame = &self.frames[self.frameCount - 1];

        while (true) {
            if (debug.TRACE_EXECUTION_INCLUDE_INSTRUCTIONS) {
                _ = self.frame.function.chunk.disassembleInstruction(self.frame.ip);
            }

            if (debug.TRACE_EXECUTION_PRINT_STACK) {
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
                .Call => {
                    const argCount = self.readByte();
                    if (!try self.callValue(self.peek(argCount), @truncate(u8, argCount))) {
                        return InterpretResult.RuntimeError;
                    }
                    // after function call, return to calling frame
                    self.frame = &self.frames[self.frameCount - 1];
                },
                .Return => {
                    const result: Value = self.pop();

                    // move outward one frame
                    self.frameCount -= 1;

                    // are we returning from the script?
                    if (self.frameCount == 0) {
                        _ = self.pop();
                        return InterpretResult.Ok;
                    }

                    // add return value to the stack and update the calling frame
                    try self.push(result);
                    self.frame = &self.frames[self.frameCount - 1];
                },
                .Negate => {
                    if (!(@as(Value, self.peek(0)) == Value.number)) {
                        self.runtimeError("Negation operand must be a number.", .{});
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
                        self.runtimeError("Operands must be two numbers or two strings.", .{});
                        return InterpretResult.RuntimeError;
                    }
                },
                .Subtract, .Multiply, .Divide, .Greater, .Less => {
                    if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        try self.binaryOp(instruction);
                    } else {
                        self.runtimeError("Operands must be two numbers.", .{});
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
                        self.runtimeError("Undefined variable '{s}'.", .{name});
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
                        self.runtimeError("Undefined variable '{s}'.", .{name});
                        return InterpretResult.RuntimeError;
                    }
                },
                .GetLocal => {
                    const slot = self.readByte();
                    // TODO: remove debugging
                    if (debug.TRACE_EXECUTION_SHOW_GET_SET_VARS) {
                        print(".GetLocal slot = {any}, slotOffset = {any}, stack.items.len = {any}\n", .{ slot, self.frame.slotOffset, self.stack.items.len });
                    }
                    try self.push(self.stack.items[self.frame.slotOffset + slot]);
                },
                .SetLocal => {
                    const slot = self.readByte();
                    if (debug.TRACE_EXECUTION_SHOW_GET_SET_VARS) {
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
        const constantIdx = self.frame.function.chunk.code.items[self.frame.ip];
        self.frame.ip += 1;
        return self.frame.function.chunk.values.items[constantIdx];
    }

    fn readByte(self: *VM) usize {
        const byte = self.frame.function.chunk.code.items[self.frame.ip];
        self.frame.ip += 1;
        return byte;
    }

    fn readShort(self: *VM) usize {
        const short = @intCast(u16, (self.frame.function.chunk.code.items[self.frame.ip] << 8) | self.frame.function.chunk.code.items[self.frame.ip + 1]);
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

    fn callValue(self: *VM, callee: Value, argCount: u8) !bool {
        if (callee.isFunction()) {
            return self.call(callee.asFunction(), argCount);
        } else if (callee.isNative()) {
            const native: NativeFunction = callee.objNative.function;
            const result: Value = native(argCount);
            var i: u8 = 0;
            while (i < argCount) {
                _ = self.stack.pop();
                i += 1;
            }

            try self.push(result);
            return true;
        }
        self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn call(self: *VM, func: *ObjFunction, argCount: u8) bool {
        if (argCount != func.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.", .{ func.arity, argCount });
            return false;
        }

        if (self.frameCount == FRAMES_MAX) {
            self.runtimeError("Stack overflow.", .{});
            return false;
        }

        // Add a new call frame
        const frame: *CallFrame = &self.frames[self.frameCount];
        frame.function = func;
        frame.ip = 0;
        frame.slotOffset = self.stack.items.len - argCount - 1;

        self.frameCount += 1;

        return true;
    }

    fn defineNative(self: *VM, name: []const u8, function: NativeFunction) !void {
        try self.push(Value{ .objString = try self.objManager.copyString(name) });
        try self.push(Value{ .objNative = try self.objManager.newNative(function) });
        try self.objManager.globals.put(self.stack.items[0].asCString(), self.stack.items[1]);
        _ = self.pop();
        _ = self.pop();
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

test "virtual machine should error if fn called with wrong nubmer of arguments" {
    const testAllocator = std.heap.page_allocator;
    var vm = try VM.init(testAllocator);

    const chars: []const u8 = (
        \\ fun a() { b(); }
        \\ fun b() { c(); }
        \\ fun c() {
        \\   c("too", "many");
        \\ }
        \\
        \\ a();
    );

    var source = try testAllocator.alloc(u8, chars.len);
    std.mem.copy(u8, source, chars);

    const result = try vm.interpret(source);
    try expect(result == InterpretResult.RuntimeError);
}
