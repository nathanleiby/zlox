const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const expect = std.testing.expect;

const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

pub const OpCode = enum(u8) {
    Return,
    Constant,
    Nil,
    True,
    False,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
    Closure,
    GetUpvalue,
    SetUpvalue,
};

pub const Chunk = struct {
    code: std.ArrayList(usize),
    lines: std.ArrayList(usize),
    values: std.ArrayList(Value),

    pub fn init(allocator: Allocator) !*Chunk {
        const c: *Chunk = try allocator.create(Chunk);
        c.code = std.ArrayList(usize).init(allocator);
        c.lines = std.ArrayList(usize).init(allocator);
        c.values = std.ArrayList(Value).init(allocator);
        return c;
    }

    pub fn free(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.values.deinit();
    }

    pub fn write(self: *Chunk, byte: usize, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn addConstant(self: *Chunk, value: Value) !usize {
        try self.values.append(value);
        return self.values.items.len - 1;
    }

    pub fn count(self: *Chunk) usize {
        return self.code.items.len;
    }

    pub fn disassemble(chunk: Chunk, name: []const u8) void {
        print("== {s} ==\n", .{name});

        var offset: usize = 0;
        while (offset < chunk.code.items.len) {
            offset = disassembleInstruction(chunk, offset);
        }
    }

    pub fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
        print("{:04} ", .{offset});
        const byte = chunk.code.items[offset];
        // line
        if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
            print("   | ", .{});
        } else {
            print("{:04} ", .{chunk.lines.items[offset]});
        }

        const item = @intToEnum(OpCode, byte);
        switch (item) {
            .Call => return byteInstruction("OP_CALL", chunk, offset),
            .Return => return simpleInstruction("OP_RETURN", offset),
            .Constant => return constantInstruction("OP_CONSTANT", chunk, offset),
            .Negate => return simpleInstruction("OP_NEGATE", offset),
            .Add => return simpleInstruction("OP_ADD", offset),
            .Subtract => return simpleInstruction("OP_SUBTRACT", offset),
            .Multiply => return simpleInstruction("OP_MULTIPLY", offset),
            .Divide => return simpleInstruction("OP_DIVIDE", offset),
            .Nil => return simpleInstruction("OP_NIL", offset),
            .True => return simpleInstruction("OP_TRUE", offset),
            .False => return simpleInstruction("OP_FALSE", offset),
            .Not => return simpleInstruction("OP_NOT", offset),
            .Greater => return simpleInstruction("OP_GREATER", offset),
            .Less => return simpleInstruction("OP_LESS", offset),
            .Equal => return simpleInstruction("OP_EQUAL", offset),
            .Print => return simpleInstruction("OP_PRINT", offset),
            .Pop => return simpleInstruction("OP_POP", offset),
            .DefineGlobal => return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
            .GetGlobal => return constantInstruction("OP_GET_GLOBAL", chunk, offset),
            .SetGlobal => return constantInstruction("OP_SET_GLOBAL", chunk, offset),
            .GetLocal => return byteInstruction("OP_GET_LOCAL", chunk, offset),
            .SetLocal => return byteInstruction("OP_SET_LOCAL", chunk, offset),
            .Jump => return jumpInstruction("OP_JUMP", 1, chunk, offset),
            .JumpIfFalse => return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
            .Loop => return jumpInstruction("OP_LOOP", -1, chunk, offset),
            .Closure => return closureInstruction(chunk, offset),
            .GetUpvalue => return byteInstruction("GET_UPVALUE", chunk, offset),
            .SetUpvalue => return byteInstruction("SET_UPVALUE", chunk, offset),
        }
    }
};

fn printName(name: []const u8) void {
    print("{s: <20}", .{name});
}

fn closureInstruction(chunk: Chunk, offset: usize) usize {
    printName("OP_CLOSURE");

    const constantIdx = chunk.code.items[offset + 1];
    print("const idx={:02} val=", .{constantIdx});
    const constant = chunk.values.items[constantIdx];
    printValue(constant);
    print("\n", .{});

    // print the function locals and upvalues
    const function = constant.objFunction;
    var j: usize = 0;
    while (j < function.upvalueCount) {
        const isLocal = chunk.code.items[offset + (j * 2)];
        const index = chunk.code.items[offset + (j * 2) + 1];
        if (isLocal == 1) {
            print("{:04}      |                     {any} {any}\n", .{ offset + (j * 2) - 2, "local", index });
        } else {
            print("{:04}      |                     {any} {any}\n", .{ offset + (j * 2) - 2, "upvalue", index });
        }
        j += 1;
    }

    return offset + 2 + (j * 2);
}

fn constantInstruction(name: []const u8, chunk: Chunk, offset: usize) usize {
    printName(name);

    const constantIdx = chunk.code.items[offset + 1];
    print("const idx={:02} val=", .{constantIdx});
    printValue(chunk.values.items[constantIdx]);
    print("\n", .{});

    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    printName(name);
    print("\n", .{});
    return offset + 1;
}

fn byteInstruction(name: []const u8, chunk: Chunk, offset: usize) usize {
    printName(name);

    // When we disassemble these instructions, we can’t show the variable’s name like we could with globals.
    // Instead, we just show the slot number.
    const slot = chunk.code.items[offset + 1];
    print("{:04}", .{slot});
    print("\n", .{});

    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: i8, chunk: Chunk, offset: usize) usize {
    printName(name);

    var jump: u16 = @truncate(u16, chunk.code.items[offset + 1]) << 8;
    jump |= @truncate(u16, chunk.code.items[offset + 2]);
    if (sign > 0) {
        print("{:04} -> {:04}", .{ offset, offset + 3 + jump });
    } else {
        print("{:04} -> {:04}", .{ offset, offset + 3 - jump });
    }
    print("\n", .{});

    return offset + 3;
}

test "chunk" {
    const allocator = std.testing.allocator;

    var chunk = try Chunk.init(allocator);
    defer {
        // cleanup
        chunk.free();
        allocator.destroy(chunk);
    }

    // write
    const fakeLineNumber = 123;
    try chunk.write(@enumToInt(OpCode.Constant), fakeLineNumber);
    const constant = try chunk.addConstant(Value{ .number = 1.2 });
    try chunk.write(constant, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.Add), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.Subtract), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.Divide), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.Multiply), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.Return), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.True), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.False), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.Nil), fakeLineNumber);

    chunk.disassemble("chunk");
}
