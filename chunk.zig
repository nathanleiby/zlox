const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const expect = std.testing.expect;

const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

pub const OpCode = enum(u8) {
    OpReturn,
    OpConstant,
    OpNil,
    OpTrue,
    OpFalse,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
    OpNot,
    OpEqual,
    OpGreater,
    OpLess,
    OpPrint,
    OpPop,
    OpDefineGlobal,
    OpGetGlobal,
    OpSetGlobal,
    OpGetLocal,
    OpSetLocal,
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

    pub fn disassemble(chunk: Chunk, name: []const u8) void {
        print("== {s} ==\n", .{name});

        var offset: usize = 0;
        while (offset < chunk.code.items.len) {
            offset = disassembleInstruction(chunk, offset);
        }
    }

    pub fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
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
            .OpReturn => {
                return simpleInstruction("OP_RETURN", offset);
            },
            .OpConstant => {
                return constantInstruction("OP_CONSTANT", chunk, offset);
            },
            .OpNegate => {
                print("OP_NEGATE            ", .{});
                const constantIdx = chunk.code.items[offset + 1];
                print("{:04} -- ", .{constantIdx});
                print("{d}", .{chunk.values.items[constantIdx]});
                print("\n", .{});
                return offset + 1;
            },
            .OpAdd => {
                return simpleInstruction("OP_ADD", offset);
            },
            .OpSubtract => {
                return simpleInstruction("OP_SUBTRACT", offset);
            },
            .OpMultiply => {
                return simpleInstruction("OP_MULTIPLY", offset);
            },
            .OpDivide => {
                return simpleInstruction("OP_DIVIDE", offset);
            },
            .OpNil => {
                return simpleInstruction("OP_NIL", offset);
            },
            .OpTrue => {
                return simpleInstruction("OP_TRUE", offset);
            },
            .OpFalse => {
                return simpleInstruction("OP_FALSE", offset);
            },
            .OpNot => {
                return simpleInstruction("OP_NOT", offset);
            },
            .OpGreater => {
                return simpleInstruction("OP_GREATER", offset);
            },
            .OpLess => {
                return simpleInstruction("OP_LESS", offset);
            },
            .OpEqual => {
                return simpleInstruction("OP_EQUAL", offset);
            },
            .OpPrint => {
                return simpleInstruction("OP_PRINT", offset);
            },
            .OpPop => {
                return simpleInstruction("OP_POP", offset);
            },
            .OpDefineGlobal => {
                return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
            },
            .OpGetGlobal => {
                return constantInstruction("OP_GET_GLOBAL", chunk, offset);
            },
            .OpSetGlobal => {
                return constantInstruction("OP_SET_GLOBAL", chunk, offset);
            },
            .OpGetLocal => {
                return byteInstruction("OP_GET_LOCAL", chunk, offset);
            },
            .OpSetLocal => {
                return byteInstruction("OP_SET_LOCAL", chunk, offset);
            },
        }
    }
};

fn printName(name: []const u8) void {
    print("{s}", .{name});
    // TODO: hacky fixed width
    // use string formatting. may need bufPrint
    // https://ziglearn.org/chapter-2/#advanced-formatting
    var i: usize = 0;
    while (i < 16 - name.len) {
        print(" ", .{});
        i += 1;
    }
}

fn constantInstruction(name: []const u8, chunk: Chunk, offset: usize) usize {
    printName(name);

    const constantIdx = chunk.code.items[offset + 1];
    print("{:04} -- ", .{constantIdx});
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
    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant = try chunk.addConstant(Value{ .number = 1.2 });
    try chunk.write(constant, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpAdd), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpSubtract), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpDivide), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpMultiply), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpReturn), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpTrue), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpFalse), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpNil), fakeLineNumber);

    chunk.disassemble("chunk");
}
