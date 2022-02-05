const std = @import("std");
const print = std.debug.print;

const allocator = std.heap.page_allocator;
const expect = std.testing.expect;

pub const OpCode = enum(usize) {
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
};

const lineType = usize;

pub const Chunk = struct {
    code: *std.ArrayList(usize),
    lines: *std.ArrayList(lineType), // TODO : i32 before, usize now?
    values: *std.ArrayList(f64),

    pub fn free(self: Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.values.deinit();
    }

    pub fn write(self: Chunk, byte: usize, line: lineType) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn addConstant(self: Chunk, value: f64) !usize {
        try self.values.append(value);
        return self.values.items.len - 1;
    }
};

pub fn disassembleChunk(chunk: Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

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
            return simpleInstruction("OP_RETURN", offset);
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
            return simpleInstruction("OP_ADD", offset);
        },
        OpCode.OpSubtract => {
            return simpleInstruction("OP_SUBTRACT", offset);
        },
        OpCode.OpMultiply => {
            return simpleInstruction("OP_MULTIPLY", offset);
        },
        OpCode.OpDivide => {
            return simpleInstruction("OP_DIVIDE", offset);
        },
        OpCode.OpNil => {
            return simpleInstruction("OP_NIL", offset);
        },
        OpCode.OpTrue => {
            return simpleInstruction("OP_TRUE", offset);
        },
        OpCode.OpFalse=> {
            return simpleInstruction("OP_FALSE", offset);
        },
        OpCode.OpNot => {
            return simpleInstruction("OP_NOT", offset);
        },
    }
}

fn simpleInstruction (name: []const u8, offset: usize) usize {
    print("{s}", .{name});
    // TODO: hacky fixed width
    var i: usize = 0;
    while (i < 16 - name.len) {
        print(" ", .{});
        i += 1;
    }
    print("\n", .{});
    return offset + 1;
}

test "chunk" {
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

    try chunk.write(@enumToInt(OpCode.OpAdd), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpSubtract), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpDivide), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpMultiply), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpReturn), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpTrue), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpFalse), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpNil), fakeLineNumber);

    // disassemble
    disassembleChunk(chunk, "chunk");
}
