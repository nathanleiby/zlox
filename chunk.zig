const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const expect = std.testing.expect;

const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

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
    OpEqual,
    OpGreater,
    OpLess,
};

// fn makeChunk(allocator: Allocator) !*Chunk {
//     const c: *Chunk = try allocator.create(Chunk);
//     c.*.code = &std.ArrayList(usize).init(allocator);
//     c.*.lines = &std.ArrayList(usize).init(allocator);
//     c.*.values = &std.ArrayList(Value).init(allocator);
//     return c;
// }

// pub const ChunkV2 = struct {
//     code: std.ArrayList(usize),
//     lines: std.ArrayList(usize),
//     values: std.ArrayList(Value),

//     pub fn init(allocator: Allocator) !*ChunkV2 {
//         const c: *ChunkV2 = try allocator.create(ChunkV2);
//         c.code = std.ArrayList(usize).init(allocator);
//         c.lines = std.ArrayList(usize).init(allocator);
//         c.values = std.ArrayList(Value).init(allocator);
//         return c;
//     }
// };

// test "chunk2" {
//     const allocator = std.testing.allocator;
//     const out = try ChunkV2.init(allocator);
//     try out.code.append(1);
//     try out.code.append(2);
//     print("out: {*}\n", .{out});
//     // print("{any}\n", .{slice});
//     out.code.deinit();
//     allocator.destroy(out);
// }

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
};

pub fn disassembleChunk(chunk: Chunk, name: []const u8) void {
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
        OpCode.OpReturn => {
            return simpleInstruction("OP_RETURN", offset);
        },
        OpCode.OpConstant => {
            print("OP_CONSTANT          ", .{});
            const constantIdx = chunk.code.items[offset + 1];
            print("{:04} -- ", .{constantIdx});
            printValue(chunk.values.items[constantIdx]);
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
        OpCode.OpFalse => {
            return simpleInstruction("OP_FALSE", offset);
        },
        OpCode.OpNot => {
            return simpleInstruction("OP_NOT", offset);
        },
        OpCode.OpGreater => {
            return simpleInstruction("OP_GREATER", offset);
        },
        OpCode.OpLess => {
            return simpleInstruction("OP_LESS", offset);
        },
        OpCode.OpEqual => {
            return simpleInstruction("OP_EQUAL", offset);
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
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

    // disassemble
    disassembleChunk(chunk.*, "chunk");
}
