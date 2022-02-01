const std = @import("std");
const print = std.debug.print;

pub const OpCode = enum(usize) {
    OpReturn,
    OpConstant,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
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
