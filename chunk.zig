const std = @import("std");

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
