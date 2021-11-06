const std = @import("std");
const print = std.debug.print;
// TODO: consider dep injecting this, so i can use a test one too
const allocator = std.heap.page_allocator;

const OpCode = enum (usize) {
    OpReturn,
    OpConstant,
};

const Chunk = struct {
    code: *std.ArrayList(usize),
    lines: *std.ArrayList(i32),
    values: *std.ArrayList(f64),

    pub fn free(self: Chunk) void {
        self.code.deinit();
        self.lines.deinit();
    }

    pub fn write(self: Chunk, byte: usize, line: i32) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn addConstant(self: Chunk, value: f64) !usize {
        try self.values.append(value);
        return self.values.items.len - 1;
    }
};

fn disassembleChunk(chunk: Chunk) void {
    print("== {s} ==\n", .{"test chunk"});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
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
                offset += 1;
            },
            OpCode.OpConstant=> {
                print("OP_CONSTANT          ", .{});
                // TODO
                const constantIdx = chunk.code.items[offset + 1];
                print("{:04} -- ", .{constantIdx});
                print("{d}", .{chunk.values.items[constantIdx]});
                print("\n", .{});
                offset += 2;
            },
        }
    }
}

pub fn main() !void {
    const chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(i32).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };

    // var values = std.ArrayList(i32).init(allocator);

    // free
    defer chunk.free();

    // write
    try chunk.write(@enumToInt(OpCode.OpConstant), 1);
    const constant = try chunk.addConstant(1.2);
    try chunk.write(constant, 1);

    try chunk.write(@enumToInt(OpCode.OpReturn), 1);

    try chunk.write(@enumToInt(OpCode.OpConstant), 2);
    const constant2 = try chunk.addConstant(99);
    try chunk.write(constant2, 2);

    try chunk.write(@enumToInt(OpCode.OpReturn), 2);


    disassembleChunk(chunk);
}

// TODO: Write a unit test for chunks

// test "if statement" {
//     const a = true;
//     var x: u16 = 0;
//     if (a) {
//         x += 1;
//     } else {
//         x += 2;
//     }
//     try expect(x == 1);
// }
