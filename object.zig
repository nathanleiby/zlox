const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const allocate = @import("./memory.zig").allocate;

pub const ObjString = struct {
    length: usize,
    chars: []const u8,
};

fn allocateString(allocator: Allocator, chars: []const u8, length: usize) !*ObjString {
    var string: *ObjString = try allocator.create(ObjString);
    string.length = length;
    string.chars = chars;
    return string;
}

// assumes it cannot take ownership of the characters you pass in,
// so it makes a copy of the chars on the heap
pub fn copyString(allocator: Allocator, chars: []const u8) !*ObjString {
    var heapChars = try allocator.alloc(u8, chars.len);
    std.mem.copy(u8, heapChars, chars);
    return try allocateString(allocator, heapChars, chars.len);
}

// assumes it can take ownership of the characters you pass in
pub fn takeString(allocator: Allocator, chars: []const u8) !*ObjString {
    return try allocateString(allocator, chars, chars.len);
}

fn concat(allocator: Allocator, a: []const u8, b: []const u8) ![]u8 {
    const result = try allocator.alloc(u8, a.len + b.len);
    std.mem.copy(u8, result, a);
    std.mem.copy(u8, result[a.len..], b);
    return result;
}

test "Copy a string" {
    const allocator = std.testing.allocator;
    const result = try copyString(allocator, "foo");
    defer {
        allocator.free(result.chars);
        allocator.destroy(result);
    }
    try expect(std.mem.eql(u8, "foo", result.chars));
}
