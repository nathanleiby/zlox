const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

pub fn free(allocator: *Allocator, comptime T: type, pointer: *void) ![]void {
    return try reallocate(allocator, pointer, @sizeOf(T), 0);
}

pub fn allocate(allocator: *Allocator, comptime T: type, count: usize) ![]void {
    return try reallocate(allocator, undefined, 0, @sizeOf(T) * count);
}

fn reallocate(allocator: *Allocator, pointer: *void, oldSize: usize, newSize: usize) ![]void {
    return try allocator.realloc(pointer, newSize);
}

// TODO: String concatentation in zig with an   allocator

// const std = @import("std");
// const Allocator = std.mem.Allocator;
// const expect = std.testing.expect;

// test "using an allocator" {
//     var buffer: [100]u8 = undefined;
//     const allocator = &std.heap.FixedBufferAllocator.init(&buffer).allocator;
//     const result = try concat(allocator, "foo", "bar");
//     try expect(std.mem.eql(u8, "foobar", result));
// }

// fn concat(allocator: *Allocator, a: []const u8, b: []const u8) ![]u8 {
//     const result = try allocator.alloc(u8, a.len + b.len);
//     std.mem.copy(u8, result, a);
//     std.mem.copy(u8, result[a.len..], b);
//     return result;
// }
