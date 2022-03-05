const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

fn free(allocator: Allocator, comptime T: type, pointer: *void) ![]void {
    return try reallocate(allocator, pointer, @sizeOf(T), 0);
}

fn allocate(allocator: Allocator, comptime T: type, count: usize) ![]void {
    return try reallocate(allocator, undefined, 0, @sizeOf(T) * count);
}

fn reallocate(allocator: Allocator, pointer: *void, _: usize, newSize: usize) ![]void {
    return try allocator.realloc(pointer, newSize);
}

pub fn concat(allocator: Allocator, a: []const u8, b: []const u8) ![]u8 {
    const result = try allocator.alloc(u8, a.len + b.len);
    std.mem.copy(u8, result, a);
    std.mem.copy(u8, result[a.len..], b);
    return result;
}
