const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const Obj = @import("./value.zig").Obj;
const ObjString = @import("./value.zig").ObjString;
const ObjType = @import("./value.zig").ObjType;
const allocate = @import("./memory.zig").allocate;

// fn allocateObject(allocator: *Allocator, t: ObjType, comptime T: type) !*Obj {
//     var a = try allocate(allocator, T, 1);
//     var object: *Obj = @ptrCast(*Obj, a);
//     object.type_ = t;
//     return object;
// }

// fn allocateString(allocator: *Allocator, chars: []const u8, length: usize) !*ObjString {
//     var obj: *Obj = try allocateObject(allocator, ObjType.string, ObjString);
//     var string = @ptrCast(*ObjString, obj);
//     string.length = length;
//     string.chars = chars;
//     return string;
// }

fn allocateString(allocator: Allocator, chars: []const u8, length: usize) !*ObjString {
    var string: *ObjString = try allocator.create(ObjString);
    string.obj.type_ = ObjType.string;
    string.length = length;
    string.chars = chars;
    return string;
}

pub fn copyString(allocator: Allocator, chars: []const u8) !*ObjString {
    var heapChars = try allocator.alloc(u8, chars.len);
    std.mem.copy(u8, heapChars, chars);
    return try allocateString(allocator, heapChars, chars.len);
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
