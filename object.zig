const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const allocate = @import("./memory.zig").allocate;

pub const ObjString = struct {
    length: usize,
    chars: []const u8,
};

pub const ObjManager = struct {
    allocator: Allocator,
    objects: std.ArrayList(*ObjString),

    pub fn init(allocator: Allocator) !*ObjManager {
        const om: *ObjManager = try allocator.create(ObjManager);
        om.allocator = allocator;
        om.objects = std.ArrayList(*ObjString).init(allocator);
        return om;
    }

    pub fn free(self: *ObjManager) void {
        // free the ObjString's
        const ownedSlice = self.objects.toOwnedSlice();
        for (ownedSlice) |o| {
            self.allocator.free(o.chars);
            self.allocator.destroy(o);
        }
        self.allocator.free(ownedSlice);

        // free the objects arraylist
        self.objects.deinit();

        // free the ObjManager itself
        self.allocator.destroy(self);
    }

    fn allocateString(self: *ObjManager, chars: []const u8, length: usize) !*ObjString {
        var string: *ObjString = try self.allocator.create(ObjString);
        string.length = length;
        string.chars = chars;

        // capture this object, so we can free later
        try self.objects.append(string);
        return string;
    }

    // assumes it cannot take ownership of the characters you pass in,
    // so it makes a copy of the chars on the heap
    pub fn copyString(self: *ObjManager, chars: []const u8) !*ObjString {
        var heapChars = try self.allocator.alloc(u8, chars.len);
        std.mem.copy(u8, heapChars, chars);
        return try self.allocateString(heapChars, chars.len);
    }

    // assumes it can take ownership of the characters you pass in
    pub fn takeString(self: *ObjManager, chars: []const u8) !*ObjString {
        return try self.allocateString(chars, chars.len);
    }
};

test "Copy a string" {
    const allocator = std.testing.allocator;

    var om = try ObjManager.init(allocator);
    defer om.free();

    const result = try om.copyString("foo");
    try expect(std.mem.eql(u8, "foo", result.chars));
}
