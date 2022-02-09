const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const allocate = @import("./memory.zig").allocate;

// toggle debug logging
const DEBUG_MODE = false;

pub const ObjString = struct {
    length: usize,
    chars: []const u8,
};

pub const ObjManager = struct {
    allocator: Allocator,
    objects: std.ArrayList(*ObjString),
    strings: std.StringHashMap(*ObjString),

    pub fn init(allocator: Allocator) !*ObjManager {
        const om: *ObjManager = try allocator.create(ObjManager);
        om.allocator = allocator;
        om.objects = std.ArrayList(*ObjString).init(allocator);
        om.strings = std.StringHashMap(*ObjString).init(allocator);
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

        // free the strings hash table
        self.strings.deinit();

        // free the ObjManager itself
        self.allocator.destroy(self);
    }

    fn allocateString(self: *ObjManager, chars: []const u8, length: usize) !*ObjString {
        var string: *ObjString = try self.allocator.create(ObjString);
        string.length = length;
        string.chars = chars;

        // capture this object, so we can free later
        try self.objects.append(string);

        // intern the string on creation
        try self.strings.put(chars, string);

        return string;
    }

    // assumes it cannot take ownership of the characters you pass in,
    // so it makes a copy of the chars on the heap
    pub fn copyString(self: *ObjManager, chars: []const u8) !*ObjString {
        if (DEBUG_MODE) print("copyString(): {s}\n", .{chars});
        if (self.strings.get(chars)) |interned| {
            if (DEBUG_MODE) {
                print("copyString() found interned string: {s}\n", .{interned.chars});
            }
            return interned;
        }

        var heapChars = try self.allocator.alloc(u8, chars.len);
        std.mem.copy(u8, heapChars, chars);
        return try self.allocateString(heapChars, chars.len);
    }

    // assumes it can take ownership of the characters you pass in
    pub fn takeString(self: *ObjManager, chars: []const u8) !*ObjString {
        if (DEBUG_MODE) print("takeString(): {s}\n", .{chars});
        if (self.strings.get(chars)) |interned| {
            if (DEBUG_MODE) print("takeString() found interned string: {s}\n", .{interned.chars});
            // TODO: not sure we can free this as advised
            // self.allocator.free(chars);
            return interned;
        }

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

test "obj manager interns strings" {
    const allocator = std.testing.allocator;

    var om = try ObjManager.init(allocator);
    defer om.free();

    const foo = try om.copyString("foo");
    const foo2 = try om.copyString("foo");
    try expect(foo == foo2);
    const foo3 = try om.takeString("foo");
    try expect(foo == foo3);

    const bar = try om.copyString("bar");
    const bar2 = try om.copyString("bar");
    try expect(bar == bar2);

    try expect(foo != bar);
}
