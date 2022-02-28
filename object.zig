const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const allocate = @import("./memory.zig").allocate;

const Value = @import("./value.zig").Value;
const Chunk = @import("./chunk.zig").Chunk;

pub const ObjString = struct {
    length: usize,
    chars: []const u8,
};

pub const ObjFunction = struct {
    arity: u8,
    chunk: *Chunk,
    name: ?*ObjString = null,
};

pub const ObjNative = struct {
    function: NativeFunction,
};

pub const NativeFunction = fn (argCount: u8) Value;

pub const ObjManager = struct {
    allocator: Allocator,
    objects: std.ArrayList(*ObjString),
    objectFns: std.ArrayList(*ObjFunction),
    objectNatives: std.ArrayList(*ObjNative),
    strings: std.StringHashMap(*ObjString),
    globals: std.StringHashMap(Value),

    pub fn init(allocator: Allocator) !*ObjManager {
        const om: *ObjManager = try allocator.create(ObjManager);
        om.allocator = allocator;
        om.objects = std.ArrayList(*ObjString).init(allocator);
        om.objectFns = std.ArrayList(*ObjFunction).init(allocator);
        om.objectNatives = std.ArrayList(*ObjNative).init(allocator);
        om.strings = std.StringHashMap(*ObjString).init(allocator);
        om.globals = std.StringHashMap(Value).init(allocator);
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

        // free the ObjFunction's
        const ownedSlice2 = self.objectFns.toOwnedSlice();
        for (ownedSlice2) |o| {
            // o.chunk.free(); // TODO: This is the segfault
            self.allocator.destroy(o.chunk);
            self.allocator.destroy(o);
        }
        self.allocator.free(ownedSlice2);

        // free the ObjNative's
        const ownedSlice3 = self.objectNatives.toOwnedSlice();
        for (ownedSlice3) |o| {
            self.allocator.destroy(o);
        }
        self.allocator.free(ownedSlice3);

        // free the objects arraylist
        self.objects.deinit();

        // free the objectFns arraylist
        self.objectFns.deinit();

        // free the objectFns arraylist
        self.objectNatives.deinit();

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
        if (self.strings.get(chars)) |interned| {
            return interned;
        }

        var heapChars = try self.allocator.alloc(u8, chars.len);
        std.mem.copy(u8, heapChars, chars);
        return try self.allocateString(heapChars, chars.len);
    }

    // assumes it can take ownership of the characters you pass in
    pub fn takeString(self: *ObjManager, chars: []const u8) !*ObjString {
        if (self.strings.get(chars)) |interned| {
            // TODO: not sure we can free this as advised
            // self.allocator.free(chars);
            return interned;
        }

        return try self.allocateString(chars, chars.len);
    }

    pub fn newFunction(self: *ObjManager) !*ObjFunction {
        var function: *ObjFunction = try self.allocator.create(ObjFunction);
        function.arity = 0;
        function.name = null;
        function.chunk = try Chunk.init(self.allocator);

        // capture this object, so we can free later
        try self.objectFns.append(function);

        return function;
    }

    pub fn newNative(self: *ObjManager, function: NativeFunction) !*ObjNative {
        var native: *ObjNative = try self.allocator.create(ObjNative);
        native.function = function;

        // capture this object, so we can free later
        try self.objectNatives.append(native);

        return native;
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

test "obj manager can create a function object" {
    const allocator = std.testing.allocator;

    var om = try ObjManager.init(allocator);
    defer om.free();

    _ = try om.newFunction();
}

fn nativeAnswerToLife(_: u8) Value {
    return Value{ .number = 42 };
}

test "obj manager can create a native function object" {
    const allocator = std.testing.allocator;

    var om = try ObjManager.init(allocator);
    defer om.free();

    _ = try om.newNative(nativeAnswerToLife);
}
