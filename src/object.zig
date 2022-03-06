const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const allocate = @import("./memory.zig").allocate;

const Value = @import("./value.zig").Value;
const Chunk = @import("./chunk.zig").Chunk;

const VM = @import("./vm.zig").VM;
const Compiler = @import("./compiler.zig").Compiler;
const debug = @import("./debug.zig");

pub const ObjString = struct {
    length: usize,
    chars: []const u8,
};

pub const ObjFunction = struct {
    arity: u8,
    upvalueCount: u8,
    chunk: *Chunk,
    name: ?*ObjString = null,
};

pub const ObjNative = struct {
    function: NativeFunction,
};

pub const NativeFunction = fn (argCount: u8) Value;

pub const ObjClosure = struct {
    function: *ObjFunction,
    upvalueCount: u8,
    upvalues: []*ObjUpvalue,
};

pub const ObjUpvalue = struct {
    location: *Value,
    next: ?*ObjUpvalue,
    closed: Value,
};

pub const ObjManager = struct {
    allocator: Allocator,
    objects: std.ArrayList(*ObjString),
    objectFns: std.ArrayList(*ObjFunction),
    objectNatives: std.ArrayList(*ObjNative),
    objectClosures: std.ArrayList(*ObjClosure),
    objectUpvalues: std.ArrayList(*ObjUpvalue),
    strings: std.StringHashMap(*ObjString),
    globals: std.StringHashMap(Value),
    // references set up by callers, to support GC
    _vm: *VM,
    _currentCompiler: **Compiler,

    pub fn init(allocator: Allocator) !*ObjManager {
        const om: *ObjManager = try allocator.create(ObjManager);
        om.allocator = allocator;
        om.objects = std.ArrayList(*ObjString).init(allocator);
        om.objectFns = std.ArrayList(*ObjFunction).init(allocator);
        om.objectNatives = std.ArrayList(*ObjNative).init(allocator);
        om.objectClosures = std.ArrayList(*ObjClosure).init(allocator);
        om.objectUpvalues = std.ArrayList(*ObjUpvalue).init(allocator);
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

        // free the ObjClosures
        const ownedSlice4 = self.objectClosures.toOwnedSlice();
        for (ownedSlice4) |o| {
            self.allocator.free(o.upvalues);
            self.allocator.destroy(o);
        }
        self.allocator.free(ownedSlice4);

        // free the ObjUpvalues
        const ownedSlice5 = self.objectUpvalues.toOwnedSlice();
        for (ownedSlice5) |o| {
            self.allocator.destroy(o);
        }
        self.allocator.free(ownedSlice5);

        // free the objects arraylist
        self.objects.deinit();

        // free the objectFns arraylist
        self.objectFns.deinit();

        // free the objectFns arraylist
        self.objectNatives.deinit();

        // free the objectFns arraylist
        self.objectClosures.deinit();

        // free the objectFns arraylist
        self.objectUpvalues.deinit();

        // free the strings hash table
        self.strings.deinit();

        // free the ObjManager itself
        self.allocator.destroy(self);
    }

    fn allocateString(self: *ObjManager, chars: []const u8, length: usize) !*ObjString {
        if (debug.STRESS_GC) self.collectGarbage();
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
        if (debug.STRESS_GC) self.collectGarbage();
        var function: *ObjFunction = try self.allocator.create(ObjFunction);
        function.arity = 0;
        function.upvalueCount = 0;
        function.name = null;
        function.chunk = try Chunk.init(self.allocator);

        // capture this object, so we can free later
        try self.objectFns.append(function);

        return function;
    }

    pub fn newNative(self: *ObjManager, function: NativeFunction) !*ObjNative {
        if (debug.STRESS_GC) self.collectGarbage();
        var native: *ObjNative = try self.allocator.create(ObjNative);
        native.function = function;

        // capture this object, so we can free later
        try self.objectNatives.append(native);

        return native;
    }

    pub fn newClosure(self: *ObjManager, function: *ObjFunction) !*ObjClosure {
        if (debug.STRESS_GC) self.collectGarbage();
        const upvalues: []*ObjUpvalue = try self.allocator.alloc(*ObjUpvalue, function.upvalueCount);
        //// skipping nulling them out ala C-programming
        // for (int i = 0; i < function->upvalueCount; i++) {
        //     upvalues[i] = NULL;
        // }

        var closure: *ObjClosure = try self.allocator.create(ObjClosure);
        closure.function = function;
        closure.upvalues = upvalues;
        closure.upvalueCount = function.upvalueCount;

        // capture this object, so we can free later
        try self.objectClosures.append(closure);

        return closure;
    }

    pub fn newUpvalue(self: *ObjManager, slot: *Value) !*ObjUpvalue {
        if (debug.STRESS_GC) self.collectGarbage();
        var upvalue: *ObjUpvalue = try self.allocator.create(ObjUpvalue);
        upvalue.location = slot;
        upvalue.next = null;
        upvalue.closed = Value{ .nil = undefined };

        // capture this object, so we can free later
        try self.objectUpvalues.append(upvalue);

        return upvalue;
    }

    fn collectGarbage(_: *ObjManager) void {
        if (debug.LOG_GC) print("-- gc begin\n", .{});
        if (debug.LOG_GC) print("-- gc end\n", .{});
    }

    // fn markRoots(self: *ObjManager) void {}
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

test "obj manager can create a closure object" {
    const allocator = std.testing.allocator;

    var om = try ObjManager.init(allocator);
    defer om.free();

    var f = try om.newFunction();
    _ = try om.newClosure(f);
}
