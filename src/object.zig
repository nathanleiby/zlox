const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const allocate = @import("./memory.zig").allocate;

const Value = @import("./value.zig").Value;
const Chunk = @import("./chunk.zig").Chunk;

const VM = @import("./vm.zig").VM;
const compiler = @import("./compiler.zig");
const debug = @import("./debug.zig");
const printValue = @import("./value.zig").printValue;

pub const ObjType = enum {
    objString,
    objFunction,
    objNative,
    objClosure,
    objUpvalue,
};

// Obj abstracts over pointers to various object types
pub const Obj = union(ObjType) {
    objString: *ObjString,
    objFunction: *ObjFunction,
    objNative: *ObjNative,
    objClosure: *ObjClosure,
    objUpvalue: *ObjUpvalue,

    fn free(self: Obj, om: *ObjManager) void {
        switch (self) {
            Obj.objString => |o| {
                om.allocator.free(o.chars);
                om.allocator.destroy(o);
            },
            Obj.objFunction => |o| {
                // o.chunk.free(); // TODO: This is the segfault
                om.allocator.destroy(o.chunk);
                om.allocator.destroy(o);
            },
            Obj.objNative => |o| om.allocator.destroy(o),
            Obj.objClosure => |o| {
                om.allocator.free(o.upvalues);
                om.allocator.destroy(o);
            },
            Obj.objUpvalue => |o| {
                om.allocator.destroy(o);
            },
        }
    }
};

pub const ObjString = struct {
    length: usize,
    chars: []const u8,
    isMarked: bool = false,

    fn markObject(self: *ObjString, om: *ObjManager) void {
        if (self.isMarked) return;
        self.isMarked = true;
        om.grayStack.append(Obj{ .objString = self }) catch return;
    }

    fn blackenObject(_: *ObjString, _: *ObjManager) void {
        // no-op
    }
};

pub const ObjFunction = struct {
    arity: u8,
    upvalueCount: u8,
    chunk: *Chunk,
    name: ?*ObjString = null,
    isMarked: bool = false,

    pub fn markObject(self: *ObjFunction, om: *ObjManager) void {
        if (self.isMarked) return;
        self.isMarked = true;
        om.grayStack.append(Obj{ .objFunction = self }) catch return;
    }

    fn blackenObject(self: *ObjFunction, om: *ObjManager) void {
        if (self.name) |name| {
            name.markObject(om);
        }
        // markArray
        var i: u8 = 0;
        while (i < self.chunk.values.items.len) {
            markValue(self.chunk.values.items[i], om);
            i += 1;
        }
    }
};

pub const ObjNative = struct {
    function: NativeFunction,
    isMarked: bool = false,

    fn markObject(self: *ObjNative, om: *ObjManager) void {
        if (self.isMarked) return;
        self.isMarked = true;
        om.grayStack.append(Obj{ .objNative = self }) catch return;
    }

    fn blackenObject(_: *ObjNative, _: *ObjManager) void {
        // no-op
    }
};

pub const NativeFunction = fn (argCount: u8) Value;

pub const ObjClosure = struct {
    function: *ObjFunction,
    upvalueCount: u8,
    upvalues: []*ObjUpvalue,
    isMarked: bool = false,

    fn markObject(self: *ObjClosure, om: *ObjManager) void {
        if (self.isMarked) return;
        self.isMarked = true;
        om.grayStack.append(Obj{ .objClosure = self }) catch return;
    }

    fn blackenObject(self: *ObjClosure, om: *ObjManager) void {
        self.function.blackenObject(om);
        var i: u8 = 0;
        while (i < self.upvalueCount) {
            self.upvalues[i].blackenObject(om);
            i += 1;
        }
    }
};

pub const ObjUpvalue = struct {
    location: *Value,
    next: ?*ObjUpvalue,
    closed: Value,
    isMarked: bool = false,

    fn markObject(self: *ObjUpvalue, om: *ObjManager) void {
        if (self.isMarked) return;
        self.isMarked = true;
        om.grayStack.append(Obj{ .objUpvalue = self }) catch return;
    }

    fn blackenObject(self: *ObjUpvalue, om: *ObjManager) void {
        markValue(self.closed, om);
    }
};

const L = std.SinglyLinkedList(Obj);

pub const ObjManager = struct {
    allocator: Allocator,

    objects: L,

    strings: std.StringHashMap(*ObjString),
    globals: std.StringHashMap(Value),

    // GC
    grayStack: std.ArrayList(Obj),

    // references set up by callers, to support GC
    _vm: ?*VM,
    isCompilerInitialized: bool,

    pub fn init(allocator: Allocator) !*ObjManager {
        const om: *ObjManager = try allocator.create(ObjManager);
        om.allocator = allocator;

        om.objects = L{};

        om.strings = std.StringHashMap(*ObjString).init(allocator);
        om.globals = std.StringHashMap(Value).init(allocator);

        // Garbage collection
        om.grayStack = std.ArrayList(Obj).init(allocator);
        om._vm = null; // set by vm.interpret()
        om.isCompilerInitialized = false; // set by compiler.compile()
        return om;
    }

    pub fn free(self: *ObjManager) void {
        self.strings.deinit(); // interned strings hash table
        // self.globals.deinit(); // globals table // TODO

        // free the nodes in the object linked list
        var it: ?*L.Node = self.objects.first;
        while (it != null) {
            const node = it.?;
            it = node.next;

            // free the data within
            node.data.free(self);

            // free the linked-list node
            self.objects.remove(node); // optional
            self.allocator.destroy(node);
        }

        // cleanup the grayStack (GC)
        self.grayStack.deinit();

        // free the ObjManager itself
        self.allocator.destroy(self);
    }

    fn allocateString(self: *ObjManager, chars: []const u8, length: usize) !*ObjString {
        if (debug.STRESS_GC) self.collectGarbage();
        var string: *ObjString = try self.allocator.create(ObjString);
        string.length = length;
        string.chars = chars;

        // capture this object, so we can free later
        var node: *L.Node = try self.allocator.create(L.Node);
        node.data = Obj{ .objString = string };
        self.objects.prepend(node);

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
        var node: *L.Node = try self.allocator.create(L.Node);
        node.data = Obj{ .objFunction = function };
        self.objects.prepend(node);

        return function;
    }

    pub fn newNative(self: *ObjManager, function: NativeFunction) !*ObjNative {
        if (debug.STRESS_GC) self.collectGarbage();
        var native: *ObjNative = try self.allocator.create(ObjNative);
        native.function = function;

        // capture this object, so we can free later
        var node: *L.Node = try self.allocator.create(L.Node);
        node.data = Obj{ .objNative = native };
        self.objects.prepend(node);

        return native;
    }

    pub fn newClosure(self: *ObjManager, function: *ObjFunction) !*ObjClosure {
        if (debug.STRESS_GC) self.collectGarbage();
        const upvalues: []*ObjUpvalue = try self.allocator.alloc(*ObjUpvalue, function.upvalueCount);

        // capture this object, so we can free later
        var i: usize = 0;
        while (i < function.upvalueCount) {
            var node: *L.Node = try self.allocator.create(L.Node);
            node.data = Obj{ .objUpvalue = upvalues[i] };
            self.objects.prepend(node);
            i += 1;
        }

        var closure: *ObjClosure = try self.allocator.create(ObjClosure);
        closure.function = function;
        closure.upvalues = upvalues;
        closure.upvalueCount = function.upvalueCount;

        // capture this object, so we can free later
        var node: *L.Node = try self.allocator.create(L.Node);
        node.data = Obj{ .objClosure = closure };
        self.objects.prepend(node);

        return closure;
    }

    pub fn newUpvalue(self: *ObjManager, slot: *Value) !*ObjUpvalue {
        if (debug.STRESS_GC) self.collectGarbage();
        var upvalue: *ObjUpvalue = try self.allocator.create(ObjUpvalue);
        upvalue.location = slot;
        upvalue.next = null;
        upvalue.closed = Value{ .nil = undefined };

        // capture this object, so we can free later
        var node: *L.Node = try self.allocator.create(L.Node);
        node.data = Obj{ .objUpvalue = upvalue };
        self.objects.prepend(node);

        return upvalue;
    }

    fn collectGarbage(self: *ObjManager) void {
        if (debug.LOG_GC) print("-- gc begin\n", .{});
        // when running some unit tests, we don't have a VM or compiler
        if (self._vm != null) {
            self.markRoots();
            self.traceReferences();
            // self.sweep(); // TODO
        }
        // TODO: This is failing in unit tests
        if (self.isCompilerInitialized) compiler.markCompilerRoots();
        if (debug.LOG_GC) print("-- gc end\n", .{});
    }

    fn markRoots(self: *ObjManager) void {
        const vm = self._vm.?;
        // values in Stack
        if (debug.LOG_GC) print("-- gc: mark values in Stack\n", .{});
        var i: u8 = 0;
        while (i < vm.stack.items.len) {
            markValue(vm.stack.items[i], self);
            i += 1;
        }

        // closures in the CallFrames
        if (debug.LOG_GC) print("-- gc: mark closures in the CallFrames\n", .{});
        var j: u8 = 0;
        while (j < vm.frameCount) {
            vm.frames[j].closure.markObject(self);
            j += 1;
        }

        // upvalues
        if (debug.LOG_GC) print("-- gc: mark upvalues\n", .{});
        var upvalue = vm.openUpvalues;
        while (upvalue) |u| {
            u.markObject(self);
            upvalue = u.next;
        }

        // global variables
        if (debug.LOG_GC) print("-- gc: mark global variables\n", .{});
        markValuesTable(self.globals, self);
    }

    fn traceReferences(self: *ObjManager) void {
        if (debug.LOG_GC) print("-- gc: trace references\n", .{});
        while (self.grayStack.popOrNull()) |obj| {
            switch (obj) {
                Obj.objString => |v| v.blackenObject(self),
                Obj.objFunction => |v| v.blackenObject(self),
                Obj.objNative => |v| v.blackenObject(self),
                Obj.objClosure => |v| v.blackenObject(self),
                Obj.objUpvalue => |v| v.blackenObject(self),
            }
        }
    }

    // fn sweep(self: *ObjManager) void {
    //     // TODO
    //     return

    // }
};

fn markValue(value: Value, om: *ObjManager) void {
    if (debug.LOG_GC) print("{*} mark ", .{&value});
    if (debug.LOG_GC) printValue(value);
    if (debug.LOG_GC) print("\n", .{});

    switch (value) {
        Value.number => return,
        Value.boolean => return,
        Value.nil => return,
        Value.objString => |v| v.markObject(om),
        Value.objFunction => |v| v.markObject(om),
        Value.objNative => |v| v.markObject(om),
        Value.objClosure => |v| v.markObject(om),
        Value.objUpvalue => |v| v.markObject(om),
    }
}

fn markValuesTable(a: std.StringHashMap(Value), om: *ObjManager) void {
    var iterator = a.iterator();

    while (iterator.next()) |entry| {
        // skipping marking keys
        markValue(entry.value_ptr.*, om);
    }
}

fn markStringsTable(a: std.StringHashMap(*ObjString), om: *ObjManager) void {
    var iterator = a.iterator();

    while (iterator.next()) |entry| {
        // skipping marking keys
        entry.value_ptr.*.markObject(om);
    }
}

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
