const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

// based on
// https://ziglang.org/documentation/master/#union

pub const ValueType = enum {
    boolean,
    number,
    nil,
    obj,
};

pub const Value = union(ValueType) {
    boolean: bool,
    number: f64,
    nil: void,
    obj: *Obj, // pointer to an obj

    pub fn isNumber(self: Value) bool {
        return (@as(Value, self) == Value.number);
    }

    fn isNil(self: Value) bool {
        return (@as(Value, self) == Value.nil);
    }

    fn isBoolean(self: Value) bool {
        return (@as(Value, self) == Value.boolean);
    }

    pub fn isFalsey(self: Value) bool {
        return self.isNil() or (self.isBoolean() and !self.boolean);
    }

    fn isObj(self: Value) bool {
        return (@as(Value, self) == Value.obj);
    }

    fn isObjType(self: Value, t: ObjType) bool {
        return self.isObj() and self.obj.*.type_ == t;
    }

    // TODO:
    fn asString(self: Value) *ObjString {
        return @ptrCast(*ObjString, self.obj);
    }

    fn asCString(self: Value) []const u8 {
        return self.asString().*.chars;
    }
};

pub const ObjType = enum {
    string,
};

pub const Obj = struct {
    type_: ObjType,

    // TODO: how to handle this error?
    // ./value.zig:32:16: error: '*Obj' and '*ObjString' do not have the same in-memory representation
    //     return @ptrCast(*ObjString, self.obj);
    //         ^
    // ./value.zig:32:41: note: '*Obj' has no in-memory bits
    //         return @ptrCast(*ObjString, self.obj);
    //                                         ^
    // ./value.zig:32:25: note: '*ObjString' has in-memory bits
    //         return @ptrCast(*ObjString, self.obj);

    // Hacky workaround for ^
    // Faking these fields so I can ensure it aligns
    // length: i64 = undefined,
    chars: []const u8 = undefined,
};

const ObjString = struct {
    obj: Obj,
    length: i64,
    chars: []const u8,
};

// // These are NUMBER_VAL, BOOL_VAL, NIL_VAL ...
// pub fn Number(n: f64) Value {
//     return Value{.number = n};
// }

// pub fn Bool(b: bool) {
//     return Value{.boolean = b};
// }

// pub fn Nil() Value {
//     return Value{.nil = undefined};
// }

pub fn valuesEqual(a: Value, b: Value) bool {
    // values must be of same type before they can be equal
    var aType: ValueType = a;
    var bType: ValueType = b;
    if (aType != bType) return false;

    switch (a) {
        ValueType.boolean => return a.boolean == b.boolean,
        ValueType.nil => return true,
        ValueType.number => return a.number == b.number,
        ValueType.obj => return a.obj == b.obj,
        // else => return false,
    }
}

pub fn printValue(value: Value) void {
    switch (value) {
        Value.number => |v| print("{d}", .{v}),
        Value.boolean => |v| print("{b}", .{v}),
        Value.nil => |_| print("nil", .{}),
        Value.obj => |v| printObj(v),
    }
}

fn printObj(obj: *Obj) void {
    // TODO
    // print("obj:{*}", obj);

    //     switch (OBJ_TYPE(value)) {
    //     case OBJ_STRING:
    //       printf("%s", AS_CSTRING(value));
    //       break;
    //   }
}

test "tagged union can access chosen type" {
    const c = Value{ .boolean = true };
    try expect(@as(Value, c) == Value.boolean);

    switch (c) {
        Value.boolean => |value| try expect(value == true),
        Value.number => unreachable,
        Value.nil => unreachable,
        Value.obj => unreachable,
    }
}

test "tagged union can access chosen type" {
    const c1 = Value{ .boolean = true };
    try expect(@as(Value, c1) == Value.boolean);
    printValue(c1);

    const c2 = Value{ .number = 1.234 };
    try expect(@as(Value, c2) == Value.number);
    printValue(c2);

    const c3 = Value{ .nil = undefined };
    try expect(@as(Value, c3) == Value.nil);
    printValue(c3);
}

test "can call isObj method on union" {
    var obj = Obj{ .type_ = ObjType.string };
    const c1 = Value{ .obj = &obj };
    try expect(c1.isObj() == true);
}

test "can translate between Value and ObjString" {
    var s = ObjString{
        .obj = Obj{ .type_ = ObjType.string },
        .length = 5,
        .chars = "hello",
    };

    var c1 = Value{ .obj = @ptrCast(*Obj, &s) };
    try expect(c1.isObjType(ObjType.string) == true);
    try expectEqualStrings("hello", c1.asCString());
}
