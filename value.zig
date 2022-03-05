const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

const ObjString = @import("./object.zig").ObjString;
const ObjFunction = @import("./object.zig").ObjFunction;
const ObjNative = @import("./object.zig").ObjNative;
const ObjClosure = @import("./object.zig").ObjClosure;

// More info on unions in Zig:
// https://ziglang.org/documentation/master/#union
pub const ValueType = enum { boolean, number, nil, objString, objFunction, objNative, objClosure };

pub const Value = union(ValueType) {
    boolean: bool,
    number: f64,
    nil: void,
    objString: *ObjString,
    objFunction: *ObjFunction,
    objNative: *ObjNative,
    objClosure: *ObjClosure,

    pub fn isNumber(self: Value) bool {
        return (@as(Value, self) == Value.number);
    }

    fn isNil(self: Value) bool {
        return (@as(Value, self) == Value.nil);
    }

    fn isBoolean(self: Value) bool {
        return (@as(Value, self) == Value.boolean);
    }

    pub fn isFunction(self: Value) bool {
        return (@as(Value, self) == Value.objFunction);
    }

    pub fn asFunction(self: Value) *ObjFunction {
        return self.objFunction;
    }

    pub fn isNative(self: Value) bool {
        return (@as(Value, self) == Value.objNative);
    }

    pub fn isClosure(self: Value) bool {
        return (@as(Value, self) == Value.objClosure);
    }

    pub fn asClosure(self: Value) bool {
        return self.objClosure;
    }

    pub fn isFalsey(self: Value) bool {
        return self.isNil() or (self.isBoolean() and !self.boolean);
    }

    pub fn isString(self: Value) bool {
        return (@as(Value, self) == Value.objString);
    }

    pub fn asCString(self: Value) []const u8 {
        return self.objString.*.chars;
    }
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
        ValueType.objString => return std.mem.eql(u8, a.asCString(), b.asCString()),
        Value.objFunction => return &a == &b,
        Value.objNative => return &a == &b,
        Value.objClosure => return &a == &b,
    }
}

fn printFunction(objFunction: *ObjFunction) void {
    if (objFunction.name) |name| {
        print("<fn {s}>", .{name.*.chars});
    } else {
        print("<script>", .{});
    }
}
pub fn printValue(value: Value) void {
    switch (value) {
        Value.number => |v| print("{d}", .{v}),
        Value.boolean => |v| print("{b}", .{v}),
        Value.nil => |_| print("nil", .{}),
        Value.objString => |_| print("{s}", .{value.asCString()}),
        Value.objFunction => |v| printFunction(v),
        Value.objNative => |_| print("<native fn>", .{}),
        Value.objClosure => |v| printFunction(v.function),
    }
}

test "tagged union can access chosen type" {
    const c = Value{ .boolean = true };
    try expect(@as(Value, c) == Value.boolean);

    switch (c) {
        Value.boolean => |value| try expect(value == true),
        else => unreachable,
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

    var s = ObjString{
        .length = 5,
        .chars = "hello",
    };
    const c4 = Value{ .objString = &s };
    try expect(@as(Value, c4) == Value.objString);
    printValue(c4);
}

test "can translate between Value and ObjString" {
    var s = ObjString{
        .length = 5,
        .chars = "hello",
    };

    var c1 = Value{ .objString = &s };
    try expect(c1.isString() == true);
    try expectEqualStrings("hello", c1.asCString());
}

test "valuesEqual for Lox strings" {
    var s1 = ObjString{
        .length = 5,
        .chars = "hello",
    };

    var c1 = Value{ .objString = &s1 };

    var s2 = ObjString{
        .length = 5,
        .chars = "hello",
    };

    var c2 = Value{ .objString = &s2 };

    try expectEqualStrings(c1.asCString(), c2.asCString());
    try expect(valuesEqual(c1, c2));
}
