const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

// based on
// https://ziglang.org/documentation/master/#union

pub const ValueType = enum {
    boolean,
    number,
    nil,
};

pub const Value = union(ValueType) {
    boolean: bool,
    number: f64,
    nil: void,
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

pub fn IsNumber(v: Value) bool {
    return (@as(Value, v) == Value.number);
}

fn IsNil(v: Value) bool {
    return (@as(Value, v) == Value.nil);
}

fn IsBoolean(v: Value) bool {
    return (@as(Value, v) == Value.boolean);
}

pub fn IsFalsey(v: Value) bool {
    return IsNil(v) or (IsBoolean(v) and !v.boolean);
}

pub fn printValue(value: Value) void {
    switch (value) {
        Value.number => |v| print("{d}", .{v}),
        Value.boolean => |v| print("{b}", .{v}),
        Value.nil => |_| print("nil", .{}),
    }
}

test "tagged union can access chosen type" {
    const c = Value{ .boolean = true };
    try expect(@as(Value, c) == Value.boolean);

    switch (c) {
        Value.boolean => |value| try expect(value == true),
        Value.number => unreachable,
        Value.nil => unreachable,
    }
}

test "tagged union can access chosen type" {
    const c1 = Value{ .boolean = true };
    try expect(@as(Value, c1) == Value.boolean);
    printValue(c1);

    const c2 = Value{ .number = 1.234};
    try expect(@as(Value, c2) == Value.number);
    printValue(c2);

    const c3 = Value{ .nil = undefined };
    try expect(@as(Value, c3) == Value.nil);
    printValue(c3);
}
