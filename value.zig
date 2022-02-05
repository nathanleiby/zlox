const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

// based on
// https://ziglang.org/documentation/master/#union

pub const ValueType = enum {
    boolean,
    double,
};

pub const Value = union(ValueType) {
    boolean: bool,
    double: f64,
};

pub fn printValue(value: Value) void {
    switch (c) {
        Value.boolean => |value| try print("{d}", (value)),
        Value.double => unreachable,
    }
}

test "tagged union can access chosen type" {
    const c = Value{ .boolean = true };
    try expect(@as(Value, c) == Value.boolean);

    switch (c) {
        Value.boolean => |value| try expect(value == true),
        Value.double => unreachable,
    }
}
