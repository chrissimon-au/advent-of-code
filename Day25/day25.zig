const std = @import("std");
const expect = std.testing.expect;

fn check() bool {
    return true;
}

test check {
    try expect(check());
}
