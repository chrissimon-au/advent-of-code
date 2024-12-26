const std = @import("std");
const ArrayList = std.ArrayList;
const expectEqual = std.testing.expectEqual;

fn parse_lock(input: []const u8) [5]u8 {
    var rows = std.mem.split(u8, input, "\n");
    var lock = [5]u8{ 0, 0, 0, 0, 0 };
    _ = rows.first();
    while (rows.next()) |row| {
        for (row, 0..) |pin, i| {
            if (pin == '#') {
                lock[i] += 1;
            }
        }
    }
    return lock;
}

test parse_lock {
    const lock =
        \\#####
        \\.####
        \\..###
        \\...##
        \\....#
        \\.....
        \\.....
    ;
    const lock_pins = parse_lock(lock);
    try expectEqual(0, lock_pins[0]);
    try expectEqual(1, lock_pins[1]);
    try expectEqual(2, lock_pins[2]);
    try expectEqual(3, lock_pins[3]);
    try expectEqual(4, lock_pins[4]);
}
