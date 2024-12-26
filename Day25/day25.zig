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

fn parse_key(input: []const u8) [5]u8 {
    var row_input = std.mem.split(u8, input, "\n");
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var rows = ArrayList([]const u8).init(allocator);
    while (row_input.next()) |row| {
        rows.append(row) catch {};
    }
    var lock = [5]u8{ 0, 0, 0, 0, 0 };
    comptime var rowIdx = 5;
    inline while (rowIdx > 0) : (rowIdx -= 1) {
        const row = rows.items[rowIdx];
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

test parse_key {
    const key =
        \\.....
        \\.....
        \\#....
        \\##...
        \\###..
        \\####.
        \\#####
    ;
    const key_heights = parse_key(key);
    try expectEqual(4, key_heights[0]);
    try expectEqual(3, key_heights[1]);
    try expectEqual(2, key_heights[2]);
    try expectEqual(1, key_heights[3]);
    try expectEqual(0, key_heights[4]);
}
