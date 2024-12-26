const std = @import("std");
const ArrayList = std.ArrayList;
const expectEqual = std.testing.expectEqual;

const KeyOrLock = union { key: Key, lock: Lock };

const Key = struct { heights: [5]u8 };
const Lock = struct { pins: [5]u8 };

fn parse_lock(input: []const u8) Lock {
    var rows = std.mem.split(u8, input, "\n");
    var lock = Lock{ .pins = [5]u8{ 0, 0, 0, 0, 0 } };
    _ = rows.first();
    while (rows.next()) |row| {
        for (row, 0..) |pin, i| {
            if (pin == '#') {
                lock.pins[i] += 1;
            }
        }
    }
    return lock;
}

fn parse_key(input: []const u8) Key {
    var row_input = std.mem.split(u8, input, "\n");
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var rows = ArrayList([]const u8).init(allocator);
    while (row_input.next()) |row| {
        rows.append(row) catch {};
    }
    var key = Key{ .heights = [5]u8{ 0, 0, 0, 0, 0 } };
    comptime var rowIdx = 5;
    inline while (rowIdx > 0) : (rowIdx -= 1) {
        const row = rows.items[rowIdx];
        for (row, 0..) |pin, i| {
            if (pin == '#') {
                key.heights[i] += 1;
            }
        }
    }
    return key;
}

test parse_lock {
    const lock_input =
        \\#####
        \\.####
        \\..###
        \\...##
        \\....#
        \\.....
        \\.....
    ;
    const lock = parse_lock(lock_input);
    try expectEqual(0, lock.pins[0]);
    try expectEqual(1, lock.pins[1]);
    try expectEqual(2, lock.pins[2]);
    try expectEqual(3, lock.pins[3]);
    try expectEqual(4, lock.pins[4]);
}

test parse_key {
    const key_input =
        \\.....
        \\.....
        \\#....
        \\##...
        \\###..
        \\####.
        \\#####
    ;
    const key: Key = parse_key(key_input);
    try expectEqual(4, key.heights[0]);
    try expectEqual(3, key.heights[1]);
    try expectEqual(2, key.heights[2]);
    try expectEqual(1, key.heights[3]);
    try expectEqual(0, key.heights[4]);
}

// test parse_either {
//     const key =
//         \\.....
//         \\.....
//         \\#....
//         \\##...
//         \\###..
//         \\####.
//         \\#####
//     ;
//     const key_heights = parse_either(key);
//     try expectEqual(4, key_heights[0]);
//     try expectEqual(3, key_heights[1]);
//     try expectEqual(2, key_heights[2]);
//     try expectEqual(1, key_heights[3]);
//     try expectEqual(0, key_heights[4]);
// }
