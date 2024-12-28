const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const expectEqual = std.testing.expectEqual;

const KeyOrLock = union(enum) { key: Key, lock: Lock, neither };

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

fn parse_key(input: []const u8, allocator: Allocator) Key {
    var row_input = std.mem.split(u8, input, "\n");
    var rows = ArrayList([]const u8).init(allocator);
    defer rows.deinit();
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

fn parse_either(input: []const u8, allocator: Allocator) KeyOrLock {
    return switch (input[0]) {
        '.' => KeyOrLock{ .key = parse_key(input, allocator) },
        '#' => KeyOrLock{ .lock = parse_lock(input) },
        else => KeyOrLock.neither,
    };
}

fn parse_input(input: []const u8, allocator: Allocator) ArrayList(KeyOrLock) {
    var items = std.mem.split(u8, input, "\n\n");
    var parsed_items = ArrayList(KeyOrLock).init(allocator);
    while (items.next()) |item| {
        parsed_items.append(parse_either(item, std.testing.allocator)) catch {};
    }
    return parsed_items;
}

fn key_matches_lock(key: Key, lock: Lock) bool {
    var matches: bool = true;
    for (0..5) |i| {
        matches = matches and (key.heights[i] + lock.pins[i] < 6);
    }
    return matches;
}

fn count_matching_keys_for_lock(lock: Lock, all_items: ArrayList(KeyOrLock)) u64 {
    var num_matching_keys: u64 = 0;
    for (all_items.items) |item| {
        switch (item) {
            .key => |key| {
                if (key_matches_lock(key, lock)) {
                    num_matching_keys += 1;
                }
            },
            else => {},
        }
    }
    return num_matching_keys;
}

fn count_key_lock_combos(input: []const u8, allocator: Allocator) u64 {
    var all_items = parse_input(input, allocator);
    defer all_items.deinit();

    var num_combos: u64 = 0;

    for (all_items.items) |item| {
        switch (item) {
            .lock => |lock| num_combos += count_matching_keys_for_lock(lock, all_items),
            else => {},
        }
    }
    return num_combos;
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
    const key: Key = parse_key(key_input, std.testing.allocator);
    try expectEqual(4, key.heights[0]);
    try expectEqual(3, key.heights[1]);
    try expectEqual(2, key.heights[2]);
    try expectEqual(1, key.heights[3]);
    try expectEqual(0, key.heights[4]);
}

test "parse either as key" {
    const key_input =
        \\.....
        \\#....
        \\#.#..
        \\###..
        \\###..
        \\####.
        \\#####
    ;
    const key = parse_either(key_input, std.testing.allocator).key;
    try expectEqual(5, key.heights[0]);
    try expectEqual(3, key.heights[1]);
    try expectEqual(4, key.heights[2]);
    try expectEqual(1, key.heights[3]);
    try expectEqual(0, key.heights[4]);
}

test "parse either as lock" {
    const lock_input =
        \\#####
        \\.####
        \\.###.
        \\..##.
        \\..#.
        \\..#..
        \\.....
    ;
    const lock = parse_either(lock_input, std.testing.allocator).lock;
    try expectEqual(0, lock.pins[0]);
    try expectEqual(2, lock.pins[1]);
    try expectEqual(5, lock.pins[2]);
    try expectEqual(3, lock.pins[3]);
    try expectEqual(1, lock.pins[4]);
}

test parse_input {
    const input =
        \\#####
        \\.####
        \\.###.
        \\..##.
        \\..#.
        \\..#..
        \\.....
        \\
        \\.....
        \\#....
        \\#.#..
        \\###..
        \\###..
        \\####.
        \\#####
    ;
    const items = parse_input(input, std.testing.allocator);
    defer items.deinit();
    const lock = items.items[0].lock;
    try expectEqual(0, lock.pins[0]);
    try expectEqual(2, lock.pins[1]);
    try expectEqual(5, lock.pins[2]);
    try expectEqual(3, lock.pins[3]);
    try expectEqual(1, lock.pins[4]);
    const key = items.items[1].key;
    try expectEqual(5, key.heights[0]);
    try expectEqual(3, key.heights[1]);
    try expectEqual(4, key.heights[2]);
    try expectEqual(1, key.heights[3]);
    try expectEqual(0, key.heights[4]);
}

// Based on https://ziggit.dev/t/the-zig-way-to-read-a-file/4663/4
fn load_file(file_name: []const u8, allocator: Allocator) anyerror![]const u8 {
    const file = std.fs.cwd().openFile(file_name, .{}) catch |err| {
        std.log.err("Failed to open file: {s}", .{@errorName(err)});
        return err;
    };
    defer file.close();

    return try file.reader().readAllAlloc(allocator, std.math.maxInt(usize));
}

test "can parse_sample_input" {
    const input = try load_file("sampledata.txt", std.testing.allocator);
    defer std.testing.allocator.free(input);

    const items = parse_input(input, std.testing.allocator);
    defer items.deinit();

    try expectEqual(5, items.items.len);
}

fn expect_input_file_matches_answer(fileBase: []const u8, allocator: Allocator) anyerror!void {
    const dataFileName = try std.mem.concat(allocator, u8, &[_][]const u8{ fileBase, "data.txt" });
    defer std.testing.allocator.free(dataFileName);
    const input = try load_file(dataFileName, std.testing.allocator);
    defer std.testing.allocator.free(input);

    const answerFileName = try std.mem.concat(allocator, u8, &[_][]const u8{ fileBase, "data.answer.txt" });
    defer std.testing.allocator.free(answerFileName);
    const answerStr = try load_file(answerFileName, std.testing.allocator);
    defer std.testing.allocator.free(answerStr);

    const answer = try std.fmt.parseInt(u64, answerStr, 10);

    try expectEqual(answer, count_key_lock_combos(input, std.testing.allocator));
}

test "count key lock combos AoC Sample Case" {
    try expect_input_file_matches_answer("sample", std.testing.allocator);
}

test "count key lock combos AoC Test Case" {
    try expect_input_file_matches_answer("test", std.testing.allocator);
}
