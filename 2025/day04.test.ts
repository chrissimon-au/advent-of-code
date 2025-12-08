import { describe, test, expect } from 'vitest';
import { readFileSync } from 'fs';

import { part1, part2 } from './day04';

const t1 = readFileSync("day04.p1.testdata.txt").toString();
const t1a = parseInt(readFileSync("day04.p1.answer.txt").toString());
const t2a = parseInt(readFileSync("day04.p2.answer.txt").toString());

describe("part1", () => {
    test.each([
        [".", 0],
        ["@", 1],
        [`
            @@@
            @@@
            @@@`, 4],
        [`
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.`, 13],
        [t1, t1a],
    ])("%$", (input: string, expected: number) => {
        expect(part1(input)).toBe(expected);
    });
});


describe("part2", () => {
    test.each([
        [".", 0],
        ["@", 1],
        [`
            @@@
            @@@
            @@@`, 9],

        [t1, t2a]
    ])("%$", (input: string, expected: number) => {
        expect(part2(input)).toBe(expected);
    })
});