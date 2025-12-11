import { describe, test, expect } from 'vitest';
import { readFileSync } from 'fs';

import { part1, part2 } from './day10';

const day = "10"

const t1 = readFileSync(`day${day}.p1.testdata.txt`).toString();
const t1a = parseInt(readFileSync(`day${day}.p1.answer.txt`).toString());
const t2a = parseInt(readFileSync(`day${day}.p2.answer.txt`).toString());

const sample = `
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}`;

describe("part1", () => {
    test.each([
        [`[#] (0) {3}`, 1],
        [`[##] (0) (1) {3}`, 2],
        [`
[##] (0) (1) {3}
[###] (0,2) (1) {3}`, 4],
        [sample, 7],
        [t1, t1a],
    ])("%$", (input: string, expected: number) => {
        expect(part1(input.trim())).toBe(expected);
    });
});

describe("part2", () => {
    test.each([
        [`[#] (0) {3}`, 3],
        [`[#] (0) (1) {3,2}`, 5],
        [`[###] (0) (1,2) (2) {3,2,5}`, 8],
        [sample, 33],
        [t1, t2a],
    ])("%$", async (input: string, expected: number) => {
        expect(await part2(input.trim())).toBe(expected);
    });
});