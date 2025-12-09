import { describe, test, expect } from 'vitest';
import { readFileSync } from 'fs';

import { part1, part2 } from './day09';

const day = "09"

const t1 = readFileSync(`day${day}.p1.testdata.txt`).toString();
const t1a = parseInt(readFileSync(`day${day}.p1.answer.txt`).toString());
const t2a = parseInt(readFileSync(`day${day}.p2.answer.txt`).toString());

const sample = `
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3`;

describe("part1", () => {
    test.each([
        [`
0,0
1,1`, 4],
        [`
1,1
0,0`, 4],
        [`
1,1
0,0
5,5`, 36],
        [sample, 50],
        [t1, t1a],
    ])("%$", (input: string, expected: number) => {
        expect(part1(input.trim())).toBe(expected);
    });
});

describe("part2", () => {
    test.each([
        [`
0,0
4,0
3,2
1,1`, -1],
        [sample, 24],
        [t1, t2a],
    ])("%$", (input: string, expected: number) => {
        expect(part2(input.trim())).toBe(expected);
    });
});