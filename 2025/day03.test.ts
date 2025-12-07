import { describe, test, expect } from 'vitest';
import { readFileSync } from 'fs';

import { part1, part2 } from './day03.ts';

const t1 = readFileSync("day03.p1.testdata.txt").toString();
const t1a = parseInt(readFileSync("day03.p1.answer.txt").toString());

describe("part1", () => {
    test.each([
        ["21", 21],
        ["98", 98],
        ["968", 98],
        ["869", 89],
        ["987654321111111", 98],
        ["811111111111119", 89],
        ["234234234234278", 78],
        ["818181911112111", 92],
        ["21\n98", 21 + 98],
        [
            `987654321111111
        811111111111119
        234234234234278
        818181911112111`, 357],
        [t1, t1a]
    ])("%s --> %s", (input: string, expected: number) => {
        expect(part1(input)).toBe(expected);
    });
});