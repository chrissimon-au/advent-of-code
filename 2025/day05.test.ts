import { describe, test, expect } from 'vitest';
import { readFileSync } from 'fs';
import { part1, part2 } from './day05';

const day = "05"

const t1 = readFileSync(`day${day}.p1.testdata.txt`).toString();
const t1a = parseInt(readFileSync(`day${day}.p1.answer.txt`).toString());
const t2a = parseInt(readFileSync(`day${day}.p2.answer.txt`).toString());

describe("part1", () => {
    test.each([
        [
            `3-5

1`, 0],
        [
            `3-5

4`, 1],
        [
            `3-5
10-14
16-20
12-18

1
5
8
11
17
32`, 3],
        [t1, t1a],
    ])("%$", (input: string, expected: number) => {
        expect(part1(input)).toBe(expected);
    });
});


// describe("part2", () => {
//     test.each([


//         //[t1, t2a]
//     ])("%$", (input: string, expected: number) => {
//         expect(part2(input)).toBe(expected);
//     })
// });