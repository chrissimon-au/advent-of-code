import { describe, test, expect } from 'vitest';
import { readFileSync } from 'fs';

import { part1, part2 } from './day06';

const day = "06"

const t1 = readFileSync(`day${day}.p1.testdata.txt`).toString();
const t1a = parseInt(readFileSync(`day${day}.p1.answer.txt`).toString());
const t2a = parseInt(readFileSync(`day${day}.p2.answer.txt`).toString());

describe("part1", () => {
    test.each([
        [`0
1
*`, 0],
        [`0
1
+`, 1],
        [`2
3
4
+`, 9],
        [`2
3
4
*`, 24],
        [`2 2
3 3
4  4
+  *`, 33],
        [`123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  `, 4277556],
        [t1, t1a],
    ])("%$", (input: string, expected: number) => {
        expect(part1(input)).toBe(expected);
    });
});

describe("part2", () => {
    test.each([
        [`0 
10
*`, 0],
        [`0 
10
+ `, 1],
[`1 
21 
+  `, 13],
[`1  34
21 21
+   +`, 86],
        [`123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  `, 3263827],
        [t1, t2a],
    ])("%$", (input: string, expected: number) => {
        expect(part2(input)).toBe(expected);
    });
});