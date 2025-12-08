import { describe, test, expect } from 'vitest';
import { readFileSync } from 'fs';

import { part1, part2 } from './day08';

const day = "08"

const t1 = readFileSync(`day${day}.p1.testdata.txt`).toString();
const t1a = parseInt(readFileSync(`day${day}.p1.answer.txt`).toString());
const t2a = parseInt(readFileSync(`day${day}.p2.answer.txt`).toString());

describe("part1", () => {
    test.each([
        [`
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689`, 10, 3, 40],
        [t1, 1000, 3, t1a],
    ])("%$", (input: string, numConnections: number, numCircuits: number, expected: number) => {
        expect(part1(input.trim(), numConnections, numCircuits)).toBe(expected);
    });
});