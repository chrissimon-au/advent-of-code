import { test, expect } from 'vitest'
import { part1, part2 } from './day01'
import { readFileSync } from 'fs';
test("none", () => expect(part1("R1")).toBe(0));
test("one", () => expect(part1("L25\nL25")).toBe(1));
test("right", () => expect(part1("R25\nL75")).toBe(1));
const sample = `L68
L30
R48
L5
R60
L55
L1
L99
R14
L82`;
test("sample", () => expect(part1(sample)).toBe(3));


const t1 = readFileSync("testdata.day01.part1.txt").toString();
const t1a = parseInt(readFileSync("testdata.day01.part1.answer.txt").toString());

test("part1", () => expect(part1(t1)).toBe(t1a));

test("p2 simple", () => expect(part2("R25")).toBe(0));
test("p2 1 rotate", () => expect(part2("R60")).toBe(1));
test("p2 back", () => expect(part2("R60\nL10")).toBe(2));
test("p2 double rotate", () => expect(part2("R110\nR40")).toBe(2));
test("p2 triple rotate", () => expect(part2("R350\nL100\nR10")).toBe(5));

test("p2 c1", () => expect(part2("L199")).toBe(2));


test("sample2", () => expect(part2(sample)).toBe(6));
const t2a = parseInt(readFileSync("testdata.day01.part2.answer.txt").toString())
test("part2", () => expect(part2(t1)).toBe(t2a));
