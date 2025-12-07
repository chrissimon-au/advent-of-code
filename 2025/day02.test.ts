import {describe as d, test as t,expect as e} from'vitest';
import { readFileSync } from 'fs';

import { part1 as p1} from './day02.ts'

d("part1",()=> {
t("no bad ids", () => e(p1("1698522-169852")).toBe(0));
t("small range ids", () => e(p1("446443-446449")).toBe(446446));
t("diff digits", () => e(p1("998-1012")).toBe(1010));
t("multiple ids", () => e(p1("11-22")).toBe(33));
t("multiple ranges", () => e(p1("11-22,95-115")).toBe(132));
t("first below start", () => e(p1("4959-5051")).toBe(5050));
t("sample", () => e(p1("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")).toBe(1227775554));
const t1 = readFileSync("day02.p1.testdata.txt").toString();
const t1a = parseInt(readFileSync("day02.p1.answer.txt").toString());
t("p1", () => e(p1(t1)).toBe(t1a));
});