#! /bin/sh

testEmptyMemory() {
  result=`rockstar aoc-day3.rock`
  assertEquals "0" "${result}"
}

testMul1by1() {
  result=`rockstar aoc-day3.rock "mul(1,1)"`
  assertEquals "1" "${result}"
}

testMul2by1() {
  result=`rockstar aoc-day3.rock "mul(2,1)"`
  assertEquals "2" "${result}"
}

testMul2by2() {
  result=`rockstar aoc-day3.rock "mul(2,2)"`
  assertEquals "4" "${result}"
}

# Load shUnit2.
. shunit2