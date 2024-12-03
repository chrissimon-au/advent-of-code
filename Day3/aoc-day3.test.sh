#! /bin/sh

testEmptyMemory() {
  result=`rockstar aoc-day3.rock`
  assertEquals "0" "${result}"
}

testMul1by1() {
  result=`rockstar aoc-day3.rock "mul(1,1)"`
  assertEquals "1" "${result}"
}

# Load shUnit2.
. shunit2