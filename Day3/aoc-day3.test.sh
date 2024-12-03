#! /bin/sh

testEmptyMemory() {
  result=`rockstar aoc-day3.rock`
  assertEquals "0" "${result}"
}

# Load shUnit2.
. shunit2