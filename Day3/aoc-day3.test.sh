#! /bin/sh

testEmptyMemory() {
  result=`rockstar aoc-day3.rock`
  assertEquals "" "${result}"
}

# Load shUnit2.
. shunit2