#! /bin/sh

testEmptyMemory() {
  result=`picat day19.pi`
  assertEquals "0" "${result}"
}

# Load shUnit2.
. shunit2