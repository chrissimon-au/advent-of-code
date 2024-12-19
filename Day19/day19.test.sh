#! /bin/sh

testEmptyMemory() {
  USAGE=$(cat <<END
r

r
END
)
  result=`picat day19.pi "$USAGE"`
  assertEquals "1" "${result}"
}

# Load shUnit2.
. shunit2