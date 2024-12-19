#! /bin/sh

testSinglePatternSingleCombo() {
  USAGE=$(cat <<END
r

r
END
)
  result=`picat day19.pi "$USAGE"`
  assertEquals "1" "${result}"
}

testTwoPatternsTwoCombosNoMixingRequired() {
  USAGE=$(cat <<END
r, wq

r
wq
q
END
)
  result=`picat day19.pi "$USAGE"`
  assertEquals "2" "${result}"
}


# Load shUnit2.
. shunit2