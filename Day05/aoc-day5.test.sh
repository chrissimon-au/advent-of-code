#! /bin/sh

BIN_PATH="bin"
OUT_PATH="out"
EXE_PATH="$PWD/$BIN_PATH/Day05"
OUTFILE_PATH="$PWD/$OUT_PATH/output.txt"
mkdir -p $BIN_PATH
mkdir -p $OUT_PATH

echo "Compiling to $EXE_PATH"
echo "Outputting to $OUTFILE_PATH"

qb64 -x $PWD/Day05.bas -o $EXE_PATH

day05() {
  rm $OUTFILE_PATH
  $EXE_PATH $OUTFILE_PATH $1
  echo `cat $OUTFILE_PATH`
}

testCheck() {
  result=`day05`
  assertEquals "0" "${result}"
}

testCheckWithInput() {
  result=`day05 1`
  assertEquals "1" "${result}"
}

testError() {
  result=`day05 5`
  assertEquals "" "${result}"
}

# Load shUnit2.
. shunit2