#! /bin/sh

BIN_PATH="bin"
IO_PATH="io"
EXE_PATH="$PWD/$BIN_PATH/Day05Part2"
INFILE_PATH="$PWD/$IO_PATH/infile.txt"
OUTFILE_PATH="$PWD/$IO_PATH/output.txt"
mkdir -p $BIN_PATH
mkdir -p $IO_PATH

echo "Compiling to $EXE_PATH"
echo "Reading input from $INFILE_PATH"
echo "Outputting to $OUTFILE_PATH"

qb64 -x $PWD/Day05Part2.bas -o $EXE_PATH

day05() {
  rm $OUTFILE_PATH
  echo "$1" > $INFILE_PATH
  $EXE_PATH $INFILE_PATH $OUTFILE_PATH
  cat $OUTFILE_PATH
}

testSinglePageUpdate() {
  result=`day05 "2|3

3,2,1"`
  assertEquals "3" "${result}"
}

testSinglePageUpdateInSecondSequence() {
  result=`day05 "1|2

3,2,1"`
  assertEquals "1" "${result}"
}

testSampleData() {
  input=`cat sampledata.txt`
  expected=`cat sampledata.part2.answer.txt`
  result=`day05 "${input}"`
  assertEquals "${expected}" "${result}"
}

# Load shUnit2.
. shunit2