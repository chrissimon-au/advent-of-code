#! /bin/sh

BIN_PATH="bin"
IO_PATH="io"
EXE_PATH="$PWD/$BIN_PATH/Day05"
INFILE_PATH="$PWD/$IO_PATH/infile.txt"
OUTFILE_PATH="$PWD/$IO_PATH/output.txt"
mkdir -p $BIN_PATH
mkdir -p $IO_PATH

echo "Compiling to $EXE_PATH"
echo "Reading input from $INFILE_PATH"
echo "Outputting to $OUTFILE_PATH"

qb64 -x $PWD/Day05.bas -o $EXE_PATH

day05() {
  rm $OUTFILE_PATH
  echo "$1" > $INFILE_PATH
  $EXE_PATH $INFILE_PATH $OUTFILE_PATH
  cat $OUTFILE_PATH
}

testSinglePageUpdate() {
  result=`day05 "2|3

6"`
  assertEquals "6" "${result}"
}

testSingleUpdateMultiplePages() {
  result=`day05 "2|3

6,7,8"`
  assertEquals "7" "${result}"
}

testMultipleUpdatesMultiplePages() {
  result=`day05 "2|3

6,7,8
4,5,6"`
  assertEquals "12" "${result}"
}

testMultipleUpdatesMultiplePagesWithInvalidUpdate() {
  result=`day05 "2|3

6,3,2
4,5,6"`
  assertEquals "5" "${result}"
}

testMultipleUpdatesMultiplePagesMultipleRulesWithInvalidUpdate() {
  result=`day05 "2|3
5|4

6,3,2
4,5,6
50,12,72,11,15"`
  assertEquals "72" "${result}"
}

testMoreRulesAndUpdates() {
  result=`day05 "2|3
5|4
12|11
15|72

6,3,2
4,5,6
50,12,72,11,15
9"`
  assertEquals "9" "${result}"
}

testSampleData() {
  input=`cat sampledata.txt`
  expected=`cat sampledata.answer.txt`
  result=`day05 "${input}"`
  assertEquals "${expected}" "${result}"
}

testTestData() {
  input=`cat testdata.txt`
  expected=`cat testdata.answer.txt`
  result=`day05 "${input}"`
  assertEquals "${expected}" "${result}"
}

# Load shUnit2.
. shunit2