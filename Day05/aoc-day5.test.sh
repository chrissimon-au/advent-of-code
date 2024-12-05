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
  echo `cat $OUTFILE_PATH`
}

testSinglePageUpdate() {
  result=`day05 "1|2

5"`
  assertEquals "5" "${result}"
}

# Load shUnit2.
. shunit2