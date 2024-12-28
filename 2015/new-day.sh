mkdir -p day$1
pushd day$1
cp ../day-template/dayDD.rkt day$1.rkt
touch test.data
touch test-answer.data
touch test-answer2.data
touch sample.data
touch sample-answer.data
touch sample-answer2.data
sed -i 's/DD/'"$1"'/g' day$1.rkt
popd