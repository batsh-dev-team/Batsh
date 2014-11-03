#!/bin/bash
inputDir=test/testcase/Batsh
for filename in ${inputDir}/*.batsh; do
  caseName=$(basename -s .batsh $filename)
  echo $caseName
  input=${inputDir}/${caseName}.batsh
  output=${inputDir}/${caseName}
  ./batsh batsh $input $output --ast --symbols
  diff $input $output
  if [ "$?" == $((0)) ]; then
    rm $output
  else
    break
  fi
done
