#!/bin/bash
inputDir=test/testcase/Batsh
for filename in ${inputDir}/*.batsh; do
  caseName=$(basename -s .batsh $filename)
  echo $caseName
  input=${inputDir}/${caseName}.batsh
  output=${inputDir}/${caseName}
  ./batsh batsh $input $output --ast
  diff $input $output
  rm $output
done
