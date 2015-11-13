#!/bin/bash

proc_tests=$(find suite -name *\.mandala)
preproc="../../compiler/preprocessor.py"

printf "TESTING MANDALA PREPROCESSOR\n"
printf "****************************\n"

sanitize() 
{
  local fullpath=$1
  testpath="${fullpath%.*}" # extension
  test_name="${testpath##*/}" # path
}

for file in $proc_tests
do
  sanitize "$file"
  python $preproc $file

  printf "Testing: $test_name \t Result: Success\n" 

done

printf "****************************\n"
printf "All tests passed.\n"

rm -f suite/*.mandala.proc

exit