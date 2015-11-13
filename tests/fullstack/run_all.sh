# automated regression testing on full programs

#!/bin/bash

preprocessor="../../compiler/preprocessor.py"
semantic="../../compiler/semantic.sh"

echo "*****************************************"
echo "****************CLEANING*****************"
echo "*****************************************"

cd ../..
make clean
cd tests/fullstack

echo "*****************************************"
echo "**************PREPROCESSING**************"
echo "*****************************************"

files=$(find suite -name *\.mandala)

for file in $files
do
  python $preprocessor $file 
done

echo "*****************************************"
echo "***********SEMANTIC CHECKING*************"
echo "*****************************************"

processed_files=$(find suite -name *\.proc)

for file in $processed_files
do
  sh $semantic $file
done

echo "*****************************************"
echo "*************COMPILING C++***************"
echo "*****************************************"

cplusplus_files=$(find suite -name *\.cpp)

for file in $cplusplus_files
do
  g++ $file -o ${file%.*}
done

echo "*****************************************"
echo "***************EXECUTING*****************"
echo "*****************************************"

for file in $cplusplus_files
do
  executable=${file%.*}
  echo $executable
  output_file=${file%.*}$".txt"
  echo $output_file
  ./$executable > $output_file
done