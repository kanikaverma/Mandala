# automated regression testing on full programs

#!/bin/bash

preprocessor="../../compiler/preprocessor.py"
semantic="../../compiler/semantic.sh"
bitch="../../compiler/run_bitch"

echo "*****************************************"
echo "****************CLEANING*****************"
echo "*****************************************"

cd ../../compiler
make clean
make
cd ../tests/fullstack

echo "*****************************************"
echo "**************PREPROCESSING**************"
echo "*****************************************"

files=$(find suite -name *\.mandala)

for file in $files
do
  python $preprocessor $file 
done

processed_files=$(find suite -name *\.proc)

for file in $processed_files
do
  echo $file
done

echo "*****************************************"
echo "************JAVA GENERATION**************"
echo "*****************************************"

processed_files=$(find suite -name *\.proc)

for file in $processed_files
do
  cat $file | ./$bitch > "suite/Program.java"
done

echo "*****************************************"
echo "*******COMPILING & EXECUTING JAVA********"
echo "*****************************************"

java_files=$(find suite -name *\.java)

for file in $java_files
do
  javac $file
  java {$file%.*}
done

# cplusplus_files=$(find suite -name *\.cpp)

# for file in $cplusplus_files
# do
#   g++ $file -o ${file%.*}
# done

# echo "*****************************************"
# echo "***************EXECUTING*****************"
# echo "*****************************************"

# for file in $cplusplus_files
# do
#   executable=${file%.*}
#   echo $executable
#   output_file=${file%.*}$".txt"
#   echo $output_file
#   ./$executable > $output_file
# done
