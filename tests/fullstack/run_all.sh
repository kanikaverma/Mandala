# automated regression testing on full programs

#!/bin/bash

preprocessor="../../compiler/preprocessor.py"
semantic="../../compiler/semantic.sh"
bitch="../../compiler/run_bitch"
dependency="Turtle.java"
dependency_compiled="Turtle.class"
warnings="../tests/fullstack/warnings.txt"

echo ""
echo "*****************************************"
echo "****************BUILDING*****************"
echo "*****************************************"
echo ""

cd ../../compiler
make 2> $warnings
cd ../tests/fullstack

echo ""
echo "*****************************************"
echo "**************PREPROCESSING**************"
echo "*****************************************"
echo "" 

files=$(find suite -name *\.mandala)

for file in $files
do
  python $preprocessor $file 
done

processed_files=$(find suite -name *\.proc)

for file in $processed_files
do
  echo "Processed: "${file##*/}
done

echo ""
echo "*****************************************"
echo "*************JAVA GENERATION*************"
echo "*****************************************"
echo ""

processed_files=$(find suite -name *\.proc)

for file in $processed_files
do
  cat $file | ./$bitch > "suite/Program.java"
done

java_files=$(find suite -name *\.java)

for file in $java_files
do
  if [ "${file##*/}" != "$dependency" ]; then
    echo "Generated: "${file##*/}
  fi
done

echo ""
echo "*****************************************"
echo "***************COMPILING*****************"
echo "*****************************************"
echo ""

cd suite 

for file in $java_files
do
  file_base=${file##*/}
  javac $file_base $dependency
done

cd ..
exec_files=$(find suite -name *\.class)

for file in $exec_files
do
  if [ "${file##*/}" != "$dependency_compiled" ]; then
    echo "Compiled: "${file##*/}
  fi
done

echo ""
echo "*****************************************"
echo "***************EXECUTING*****************"
echo "*****************************************"
echo ""

cd suite

for file in $exec_files
do
  if [ "${file##*/}" != "$dependency_compiled" ]; then
    file_base=${file##*/}
    exec_file=${file_base%.*}
    java $exec_file
  fi
done

echo ""
echo "*****************************************"
echo "****************CLEANING*****************"
echo "*****************************************"
echo ""

rm -f *.class
rm -f *.proc
mv $dependency $dependency$".keep"
rm -f *.java
mv $dependency$".keep" $dependency

cd ../../../compiler
make clean 2>> $warnings
echo "" 