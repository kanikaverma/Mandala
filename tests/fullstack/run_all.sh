# Automated regression testing on full programs

#!/bin/bash

preprocessor="../../compiler/preprocessor.py"
semantic="../../compiler/semantic.sh"
run="../../compiler/run"
dependency="Turtle.java"
dependency_compiled="Turtle.class"
expected="Expected.java"
warnings="../tests/fullstack/warnings.txt"
compare="compare.py"

SignalError() {
  if [ $error -eq 0 ] ; then
    echo "FAILED"
    error=1
  fi
  echo " $1"
}

echo ""
echo "*****************************************"
echo "****************BUILDING*****************"
echo "*****************************************"
echo ""

cd ../../compiler
echo "" > $warnings 
echo "" > $expected
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

echo "Mandala m = Create Mandala;
Shape circle1 = Create Shape: {Geo circle Size 30.0 Color yellow Rotation 0.0};
Shape square1 = Create Shape: {Geo square Size 50.0 Color yellow Rotation 0.0};
Layer layer1 = Create Layer: {Radius 100.0 Shape circle1 Count 4 Offset 1.0 AngularShift 0};
Layer layer2 = Create Layer: {Radius 100.0 Shape square1 Count 4 Offset 0.0 AngularShift 0};
addTo: (m, layer1, layer2);
" | ./$run > "suite/Program.java"

processed_files=$(find suite -name *\.proc)

for file in $processed_files
do
  # cat $file
  #./$run < $file > "suite/Program.java" &
  # ./$run < $file > "suite/Program.java" &
  # echo $contents > output & ./$run output > "suite/Program.java" &
  echo "hi"
done

java_files=$(find suite -name *\.java)

for file in $java_files
do
  if [ "${file##*/}" != "$dependency" ] && [ "${file##*/}" != "$expected" ]; then
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
  if [ "${file##*/}" != "$expected" ]; then
    file_base=${file##*/}
    javac $file_base $dependency
  fi
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

echo "HELLO WORLD"

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
echo "***************COMPARING*****************"
echo "*****************************************"
echo ""

diff=$(python $compare output_HelloWorld.jpg ${file_base%.*}$".jpg")

if [[ $diff -eq 0 ]]; then
  echo "Output Correct: [y]"
else
  echo "Output Correct: [ ]"
fi

echo ""
echo "*****************************************"
echo "****************CLEANING*****************"
echo "*****************************************"
echo ""

rm -f *.class
rm -f *.proc
mv $dependency $dependency$".keep"
mv $expected $expected$".keep"
#rm -f *.java
mv $dependency$".keep" $dependency
mv $expected$".keep" $expected

cd ../../../compiler
make clean 2>> $warnings
echo ""
