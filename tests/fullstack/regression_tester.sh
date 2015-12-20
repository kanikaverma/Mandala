# Automated regression testing on full programs
# Functionality for multiple files included 

#!/bin/bash

preprocessor="../../compiler/preprocessor.py"
run="../../compiler/run"
j_file="Program.java"
exe="Program"
warnings="../tests/fullstack/warnings.txt"
compare="compare.py"

# BUILDING

cd ../../compiler
echo "" > $warnings 
make 2> $warnings
cd ../tests/fullstack

# GET ALL MANDALA FILES 
mandala_files=$(find suite -name *\.mandala)

for m_file in $mandala_files
do
  # PREPROCESSING
  python $preprocessor $m_file
  p_file=$(find suite -name *\.proc) 

  # JAVA GENERATION
  ./$run < $p_file > "suite/Program.java" & 

  # JAVA COMPILATION
  cd suite 
  javac $j_file

  # EXECUTION 
  java $exe

  # COMPARING
  diff=$(python $compare output.jpg Program.jpg)

  if [[ $diff -eq 0 ]]; then
    echo "Output Correct: [y]"
  else
    echo "Output Correct: [ ]"
  fi

  # CLEANING 
  rm -f *.proc
  mv Turtle.java Turtle.java.keep
  rm -f *.java
  mv Turtle.java.keep Turtle.java
  rm -f *.class
  cd ..
done