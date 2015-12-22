# Automated regression testing

#!/bin/bash

# Author: Harsha Vemuri

# COMPONENTS
preprocessor="../../compiler/preprocessor.py"
run="../../compiler/run"
j_file="Program.java"
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

  # PASSING TESTS 
  if [[ $m_file == *"p_"* ]]
  then 

    # PREPROCESSING
    python $preprocessor $m_file
    p_file=$(find suite -name *\.proc) 

    # JAVA GENERATION
    ./$run < $p_file > "suite/Program.java" 

    # JAVA COMPILATION
    cd suite 
    javac $j_file

    #COMPARING
    t_filename=${m_file%.*}
    t_filename=${t_filename##*/}$".txt"
    compareTo=$"solutions/"$t_filename

    diff=$(python $compare Program.java $compareTo)

    if [[ $diff -eq 0 ]]; then
      echo "Output Correct: [y]"$" for ${m_file##*/}"
    else
      echo "Output Correct: [n]"$" for ${m_file##*/}"
    fi

  # TESTS THAT FAIL 
  else
    t_filename=${m_file%.*}
    t_filename=$"suite/solutions/"${t_filename##*/}$".txt"
    err=$(<$t_filename)
    if [[ $err == "ERROR" ]]
    then
      echo "Output Correct: [y]"$" for ${m_file##*/}"
    else
      echo "Output Correct: [n]"$" for ${m_file##*/}"
    fi

    cd suite 
    
  fi

  # CLEANING 
  rm -f *.proc
  mv Turtle.java Turtle.java.keep
  rm -f *.java
  mv Turtle.java.keep Turtle.java
  rm -f *.class
  cd ..

done