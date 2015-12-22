# Compile a single mandala program and save the output

#!/bin/bash

# Author: Harsha Vemuri 

filename=$"suite/"$1

preprocessor="../../compiler/preprocessor.py"
run="../../compiler/run"
j_file="Program.java"
exe="Program"
warnings="../tests/fullstack/warnings.txt"

# BUILDING
cd ../../compiler
echo "" > $warnings 
make 2> $warnings
cd ../tests/fullstack

# PREPROCESSING
python $preprocessor $filename
p_file=$(find suite -name *\.proc) 

# JAVA GENERATION
./$run < $p_file > "suite/Program.java" || {
  t_filename=${filename%.*}
  t_filename=$"suite/solutions/"${t_filename##*/}$".txt"
  echo "ERROR" > $t_filename
  cd suite
  rm -f *.proc
  mv Turtle.java Turtle.java.keep
  rm -f *.java
  mv Turtle.java.keep Turtle.java
  cd ..
  exit 0
}

# JAVA COMPILATION
cd suite
javac $j_file 

# SAVE OUTPUT
t_filename=${filename%.*}
t_filename=${t_filename##*/}$".txt"
cat "Program.java" > $"solutions/"$t_filename

# CLEANING
rm -f *.proc
mv Turtle.java Turtle.java.keep
#rm -f *.java
mv Turtle.java.keep Turtle.java
rm -f *.class
cd ..