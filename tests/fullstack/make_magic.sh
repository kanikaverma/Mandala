# compile a single mandala program and display the result 

#!/bin/bash

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
./$run < $p_file > "suite/Program.java" & 

# JAVA COMPILATION
cd suite
javac $j_file

# EXECUTION
java $exe 

# SAVE OUTPUT
# todo

# CLEANING
rm -f *.proc
mv Turtle.java Turtle.java.keep
#rm -f *.java
mv Turtle.java.keep Turtle.java
rm -f *.class
cd ..