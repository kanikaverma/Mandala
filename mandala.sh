# compile and execute a mandala program

#!/bin/bash

filename=$"src/"$1

preprocessor="compiler/preprocessor.py"
run="compiler/run"
j_file="Program.java"
exe="Program"
warnings="tests/fullstack/warnings.txt"

# BUILDING
echo "" > $warnings 
make 2> $warnings

# PREPROCESSING
python $preprocessor $filename
p_file=$filename$".proc"

# JAVA GENERATION
./$run < $p_file > $"src/"$j_file & 

# JAVA COMPILATION
cd src
javac $j_file 

# EXECUTION
java $exe 

# CLEANING
rm -f *.proc
mv Turtle.java Turtle.java.keep
rm -f *.java
mv Turtle.java.keep Turtle.java
rm -f *.class
cd ..