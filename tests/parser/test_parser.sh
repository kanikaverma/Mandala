#! /bin/bash

infile="parser_tests.txt"
outfile="parser_error_log.txt"

"" > "$outfile"

while IFS= read line 
do
    echo "$line" >> "$outfile"
    echo "$line" | menhir --interpret ../../compiler/parser.mly --explain >> "$outfile"
done <"$infile" 