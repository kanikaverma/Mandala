#!/bin/bash

cd ../compiler

set -e

PARSE='
open Ast;;\n
\n
let lexbuf Lexing.from_channel stdin in\n
Parser.program Scanner.token lexbuf;;'
(echo -e $PARSE; cat -) | ocaml scanner.cmo parser.cmo
