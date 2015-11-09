#!/bin/bash

path="../compiler"

ocamllex ${path}/scanner.mll
ocamlyacc ${path}/parser.mly
ocamlc -c ${path}/ast.mli
ocamlc -c ${path}/parser.mli
ocamlc -c ${path}/scanner.ml
ocamlc -c ${path}/parser.ml
