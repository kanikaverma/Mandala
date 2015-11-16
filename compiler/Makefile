default: run semantic

run: scanner parser semantic gen_java
	ocamlc -o run scanner.cmo parser.cmo semantic.cmo gen_java.cmo

gen_java: sast
	ocamlc -c gen_java.ml

semantic: scanner parser semantic
	ocamlc -o semantic parser.cmo scanner.cmo semantic.cmo


semantic: sast scanner
	ocamlc -c semantic.ml

scanner: parser
	ocamllex scanner.mll; ocamlc -c scanner.ml

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml


sast: ast
	ocamlc -c sast.mli

ast:
	ocamlc -c ast.mli

.PHONY: clean
clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.proc
	rm -f scanner.ml
	rm -f parser.ml
	rm -f parser.mli
