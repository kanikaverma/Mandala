default: run semantic java_semantic

run: scanner parser semantic java_semantic gen_java
	ocamlc -o run scanner.cmo parser.cmo semantic.cmo java_semantic.cmo gen_java.cmo

gen_java: sast
	ocamlc -c gen_java.ml

java_semantic: scanner parser semantic java_semantic
	ocamlc -o semantic java_semantic parser.cmo scanner.cmo semantic.cmo java_semantic.cmo

java_semantic: jast sast
	ocamlc -c java_semantic.ml

semantic: scanner parser semantic
	ocamlc -o semantic parser.cmo scanner.cmo semantic.cmo


semantic: sast scanner
	ocamlc -c semantic.ml

scanner: parser
	ocamllex scanner.mll; ocamlc -c scanner.ml

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

jast: sast ast
	ocamlc -c jast.mli

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
