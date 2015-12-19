default: run semantic sast_to_jast

run: scanner parser semantic sast_to_jast gen_java
	ocamlc -o run scanner.cmo parser.cmo semantic.cmo sast_to_jast.cmo gen_java.cmo

gen_java: sast
	ocamlc -c gen_java.ml

sast_to_jast_o: scanner parser semantic sast_to_jast
	ocamlc -o semantic sast_to_jast parser.cmo scanner.cmo semantic.cmo sast_to_jast.cmo

sast_to_jast: jast sast
	ocamlc -c sast_to_jast.ml

semantic_o: scanner parser semantic
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
