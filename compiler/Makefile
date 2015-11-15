default: run_bitch semantic

run_bitch: scanner parser semantic gen_java
	ocamlc -o run_bitch scanner.cmo parser.cmo semantic.cmo gen_java.cmo

#sast_jast: scanner parser semantic sast_to_jast
	#ocamlc -o sast_to_jast scanner.cmo parser.cmo semantic_checker.cmo sast_to_jast.cmo 	

#pretty: scanner parser pretty_printer
	#ocamlc -o pretty parser.cmo scanner.cmo pretty_printer.cmo

gen_java: sast
	ocamlc -c gen_java.ml

semantic: scanner parser semantic
	ocamlc -o semantic parser.cmo scanner.cmo semantic.cmo

#sast_to_jast: jast
	#ocamlc -c sast_to_jast.ml 

semantic: sast scanner
	ocamlc -c semantic.ml

scanner: parser
	ocamllex scanner.mll; ocamlc -c scanner.ml

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

#jast: sast ast
#	ocamlc -c jast.mli

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