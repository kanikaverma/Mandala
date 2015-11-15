(* Code generation*)
open Ast
open Sast
open Semantic
open Lexing

let sast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	Semantic.semantic_check ast

let gen_java s =
	print_string "public class Program {\n\n\t";
	print_string "Turtle t = new Turtle();\n";
    print_string "t.hide();\n";
    print_string "t.setPosition(0, 0);\n";
    print_string "t.dot();\n";
    print_string "}}/n"

let _ =	
	gen_java sast