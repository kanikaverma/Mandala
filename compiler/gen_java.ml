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
	print_string "public class Program {\n\n";
  print_string "\tpublic static void main(String[] args) {\n\n";
	print_string "\t\tTurtle t = new Turtle();\n";
    print_string "\t\tt.hide();\n";
    print_string "\t\tt.setPosition(0, 0);\n";
    print_string "\t\tt.dot();\n";
    print_string "\t}\n\n}"

let _ =	
	gen_java sast