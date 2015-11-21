(* Code generation*)
open Ast
open Sast
open Semantic
open Lexing

exception Error of string

let sast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	Semantic.semantic_check ast

let proc_expr =function
	Sast.Call(fname, actual_args)->
		
		print_string "    t.setPosition(0, 0);\n";      (*should be in sast.mandala case*)
		print_string "    t.dot();\n"      (*should be in sast.mandala case*)


let proc_stmt = function
	Sast.Mandala(vname) ->
		(*print java code for mandala of this name*)
		print_string "    Turtle t = new Turtle();\n";    (*should be in sast.mandala case*)
		print_string "    t.hide();\n";     (*should be in sast.mandala case*)
	| Sast.Expr(expression)->
		proc_expr expression
	| _ -> raise (Error("unsupported statement found")) 

let gen_java = function
	Sast.SProg(s)-> 
		let x = List.length s in
		if (x>0) then (
			print_string "public class Program {\n\n";
 			print_string "  public static void main(String[] args) {\n\n";
		

			List.map proc_stmt s; 

			print_string "	t.save(\"Program.jpg\");";
			print_string "  }\n\n}";

		)	

		else print_string "No input provided"

let _ =	
	gen_java sast