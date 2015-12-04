open Ast
open Sast
open Jast
open Semantic

type shape = {
	name: string;
	geo : string;
	size : float;
	color: string;
	rotation: float
}

type layer = {
	name: string;
	radius : float;
	shape : shape;
	count : int;
	offset : float;
	angularshift : float
}

type mandala={
	name: string;
	list_of_layers : layer list;
	max_layer_radius : float; (* define the max layer radius as the maximum of the sum of the the layer radius + shape radius *)
	is_draw: bool
}

type drawing={
	mandala_list : mandala list
}

type java_shapes = {
	shape_list : shape list
}

let createMandala = 




let sast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	Semantic.semantic_check ast

let proc_stmt=function
	Sast.Mandala(var_decl) ->
		(*print java code for mandala of this name*)
		(*create new mandala object of name vname*)
		let (typ, name)= var_decl in
		let 

	| Sast.Expr(expression)->
		proc_expr expression
	| _ -> raise (Error("unsupported statement found")) 

let gen_java (env:translation_enviornment):(Ast.expr -> Sast.sexpr * sdata_type)= function 
	Sast.SProg(s)-> 
		let x = List.length s in
		if (x>0) then (
			
			List.map proc_stmt s;   (*fold left?? *)

		)	

		else print_string "No input provided"

let empty_enviornment=
	{
		drawing={mandala_list=[];}
	}

let _ =	
	let env = empty_enviornment in 
	gen_java env sast



(*let get_shapes shape= List.map extract_attributes shape

let get_layers layer = List.map get_shapes layer  *)
(* pass in mandla *)
(* let mandala = get_layers m *)


(* let convert_stmts = function
    Sast.Mandala(name) -> 
    | Sast.Layer(name, )


let convert_method = function
    Sast.Sprog(s)-> List.map convert_stmts s;

let rec java_semantic_check (check_program: Sast.sprogram): (Jast.javaprogramedo) = 
    let sast_stmts = convert_method check_program in
    if not (sast_stmts )
    Jast.JavaProgramTest(sast_stmts) *)
