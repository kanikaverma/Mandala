open Ast
open Sast
open Jast
open Semantic

type shape = {
	geo : string;
	size : float;
	color: string;
	rotation: float
}

type layer = {
	radius : float;
	shape : shape;
	count : int;
	offset : float;
	angularshift : float
}

type mandala={
	list_of_layers : layer list;
	max_layer_radius : float; (* define the max layer radius as the maximum of the sum of the the layer radius + shape radius *)
	is_draw: bool
}

type java_shapes = {
	shape_list : shape list
}





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
