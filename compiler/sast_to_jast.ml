open Ast
open Sast
open Jast
open Semantic


(* SEPARATE OUT STATEMENTS *)
(* PARSE STATEMENTS- includes func declaration and assignment *)
(* PARSE EXPRESSIONS AS PART OF STATEMENTS - check the types of variable names passed in to expression *)
(* then convert SAST TO JAST *)



(* type shape = {
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
	max_layer_radius : float; *) (* define the max layer radius as the maximum of the sum of the the layer radius + shape radius *)
	(* is_draw: bool
}

type drawing={
	mandala_list : mandala list
}

type java_shapes = {
	shape_list : shape list
}

type symbol_table = {
	draw_stmts : drawing	
} *)
	(* let var_empty_table_init = {parent=None; variables=[];} *)

(* type java_drawing= 
{
	java_mandala_list: mandala list 
}
 type java_symbol_table = {
	java_draw_stmts: java_drawing
} *)
(* let createMandala = *)
(* list to hold all variable names and their types *)
type basic_symbol_table = {
	variables: (string * jdata_type) list
}


let sast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	Semantic.semantic_check ast

(* let rec parse_actual_args (env, args_list, update_list:Jast.drawing * Sast.sexpr list * Jast.jexpr list) = match args_list
	with [] -> (update_list, env)
	| [arg] -> let (new_j_expr, new_draw_env) = proc_expr env arg in (update_list@[new_j_expr], new_draw_env)
	| arg :: other_args -> 
		let (new_arg, typ, new_draw_env) = proc_expr env arg in *)
			(* let (nm, tp) = List.hd new_env.var_scope.variables in *)
			(* parse_actual_args (other_args, new_draw_env, update_list@[new_arg]) *)

(* let (new_stmt, typ, new_env) = proc_stmt env stmt in (update_list@[new_stmt], new_env) *)
(*let get_actual_arg_type env = function
	(sdata_type, string) -> (string)*)

let find_variable (scope: symbol_table) name=
	try
		List.find (fun (s,_) -> s=name) scope.variables 
	(* with Not_found -> try List.find (fun (s, _) -> s=name) scope.parent.variables*) 
	with Not_found -> raise (Error ("THIS FAILED AGAIN!!! "^name))


let rec find_mandala (scope: Jast.drawing) name = 
	try 
		List.find ( fun (mandala_name, _) -> mandala_name = name) scope.mandala_list 
	with Not_found -> raise (Error ("MANDALA WAS NOT FOUND!! "))

let rec parse_actual_args (env, args_list, update_list:Jast.drawing * Sast.sexpr list * Jast.jexpr list):(Jast.jexpr list * Jast.drawing) = match args_list
	with [] -> (update_list, env)
	| [arg] -> let (new_j_expr, new_draw_env) = proc_expr env arg in (update_list@[new_j_expr], new_draw_env)
	| arg :: other_args -> 
		let (new_arg, new_draw_env) = proc_expr env arg in
			(* let (nm, tp) = List.hd new_env.var_scope.variables in *)
			parse_actual_args (new_draw_env, other_args, update_list@[new_arg])

and proc_expr (env:Jast.drawing): (Sast.sexpr -> Jast.jexpr * Jast.drawing * Jast.jdata_type) = function
	Sast.Id(vname) ->
		(* Want to go from Sast.Id to Jast.jexpr or Jast.JId, and Jast.drawing *)
		(* raise (Error("Id expression hit")) *)
		let id_name = vname in 
		let j_id = Jast.JId(id_name) in 
		(j_id, env, )
	| Sast.Literal(number_var) ->
		raise (Error("Hit number var"))

	|Sast.Call(fid, args) ->
		(* raise (Error("Call function expression hit"))*)
		(* JavaFunction of javaFuncCall * javaBlock *)
		(* turn it into a Jast.JFuncCall JFuncCall of jdata_type * string *)
		(* go to JavaBlock JavaBlock of jStmt list *)
		(* jStmt JStmt of jexpr *)
		(* jexpr 
			jexpr =
				JLiteral of int
				| JFloat_Literal of float
				| JId of string
				| JBinop of jexpr * op * jexpr
				| JCall of string * jexpr list
		*)
		(* want to be able to call draw: (m); *)

		(* turn fid, the function name into something of type FuncCall, with the funs return type *)
		(* let actual_args = List.map (fun arg -> proc_expr env arg) args in *)
		
		if (fid ="draw")
		then
			let args_list = [] in 
			let result_args = parse_actual_args (env, args, args_list) in 
			let (args_info, new_env) = result_args in 
			let func_name = fid in 
			(* want to check if the variable you are calling draw on is m, and updated it's boolean is_draw to true *)
			(* find the actual mandala passed in by args in the environment *)
			let curr_mandala_name = List.hd args_info in 
			(* let test_string = proc_expr new_env curr_mandala_name in *)
			
			(* now find the mandala with that name in the environment *)
			(* let mandala_object = List.find ( fun (mandala_name, _) -> mandala_name = curr_mandala_name) env.mandala_list in  *)
			let (test_string, newer_env) = proc_expr new_env (List.hd args) in 
			let j_string = Jast.JId(test_string) in 
			let mandala_object = find_mandala env j_string in 
			(* do list.fold_left to add all mandalas that have is_draw =true to the new environment *)
			(* let drawn_mandalas = List.fold_left ( fun a (m_name, m_typ) -> if (m_typ.is_draw = true) then (m_name, m_typ)::a else a) env.mandala_list in 
			let false_mandalas = List.fold_left ( fun b (m_name, m_typ) -> if (m_typ.is_draw = false) then (m_name, m_typ) :: b else b) env.mandala_list in 
			*)
			let drawn_mandalas = List.filter ( fun (m_name, m_typ) -> if (m_typ.is_draw = true) then true else false) env.mandala_list in 
			let false_mandalas = List.filter (fun (man_name, man_typ) -> if (man_typ.is_draw = false) then true else false) env.mandala_list in 
			let first_element = List.hd false_mandalas in 
			let (t_one, t_two) = first_element in
			(* raise (Error("IS THE VARIABLE NAME VIEWED AS A STRING?!??! "^t_one)); *)
			(* all false mandalas except the current mandala *)
			let other_false_mandalas = List.filter (fun (x, other_typ) -> if (x = curr_mandala_name) then false else true) false_mandalas in  

			(*then add the current mandala to this list, setting its is_draw = true *)
			let updated_current_mandala = {
				name = curr_mandala_name;
				list_of_layers = [];(* mandala_object.list_of_layers; *)
				max_layer_radius = 0.0; (*mandala_object.list_of_layers;*)
				is_draw = true;
			} in
			(* list of all mandalas to draw *)
			let updated_drawn_mandalas =  drawn_mandalas @ [curr_mandala_name, updated_current_mandala] in
			let true_and_false_mandalas = updated_drawn_mandalas @ other_false_mandalas in 
			let new_draw_env = {mandala_list = true_and_false_mandalas;} in 
			let java_arg_list = [] in 
			let updated_java_arg_list = java_arg_list @ [Jast.JId(curr_mandala_name)] in 
			(Jast.JCall(func_name, updated_java_arg_list), new_draw_env)
		
		else
		(* now have all updated mandalas *)
			let alt_args_list = [] in 
			let result_actual_args = parse_actual_args (env, args, alt_args_list) in 
			let (result_args_info, result_new_env) = result_actual_args in 
			let func_name = fid in 
			(Jast.JCall(func_name, result_actual_args), result_new_env)

		(* NEED TO KEEP TRACK OF ALL MANDALAS THAT ARE NOT BEIGN DRAWN ALSO! *)


		 (* type mandala={
	name: string;
	list_of_layers : layer list;
	max_layer_radius : float;
	is_draw: bool
} *)
		(* NEED TO CHECK FOR DRAW FUNCTION! if it's draw, set the boolean for the mandala with that name to true *)
		(* 
if (fid = "draw")
			then let actual_expr_list = List.fold_left (fun a (expr,_, ret_env) -> expr :: a) [] actual_types in
			let len = List.length actual_expr_list in
			if (len == 1) 
			then (Sast.Call(fid, actual_expr_list), Sast.Void, env)
			else raise(Error("Draw function has incorrect parameters"^ string_of_int actual_len)) 	

		*)
		(* NEED TO CHECK IF FUNCTION IS ADD TO also! *)
		(* turn the list of mandalas into an array of mandalas *)
		(*try 
			let found_mandala = List.find (fun (s,_) -> s=name) new_env
		if (fid = "draw")
			then let  *)
		



	| _ -> raise (Error("Other call found")) 

(* and parse_actual_args (env, args_list, update_list:Jast.drawing * Sast.sexpr list * Jast.jexpr list-> Jast.jexpr list * Jast.drawing) = match args_list
	with [] -> (update_list, env)
	| [arg] -> let (new_j_expr, new_draw_env) = proc_expr env arg in (update_list@[new_j_expr], new_draw_env)
	| arg :: other_args -> 
		let (new_arg, new_draw_env) = proc_expr env arg in
			(* let (nm, tp) = List.hd new_env.var_scope.variables in *)
			parse_actual_args (new_draw_env, other_args, update_list@[new_arg]) *)


(* Need to chagne it to return a Jast.jstmt * drawing instead of just a mandala list *)
let proc_stmt (env:Jast.drawing):(Sast.sstmt -> Jast.jStmt * Jast.drawing) =function
	Sast.Mandala(var_decl) ->
		(*print java code for mandala of this name*)
		(*create new mandala object of name vname*)
		let {skind = typ1; svname= name1;}= var_decl in
		(* WANT TO CREATE A MANDALA 
		type mandala={
			name: string;
			list_of_layers : layer list;
			max_layer_radius : float; (* define the max layer radius as the maximum of the sum of the the layer radius + shape radius *)
			is_draw: bool
		} *)

		let new_mandala = 
		{
			name= name1; 
			list_of_layers= []; 
			max_layer_radius= 0.0; 
			is_draw= false;
		} in 
		let new_drawing = env.mandala_list @ [(name1, new_mandala)] in
		let new_env = {mandala_list=new_drawing;} in
		(* let updated_j_stmt = Jast.JStmt(Jast.Id())
		Jast.JavaMain(Jast.JavaBlock()) *)
		(Jast.JStmt(Jast.JMandala(name1, new_mandala)), new_env)





(* let new_sym_table = {parent = env.var_scope.parent; 
		variables = new_vars;} in *)
	(*let test_name = "heeyy" in
	 let tester = try 
		raise (Error("FAIL "^name))
	with Not_found ->
		raise (Error("VAR NEVER ADDED "^name)) 
	in *)
	(* let new_env = { env with var_scope = new_sym_table} in
	new_env
drawing={mandala_list=[];} *)
	
	| Sast.Expr(expression)->
		(* Want to add this expression to the mandala list *)
		(* proc_expr returns a jexpr and an updated drawing *)
		let updated_expr = proc_expr env expression in 
		let (j_expr, new_env) = updated_expr in
		(* now want to return new environment and jstmt *)
		let updated_java_stmt = Jast.JStmt(j_expr) in 
		(* let updated_env = Jast.symbol_table(new_env) in *)
		(updated_java_stmt, new_env)

		(* type mandala={
	name: string;
	list_of_layers : layer list;
	max_layer_radius : float; (* define the max layer radius as the maximum of the sum of the the layer radius + shape radius *)
	is_draw: bool 
} *)
		(* now add the updated expression to the mandala list *)
		(* let updated_mandala = {}
		env.mandala_list @ [] *)
		(* CHANGE FOLLOWIGN LINE TO ACTUALLY FIND THE CORRECT MANDALA TO UPDATE!! *)
		(* return Jast.jStmt * Jast.drawing *)
		(* Want to consider the expression draw: (m) *)
		(* let recent_mandala = List.hd env.mandala_list in 
		let bool_is_draw = recent_mandala.is_draw in 
		let updated_mandala_list = env.mandala_list@[recent_mandala] in 

		let new_mandala = 
		{
			name= name1; 
			list_of_layers= []; 
			max_layer_radius= 0.0; 
			is_draw= true;
		} in
		let new_drawing = env.mandala_list @ [new_mandala] in

		let new_env = {mandala_list=new_drawing;} in
		(new_drawing, new_env) *)


	| _ -> raise (Error("unsupported statement found")) 

(* check out each statement *)
(* returns Jast.jStmt list * env *)
let rec separate_statements (stmts, env, update_list:Sast.sstmt list * Jast.drawing * Jast.jStmt list -> Jast.jStmt list * Jast.drawing) = match stmts 
	with [] -> ((update_list@[Jast.JStmt(Jast.JId("hello"))]), env)
	| [stmt] -> let (new_stmt, new_env) = proc_stmt env stmt in (update_list@[new_stmt], new_env)
	| stmt :: other_stmts ->
		let (new_stmt, new_env) = proc_stmt env stmt in
		(* let (nm, tp) = List.hd new_env.var_scope.variables in *)
		separate_statements (other_stmts, new_env, update_list@[new_stmt])

let gen_java (env:Jast.drawing):(Sast.sprogram -> Jast.javaprogram * Jast.drawing * Jast.jStmt list)= function 
	Sast.SProg(s)-> 
		(* Check if the program has at least one statement *)
		let x = List.length s in
		if (x>0) then (
			(* CHECK ORDER OF STATEMENTS *)
			let update_list = []  in 
			(* Already reversed the statements in semantic when going from ast to jast, so don't need to reverse again *)
			let resulting_statments = separate_statements (s, env, update_list) in  (* List.map( fun stmt_part -> separate_statements prog_stmts env ) in *)
			(* thsi returns a list of Jast.jsstmts list and an enviroment *)
			let (statements, updated_env) = resulting_statments in 
			let updated_block = Jast.JavaBlock(statements) in 
			let updated_main = Jast.JavaMain(updated_block) in 
			let updated_class = Jast.JavaClass(updated_main) in 
			let empty_javaclass_list = [] in 
			let updated_javaclass_list = empty_javaclass_list @ [updated_class] in 
			let updated_program = Jast.JavaProgram(updated_javaclass_list) in 
			(* let updated_symbol_table = {draw_stmts = updated_env;} in *)
			updated_program, updated_env, statements



		(*in *)

		(* JavaProgram of javaClass list
			JavaClass of string * string * javaMethod
			JavaMain of javaBlock
			| JavaDefaultConstructor of string * javaBlock
			| JavaFunction of javaFuncCall * javaBlock

			JavaBlock of jStmt list

			JFuncCall of jdata_type * string

			JStmt of jexpr

			 jexpr =
				JLiteral of int
				| JFloat_Literal of float
				| JId of string
				| JBinop of jexpr * op * jexpr
				| JCall of string * jexpr list
		*)


			(* List.map (proc_stmt env) s; *)  (*fold left?? *)

		)	

		else raise (Error("INPUT IS 0 arguements!"))
	(* | _ ->
		raise (Error("gen_java didn't work! ")) *)

(* create empty initial environment *)
(* the environment keep track of the drawing we are creating *)
(* Sast.Mandala({skind = typ; svname = name}) *)
(*  {{ 
    kind = $1;   
    vname = $4;   
  }} *)

(*let empty_mandala_list = {
	mandala_list = [];
}*)
let empty_drawing_env=
{
	mandala_list = [];
}
(* {
	drawing= {mandala_list=[];}
} *)
(* Go from Sast to Jast *)
let _ =	
	(* Initialize an empty drawing *)
	let env = empty_drawing_env in 
	(* once have everything in gen_java now we can make all of it work. *)
	let (a, b, c) = gen_java env sast in 
	a, b, c


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
