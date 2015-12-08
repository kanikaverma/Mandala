open Ast
open Sast
open Jast
open Semantic

(*Edo's test shapes *)
let sample_circle = {
	name = "c1";
	geo = "Circle";
	size = 15.0;
	color = "Red";
	rotation = 0.0;
}

let sample_square = {
	name = "s1";
	geo = "Square";
	size = 30.0;
	color = "Red";
	rotation = 0.0;
}

let sample_square2 = {
	name = "s2";
	geo = "Square";
	size = 30.0;
	color = "Red";
	rotation = 45.0;
}

let sample_square3 = {
	name = "s3";
	geo = "Square";
	size = 30.0;
	color = "Red";
	rotation = 60.0;
}

let sample_layer1 = {
	name = "lay1";
	radius = 200.0;
	shape =  sample_circle;
	count = 4;
	offset = 0.0;
	angularshift = 0.0;
}

let sample_layer2 = {
	name = "lay2";
	radius = 100.0;
	shape =  sample_square;
	count = 8;
	offset = 0.0;
	angularshift = 0.0;
}

let sample_layer3 = {
	name = "lay3";
	radius = 100.0;
	shape =  sample_square2;
	count = 8;
	offset = 22.5;
	angularshift = 0.0;
}

let sample_layer4 = {
	name = "lay4";
	radius = 100.0;
	shape =  sample_square3;
	count = 8;
	offset = 0.0;
	angularshift = 0.0;
}

let sample_mandala = {
	name = "m";
	list_of_layers = [sample_layer1; sample_layer2; sample_layer3];
	max_layer_radius = 250.0;
	is_draw = true;
}

(* End of Test Shapes *)
let pi = 3.14159

(* SEPARATE OUT STATEMENTS *)
(* PARSE STATEMENTS- includes func declaration and assignment *)
(* PARSE EXPRESSIONS AS PART OF STATEMENTS - check the types of variable names passed in to expression *)
(* then convert SAST TO JAST *)

type basic_symbol_table = {
	variables: (string * jdata_type) list
}


let sast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	Semantic.semantic_check ast

let find_variable (scope: Jast.drawing) name=
	try
		List.find (fun (s,_) -> s=name) scope.variables 
	with Not_found -> raise (Error ("Didn't find variable in Sast_to_jast! "^name))
let find_mandala (scope: Jast.drawing) mandala_name = 
	try List.find ( fun (str, mndla) -> str = mandala_name) scope.mandala_list
	with Not_found -> raise (Error ("MANDALA WAS NOT FOUND IN MANDALA LIST! "^mandala_name))
let rec get_layer_info(env, actual_args, layer_list: Jast.drawing * Sast.sexpr list * Jast.layer list): (Jast.layer list * Jast.drawing) = match actual_args
	with []-> raise (Error("INVALID, must have atleast one arg!"));
	| [layer_arg] -> let (new_env, ret_typ) = proc_expr env layer_arg in 
		(*Now we ahve checked the arguemnts *)
		(* now we want to look in the variables to see if the layer is defined *)
		let layer_name = match layer_arg 
			with Sast.Id(layer_arg) -> let l_arg = layer_arg in l_arg
			| _ -> raise (Error("This layer is not a string name "));
		in 
		let (my_layer_name, my_layer_typ) = find_variable new_env layer_name in 
		let my_layer_info = match my_layer_typ 
			with Jast.JLayert(my_layer_typ) -> let result_layer = my_layer_typ in result_layer 
			| _ -> raise (Error ("Getting layer info failed!"));
		in 
		(layer_list @[my_layer_info], new_env)
	| layer_arg :: other_layers -> let (new_env, ret_typ) = proc_expr env layer_arg in
		(*Now we ahve checked the arguemnts *)
		(* now we want to look in the variables to see if the layer is defined *)
		let layer_name = match layer_arg 
			with Sast.Id(layer_arg) -> let l_arg = layer_arg in l_arg
			| _ -> raise (Error("This layer is not a string name "));
		in 
		let (my_layer_name, my_layer_typ) = find_variable new_env layer_name in 
		let my_layer_info = match my_layer_typ 
			with Jast.JLayert(my_layer_typ) -> let result_layer = my_layer_typ in result_layer 
			| _ -> raise (Error ("Getting layer info failed!"));
		in 
		get_layer_info (new_env, other_layers, layer_list @ [my_layer_info])

and parse_actual_args (env, args_list:Jast.drawing * Sast.sexpr list): (Jast.drawing) = match args_list
	with  [] -> (env)
	| [check_arg] -> let (new_draw_env, typ) = proc_expr env check_arg in (new_draw_env) 
	| check_arg :: other_args -> let (new_draw_env, typ) = proc_expr env check_arg in
			parse_actual_args (new_draw_env, other_args)
	| _ -> raise (Error("HELLO HIT THIS IN PARSE ")) 
	(* match check_arg 
		with Sast.Id(check_arg) -> let (new_j_expr, new_draw_env, typ) = proc_expr env check_arg in (update_list@[(new_j_expr, typ)], new_draw_env) *)
		(*| Sast.Number (check_arg) -> let (new_j_expr, new_draw_env, typ) = proc_expr env check_arg in (update_list@[(new_j_expr, typ)], new_draw_env)
		| _ -> raise (Error ("Couldn't find the correct arguement match ")); *)
		(*let (new_arg, new_draw_env, typ) = proc_expr env check_arg in
			parse_actual_args (new_draw_env, other_args, update_list@[(new_arg, typ)]) *)
(*with [] -> (update_list, env)
	| *)
and proc_expr (env:Jast.drawing): (Sast.sexpr -> Jast.drawing * Jast.jdata_type) = function
	Sast.Id(vname) ->
		(* Want to go from Sast.Id to Jast.jexpr or Jast.JId, and Jast.drawing *)
		let var_info = try 
			find_variable env vname 
		with Not_found -> 
			raise (Error("undeclared identifier: "^vname))
		in let (name, typ) = var_info in 
		(env, typ)
	| Sast.Literal(literal_var) ->
		raise (Error("Hit literal var"))
	| Sast.Number(number_var) ->
		raise (Error("Hit number var"))
	|Sast.Call(fid, args) ->
		(* run proc_expr on the actual arguments for the function *)
		(* let (new_env, actual_types) = List.map (fun expr -> proc_expr env expr) args in *)

		let new_env  = parse_actual_args (env, args) in 

		(* Now call List.find to get each of the arguments in the list *)

		(* ALL THIS FUNCITON SHOULD DO IS *)
		(* CHECK IF ARGS ARE DEFINED IN VARIABALES *)
		(* IF THEY ARE OF TYPE MANDALA then call list.find on the mandalas, otherwise call list.find on variables!
		or maybe  call list.find on variables anyways *)



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
		(* let args_list = [] in 
			let result_args = parse_actual_args (env, args, args_list) in 
			(* let (expr_and_typ_info, new_env) = result_args in
			let args_info = List.fold_left (fun a (expr,_) -> expr :: a) [] expr_and_typ_info in
			let test_call = Jast.JCall(fid, args_info) in *)
			let func_name = fid in 
			(* Will throw error here if parameter is undefined *)
			let actual_types = List.map (fun expr -> proc_expr env expr) args in *)
		let len = List.length args in
		if (fid ="draw")
		then
			(* check that arg passed in is a valid argument *)
			(* CHECK FOR ARG PASSED IN *)


			(* let actual_expr_list = List.fold_left (fun a (expr,_, ret_env) -> expr :: a) [] actual_types in *)
			(* let len = List.length args in *)
			if (len == 1) 
				then (* only have one mandala that we are drawing *)
					(* Update environment - includes changing bool for current mandala is_draw to true *)
					(* WILL ADD MANDALAS MULTIPLE TIMES TO THE MANDALA LIST! WHEN PARSING IT, WILL BE FINE, WILL GET ALL CORRECT ONES *)
					(* let curr_mandala = find_mandala new_env args in *)
					(* CHANGE EVERYTHING SO THAT IT WILL JUST LOOK AT SAST.ID! *)
					let check_arg = List.hd args in 
					let curr_name = match check_arg
						with Sast.Id(check_arg) -> let new_check_arg = check_arg in new_check_arg (*raise (Error("CHECK ARG IS A STRING!!! "^check_arg));*) (*check_arg*)
						| _ -> raise (Error("This mandala has not been defined"))

					in 
					(* TODO: add this and find_mandala func and change updated_current_manda let mandala_info = find_mandala new_env curr_name in *)
					let drawn_mandalas = List.filter ( fun (m_name, m_typ) -> if (m_typ.is_draw = true) then true else false) env.mandala_list in 
					let false_mandalas = List.filter (fun (man_name, man_typ) -> if (man_typ.is_draw = false) then true else false) env.mandala_list in 
					let other_false_mandalas = List.filter (fun (x, mandala_info) -> if (x = curr_name) then false else true) false_mandalas in  


					let updated_current_mandala = {
						name = curr_name;
						list_of_layers = [];(* mandala_object.list_of_layers; *)
						max_layer_radius = 0.0; (*mandala_object.list_of_layers;*)
						is_draw = true;
					} in
					(* list of all mandalas to draw *)
					let updated_drawn_mandalas =  drawn_mandalas @ [curr_name, updated_current_mandala] in
					let true_and_false_mandalas = updated_drawn_mandalas @ other_false_mandalas in 
					let new_draw_env = {mandala_list = true_and_false_mandalas; variables = new_env.variables; java_shapes_list = new_env.java_shapes_list;} in 
					(* let java_arg_list = [] in 
					let updated_java_arg_list = java_arg_list @ [Jast.JId(curr_name)] in *)
					(* (Jast.JCall(func_name, updated_java_arg_list), new_draw_env, Jast.JVoid) *)
					(new_draw_env, Jast.JVoid)
				(* (Jast.JCall(fid, actual_expr_list), Sast.Void, env) *)
				else raise(Error("Draw function has incorrect parameters"^ string_of_int len))
			else 
				if (fid="addTo")
				then (* Check that length is greater than 1, or at least two args *)
					if (len > 1)
					then 
						(*pull out the first argument, which is a mandala *)
						(* The first argument should be the mandala that you are adding the layer to *)
						let update_mandala = List.hd args in 
						let update_mandala_name = match update_mandala 
							with Sast.Id(update_mandala) -> let m_name = update_mandala in m_name 
							| _ -> raise (Error("This name is not a string! "))
						(* we know that this mandala has been declared, 
						because above when the args are mapped, they are each checked with find_variable *)
						(* return type for proc_expr is Jast.jexpr * Jast.drawing * Jast.jdata_type *)
						(* NEED TO WRITE A FUNCTION get_layers to call expr and check that each ID layer is defined 
							let rec get_layers(env, actual_args, layer_list: Jast.drawing * Jast.jexpr list * Jast.layer list): (Jast.layer list * Jast.drawing) = match actual_args
								with []-> raise (Error("INVALID, must have atleast one arg!"));
								| [layer_arg] -> let (new_expr, new_env, ret_typ) = proc_expr env layer_arg in (layer_list @[new_expr], new_env)
								| layer_arg :: other_layers -> let (new_expr, new_env, ret_typ) = proc_expr env layer_arg in
								get_layers (other_layers, new_env, layer_list@[new_stmt])
							*)
						in 
						let (mandala_name, actual_mandala) = find_mandala new_env update_mandala_name in 
						let curr_layer_list = actual_mandala.list_of_layers in 
						let separate_layers_list = match args 
							with hd :: tail -> get_layer_info (new_env, tail, curr_layer_list) 
							| _ -> raise (Error("This doesn't have a mandala and layers ! "^update_mandala_name))
						in 
						(* separate_layers_list is of type Jast.layer list * Jast.drawing *)
						let (actual_layer_list, layer_updated_env) = separate_layers_list 
						in 
						(* Now get the actual mandala with that name *)
						(* WRITE METHOD FOR FIND MANDALA *)
						(* WRITE FUNCTION TO Check the max layer radius *)
						(* Now copy over the mandala info to a new mandala list, and add the layers to the mandala's layer list *)
						let updated_current_mandala = {
							name = update_mandala_name;
							list_of_layers = actual_layer_list; (*@ actual_layer_list; *)(* mandala_object.list_of_layers; *)
							max_layer_radius = actual_mandala.max_layer_radius; (*mandala_object.list_of_layers;*)
							is_draw = actual_mandala.is_draw;
						} in
						(* JCall of string * jexpr list *)
						(* return type for proc_expr is Jast.jexpr * Jast.drawing * Jast.jdata_type *)
						(* get a list of all mandalas except the one that has just been updated, then add that mandala *)
						let other_unchanged_mandalas = List.filter (fun (x, mandala_info) -> if (x = update_mandala_name) then false else true) env.mandala_list in  
						let updated_drawn_mandalas =  other_unchanged_mandalas @ [update_mandala_name, updated_current_mandala] in
						let new_draw_env = {mandala_list = updated_drawn_mandalas; variables = new_env.variables; java_shapes_list = [];} in 

						(* (Jast.JCall(func_name, actual_expr_list), new_draw_env, Jast.JMandalat(updated_current_mandala)) *)
						(new_draw_env, Jast.JMandalat(updated_current_mandala))
						(* (Jast.JCall(func_name, actual_expr_list), Sast.Mandalat, env)*)
					else 
						raise (Error( "addTo function has incorrect parameters "))
				else
					(* (Jast.JCall(func_name, args_info), env, Jast.JVoid) *)
					(env, Jast.JVoid)

			(* Want to set the bool is_draw to true for the list of mandalas *)
			(* return Jast.jexpr * Jast.drawing * Jast.jdata_type *)

	| _ -> raise (Error("Other call found"))

(* Need to chagne it to return a Jast.jstmt * drawing instead of just a mandala list *)
let proc_stmt (env:Jast.drawing):(Sast.sstmt -> Jast.drawing) =function
	Sast.Mandala(var_decl) ->
		(*print java code for mandala of this name*)
		(*create new mandala object of name vname*)
		let {skind = typ1; svname= name1;}= var_decl in
		(* Create a new mandala *)
		let new_mandala = 
		{
			name= name1; 
			list_of_layers= []; 
			max_layer_radius= 0.0; 
			is_draw= false;
		} in 
		let new_drawing = env.mandala_list @ [(name1, new_mandala)] in
		let new_vars = env.variables @ [(name1, Jast.JMandalat(new_mandala))] in 
		let new_env = {mandala_list=new_drawing; variables = new_vars; java_shapes_list = [];} in
		(* (Jast.JStmt(Jast.JMandala(name1, new_mandala)), new_env) *)
		new_env
	| Sast.Expr(expression)->
		(* Want to add this expression to the mandala list *)
		(* proc_expr returns a jexpr and an updated drawing *)
		let updated_expr = proc_expr env expression in 
		(* Return type of proc_expr is Jast.jexpr * Jast.drawing * Jast.jdata_type *)
		let (new_env, j_typ) = updated_expr in
		(* now want to return new environment and jstmt *)
		(* let updated_java_stmt = Jast.JStmt(j_expr) in *)
		(* let updated_env = Jast.symbol_table(new_env) in *)
		(* (updated_java_stmt, new_env) *)
		new_env

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
let rec separate_statements (stmts, env:Sast.sstmt list * Jast.drawing) = match stmts 
	with [] -> (env)
	| [stmt] -> let (new_env) = proc_stmt env stmt in (new_env)
	| stmt :: other_stmts ->
		let (new_env) = proc_stmt env stmt in
		(* let (nm, tp) = List.hd new_env.var_scope.variables in *)
		separate_statements (other_stmts, new_env)

(* TODO: Can change so gen_java doesn't return javaprogram, can just return javaclass list *)
let gen_java (env:Jast.drawing):(Sast.sprogram -> Jast.drawing)= function 
	Sast.SProg(s,f)-> 
		(* Check if the program has at least one statement *)
		let x = List.length s in
		if (x>0) then (
			(* CHECK ORDER OF STATEMENTS *)
			let update_list = []  in 
			(* Already reversed the statements in semantic when going from ast to jast, so don't need to reverse again *)
			let updated_env = separate_statements (s, env) in  (* List.map( fun stmt_part -> separate_statements prog_stmts env ) in *)
			(* thsi returns a list of Jast.jsstmts list and an enviroment *)
			(* let (statements, updated_env) = resulting_statments in *)
			(* let updated_block = Jast.JavaBlock(statements) in 
			let updated_main = Jast.JavaMain(updated_block) in *)
			(* let updated_class = Jast.JavaClass(updated_main) in *) 
			(* let empty_javaclass_list = [] in 
			let updated_javaclass_list = empty_javaclass_list @ [updated_class] in *)
			(* CREATE SHAPE LIST HERE! *)
			(* let test_shape_list =  updated_env.java_shapes_list in 
			let updated_program = Jast.JavaProgram(updated_class, test_shape_list) in *)
			(* let updated_symbol_table = {draw_stmts = updated_env;} in *)
			updated_env

		)	

		else raise (Error("INPUT IS 0 arguements!"))
	(* | _ ->
		raise (Error("gen_java didn't work! ")) *)




(* PROCESS SHAPES! *)

let rec extract_shapes_from_layer (new_list:Jast.jShape list):(Jast.layer -> Jast.jShape list) = function
	my_layer -> 
		let listed_shape = my_layer.shape in
		let count = my_layer.count in
		if (count >= 1 && listed_shape.geo = "Square")
		then 
			let rec loop = function
			(new_list, k) -> 
			 let rad_offset = my_layer.offset *. pi /. 180.0 in 
			 let my_angle = -1.0 *. (rad_offset +. pi/.2.0 -. (float_of_int k) *. 2.0*.pi /.(float_of_int my_layer.count)) in 
			 let x_pos = cos (my_angle) *. my_layer.radius in
			 let y_pos = sin (my_angle) *. my_layer.radius in
			 let new_shape = Jast.Square(listed_shape.size, x_pos, y_pos, listed_shape.rotation) in 
				 if (k > 0) then 
				 let updated_k = k - 1 in 
				 	loop (new_list@[new_shape], updated_k)
				 else
					new_list@[new_shape]
			in 

			loop(new_list, count)

		else if (count >= 1 && listed_shape.geo = "Circle")
		then 
			let rec loop = function
			(new_list, k) -> 
			 let my_angle = my_layer.offset +. pi/.2.0 -. (float_of_int k) *. 2.0*.pi /.(float_of_int my_layer.count) in 
			 let x_pos = cos (my_angle) *. my_layer.radius in
			 let y_pos = sin (my_angle) *. my_layer.radius in
			 let new_shape = Jast.Circle(listed_shape.size, x_pos, y_pos) in 
				 if (k > 0) then 
				 let updated_k = k - 1 in 
				 	loop (new_list@[new_shape], updated_k)
				 else
					new_list@[new_shape] 
			in 
			loop(new_list, count)
	else raise (Error ("No triangles"))

let get_layers  = function 
	mandala ->
	mandala.list_of_layers 
	(* function
	Jast.mandala -> mandala_shape.list_of_layers *)

let process_mandala = function
	mandala ->
	let list_of_layers = get_layers mandala in
		List.fold_left extract_shapes_from_layer [] list_of_layers

(* create empty initial environment *)
(* the environment keep track of the drawing we are creating *)
let empty_drawing_env=
{
	mandala_list = [];
	variables = [];
	java_shapes_list = [];
}

(* let rec sast_to_jast_final_convert() =	
	(* Initialize an empty drawing *)
	let env = empty_drawing_env in 
	(* once have everything in gen_java now we can make all of it work. *)
	let (a, b, c) = gen_java env sast in 
	a, b, c *)

let rec actual_final_convert (check_program: Sast.sprogram): (Jast.javaprogram) = 
	let env = empty_drawing_env in 
	let new_draw_env = gen_java env sast in 
	let mandala_lists = new_draw_env.mandala_list in 
	(* ADD WAY TO ITERATE THROUGH LIST OF MANDALAS *)
	let (mandala_name, first_mandala) = List.hd mandala_lists in 
	(* let (prog_part_one, prog_part_two) = new_java_prog in*)
	(* REPLACE WITH FIRST MANDLAA INSTEAD OF SAMPLE MANDALA *)
	let mandala_resulting_sample = process_mandala sample_mandala in 
	let prog_name = Jast.CreateClass("Program") in 
    Jast.JavaProgram(prog_name, mandala_resulting_sample)
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
(* EDO's CODE! *)



(* START OF FINAL COMMENT 


let rec extract_shapes_from_layer (new_list:Jast.jShape list):Jast.layer -> Jast.jShape list = function
	my_layer -> 
		let listed_shape = my_layer.shape in
		if (my_layer.count = 1 && listed_shape.geo = "Circle")
		then
			let new_shape = Jast.Circle(listed_shape.size, 0.0, my_layer.radius) in 
			new_list@[new_shape]
		else if (my_layer.count = 1 && listed_shape.geo = "Square")
		then
			let new_shape = Jast.Square(listed_shape.size, 0.0, my_layer.radius, listed_shape.rotation) in 
			new_list@[new_shape]
		else if (my_layer.count = 2 && listed_shape.geo = "Square")
		then let my_angle0 = pi/.2.0 in
			 let my_angle1 = pi/.2.0 -. 1.0 *. 2.0*.pi /.(float_of_int my_layer.count) in 
			 let x_pos0 = cos (my_angle0) *. my_layer.radius in
			 let y_pos0 = sin (my_angle0) *. my_layer.radius in
			 let x_pos1 = cos (my_angle1) *. my_layer.radius in
			 let y_pos1 = sin (my_angle1) *. my_layer.radius in
			 let new_shape_a = Jast.Square(listed_shape.size, x_pos1, y_pos1, listed_shape.rotation) in 
			 let new_shape_b = Jast.Square(listed_shape.size, x_pos0, y_pos0, listed_shape.rotation) in 
			 new_list@[new_shape_a]@[new_shape_b]
		else 
			raise (Error("I can only handle one circle or square per layer right now!"))


(*let empty_list = [shape_with_pos("Circle", 100, "Red", 0, 0, 0); shape_with_pos("Circle", 100, "Blue", 0, 0, 0)] in*)

let get_layers = function
	mandala -> mandala.list_of_layers

let process_mandala = function
	mandala -> 
	let list_of_layers = get_layers mandala in
	List.fold_left extract_shapes_from_layer [] list_of_layers





(*let get_shapes shape= List.map extract_attributes shape
let get_layers layer = List.map get_shapes layer  *)
(* pass in mandla *)
(* let mandala = get_layers m *)


(* let convert_stmts = function
    Sast.Mandala(name) -> 
    | Sast.Layer(name, )
let convert_method = function
    Sast.Sprog(s)-> List.map convert_stmts s;
*)

let convert_shape = function
	my_shape ->
		if (my_shape.geo = "Circle") then
			Jast.Circle(my_shape.size, 0.0, 0.0)
		else if (my_shape.geo = "Square") then
			Jast.Square(my_shape.size, 0.0, 0.0, my_shape.rotation)
		else if (my_shape.geo = "Triangle") then
			Jast.Triangle(my_shape.size, 0.0, 0.0, my_shape.rotation)
		else
			raise (Error("undefined shape "))


(*let rec shapes_to_jast (s: java_shapes) : (Jast.javaprogram) =
	let new_shapes_list = List.map convert_shape s.shape_list in
	Jast.JavaProgram(Jast.CreateClass("Program"), new_shapes_list)*)


(* let rec final_convert (check_program: Sast.sprogram): (Jast.javaprogram) = 
    Jast.JavaProgram(Jast.CreateClass("Program"), [Jast.Circle(100.0,0.0,0.0); Jast.Square(80.0,0.0,40.0,45.0); Jast.Square(80.0,0.0,-40.0,45.0); Jast.Square(80.0,0.0,120.0,45.0)]) *)

(* Starting function *)
let rec actual_final_convert (check_program: Sast.sprogram): (Jast.javaprogram) = 
    Jast.JavaProgram(Jast.CreateClass("Program"), process_mandala sample_mandala)

END OF FINAL COMMENT *)
