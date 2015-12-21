open Ast
open Sast
open Jast
open Semantic

let pi = 3.14159


type environment = {
	drawing: Jast.drawing;
	functions: Sast.sfuncdecl list;
}

let sast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	Semantic.semantic_check ast

let find_function (scope: environment) fid = 
	try
		List.find (fun s -> s.sfname = fid) scope.functions
	with Not_found -> raise (Error ("Didn't find function in Sast_to_jast! "^fid))

let find_variable (scope: environment) name=
	try
		List.find (fun (s,_) -> s=name) (List.rev scope.drawing.variables)
	with Not_found -> raise (Error ("Didn't find variable in Sast_to_jast! "^name))
let find_variable_check_return_type (scope, return_typ: environment * smndlt) name=
	try
		List.find (fun (s,_) -> s=name) scope.drawing.variables 
	with Not_found -> 
	if (not(return_typ = Sast.Voidt)) then 
		raise (Error ("Didn't find return statement for non-void function"))
	else
		("", Jast.JVoid)
let find_mandala (scope: environment) mandala_name = 
	try List.find ( fun (str, mandala) -> str = mandala_name) scope.drawing.mandala_list
	with Not_found -> raise (Error ("MANDALA WAS NOT FOUND IN MANDALA LIST! "^mandala_name))

let rec proc_bin_expr (scope: environment):(Sast.sexpr -> Sast.sexpr)  = function
	Sast.Float_Literal(term1) -> Sast.Float_Literal(term1)
	| Sast.Id(var) -> 
		let (n,v) = find_variable scope var in
		let Jast.JNumbert(my_float) = v in
		Sast.Float_Literal(my_float)
	| Sast.Binop(t1, op, t2) ->

		let eval_term1 = proc_bin_expr scope t1 in 
		let eval_term2 = proc_bin_expr scope t2 in 
		let Sast.Float_Literal(float_term_one) = eval_term1 in 
		let Sast.Float_Literal(float_term_two) = eval_term2 in 

		let result = match op 
			with Add -> float_term_one +. float_term_two 
			| Sub -> float_term_one -. float_term_two 
			| Mult -> float_term_one *. float_term_two 
			| Div -> float_term_one /. float_term_two

		in Sast.Float_Literal(result)



let rec get_layer_info(env, actual_args, layer_list: environment * Sast.sexpr list * Jast.layer list): (Jast.layer list * environment) = match actual_args
	with []-> raise (Error("INVALID, must have atleast one arg!"));
	| [layer_arg] -> let (new_env, ret_typ) = proc_expr env layer_arg in 
		(*Now we ahve checked the arguemnts *)
		(* now we want to look in the variables to see if the layer is defined *)
		let layer_name = match layer_arg 
			with Sast.Id(l) -> l
			| _ -> raise (Error("This layer is not a string name "));
		in 
		let (my_layer_name, my_layer_typ) = find_variable new_env layer_name in 
		(*let get_that_out_of_there = List.filter ( fun (l_name, l_typ) -> if (l_name=my_layer_name) then false else true) new_env.drawing.variables in 
		let new_drawing = {new_env.drawing with variables = get_that_out_of_there} in
		let new_new_env = {new_env with drawing = new_drawing} in*)
		let my_layer_info = match my_layer_typ 
			with Jast.JLayert(m) -> m
			| _ -> raise (Error ("Getting layer info failed!"));
		in 
		(layer_list @[my_layer_info], new_env)
	| layer_arg :: other_layers -> let (new_env, ret_typ) = proc_expr env layer_arg in
		(*Now we ahve checked the arguemnts *)
		(* now we want to look in the variables to see if the layer is defined *)
		let layer_name = match layer_arg 
			with Sast.Id(l) -> l
			| _ -> raise (Error("This layer is not a string name "));
		in 
		let (my_layer_name, my_layer_typ) = find_variable new_env layer_name in 
		(*let get_that_out_of_there = List.filter ( fun (l_name, l_typ) -> if (l_name=my_layer_name) then false else true) new_env.drawing.variables in 
		let new_drawing = {new_env.drawing with variables = get_that_out_of_there} in
		let new_new_env = {new_env with drawing = new_drawing} in *)
		let my_layer_info = match my_layer_typ 
			with Jast.JLayert(m) -> m
			| _ -> raise (Error ("Getting layer info failed!"));
		in 
		get_layer_info (new_env, other_layers, layer_list @ [my_layer_info])

and match_formals (formals, params, env: Sast.svar_decl list * Sast.sexpr list * environment)  = match formals
with [] -> env
| [formal] -> let namer = formal.svname in
			let result = 
		   match params 
		    with [] -> env
			|[param] ->
			   let (_, my_val) = proc_expr env param in
			   let new_variables = env.drawing.variables@[(namer, my_val)] in
			   let drawing = env.drawing in
			   let new_drawing = {drawing with variables = new_variables} in
			   let new_env = {env with drawing = new_drawing} in
			   new_env in
			   result
| formal :: other_formals -> let namee = formal.svname in
		   match params 
			with [] -> env
			| (param :: other_params) ->
			   let (_, my_val) = proc_expr env param in
			   let new_variables = env.drawing.variables@[namee, my_val] in
			   let drawing = env.drawing in
			   let new_drawing = {drawing with variables = new_variables} in
			   let new_env = {env with drawing = new_drawing} in
			   match_formals (other_formals, other_params, new_env) 
			


and process_arguments  (params, l: Sast.sexpr list * string list) = match params
			with [] -> l
			| [param] -> let result = match param with Sast.Float_Literal(term1) -> l
							|Sast.Id(var) -> l @ [var] in result
			| param :: other_params -> let result = match param with Sast.Float_Literal(term1) -> l
							|Sast.Id(var) -> l @ [var] in 
							process_arguments (other_params, result)

and proc_expr (env:environment): (Sast.sexpr -> environment * Jast.jdata_type) = function
	Sast.Id(vname) ->
		(* Want to go from Sast.Id to Jast.jexpr or Jast.JId, and Jast.drawing *)
		let var_info = try 
			find_variable env vname 
		with Not_found -> 
			raise (Error("undeclared identifier: "^vname))
		in let (name, value) = var_info in 
		(env, value)

	| Sast.Literal(literal_var) ->	
		(env, Jast.JInt(literal_var))
	| Sast.Float_Literal(number_var) ->
		(env, Jast.JNumbert(number_var))
	| Sast.Binop(term1, operator, term2) ->


		(*Recursively calls a binary operator*)
		let eval_term1 = proc_bin_expr env term1 in 
		let eval_term2 = proc_bin_expr env term2 in 

		(*Can be a variable or a float literal*)
		let float_term_one = match eval_term1
			with Sast.Float_Literal(term1) -> term1
			| Sast.Id(var) -> 
				let (n,v) = find_variable env var in
				let Jast.JNumbert(my_float) = v in
				my_float
			| _ -> raise(Error("Operand one is not a float literal, invalid operand "))
		in

		let float_term_two = match eval_term2
			with Sast.Float_Literal(term2) -> term2
			| Sast.Id(var) -> 
				let (n,v) = find_variable env var in
				let Jast.JNumbert(my_float) = v in
				my_float
			| _ -> raise(Error("Operand two is not a float literal, invalid operand "))
		in
		
		(*Calls supported binary operator*)
		let result = match operator 
				with Add -> float_term_one +. float_term_two 
				| Sub -> float_term_one -. float_term_two 
				| Mult -> float_term_one *. float_term_two 
				| Div -> float_term_one /. float_term_two

in (env, Jast.JNumbert(result)) 

	(*Process function calls*)
	|Sast.Call(fid, args) ->

		let old_variables = env.drawing.variables in

		if not (  List.length args > 0 ) then ( 
			(*Make sure that func_decl has no formal arguments*)
			let my_func_decl = find_function env fid in
			let my_body = my_func_decl.sbody in
			let env_with_return = separate_statements_s(my_body, env) in
			let return_name = "return" in 

			let var = find_variable_check_return_type (env_with_return, my_func_decl.sreturntype) return_name in

			let (n, v) = var in
			let new_env = {
				drawing = {mandala_list = env_with_return.drawing.mandala_list; variables = old_variables; java_shapes_list = env_with_return.drawing.java_shapes_list};
				functions =  env_with_return.functions;
			} in
			(new_env, v)

		)
		 else 

		(*Add all variables only to this function's scope -- everything is the same except for variables*)
		(*At end, empty out variables, store them, put in the arg variables, later add back at end (but remove arg variables)*)
		let all_param_names = process_arguments (args, []) in
		let only_param_variables = List.filter ( fun (n, v) -> if ( List.mem n all_param_names ) then true else false) env.drawing.variables in  

		let env_with_param_vars = {
			drawing = {mandala_list = env.drawing.mandala_list; variables = only_param_variables; java_shapes_list = env.drawing.java_shapes_list};
			functions =  env.functions;
		} in
 
		(*Grab the function from its table*)
		if ( not(fid = "draw") && not (fid = "addTo")) then (
			let my_func_decl = find_function env_with_param_vars fid in
			let my_formals = my_func_decl.sformals in
			let new_env = match_formals(my_formals, args, env_with_param_vars) in
			let func_stmts = my_func_decl.sbody in 
			let l = List.length func_stmts in
			(*Process statements with limited scope*)
			let env_with_return = separate_statements_s(func_stmts, new_env) in 
			let return_name = "return" in 

			(*Get return value (will check if return type is void if applicable)*)
			let var = find_variable_check_return_type (env_with_return, my_func_decl.sreturntype) return_name in

			let (n, v) = var in
			let new_env = {
				drawing = {mandala_list = env_with_return.drawing.mandala_list; variables = old_variables; java_shapes_list = env_with_return.drawing.java_shapes_list};
				functions =  env_with_return.functions;
			} in
			(new_env, v) )

	else 
		let len = List.length args in
		if (fid ="draw")
		then
			if (len == 1) 
				then (*Drawing one mandala*)
					let check_arg = List.hd args in 
					let curr_name = match check_arg
						with Sast.Id(check_arg) -> let new_check_arg = check_arg in new_check_arg
						| _ -> raise (Error("This mandala has not been defined"))
					in 

					(*Find mandala from mandala_list*)
					let (mandala_name, actual_mandala) = find_mandala env curr_name in

					let updated_current_mandala = {
						name = curr_name;
						list_of_layers = actual_mandala.list_of_layers;
						max_layer_radius = actual_mandala.max_layer_radius;
						is_draw = true;
					} in

					(*Remove current mandala from variable list*)
					let filtered_vars = List.filter (fun (var_name, var_typ) -> if (var_name=curr_name) then false else true) env.drawing.variables in

					(*Remove current mandala from mandala list*)
					let filtered_mandalas = List.filter (fun (var_name, var_typ) -> if (var_name=curr_name) then false else true) env.drawing.mandala_list in

					(*Reintroduce mandala with updated values and return environment*)
					let mandalas_to_be_drawn = filtered_mandalas@[(curr_name, updated_current_mandala)] in 
					let updated_vars = filtered_vars @ [(curr_name, Jast.JMandalat(updated_current_mandala))] in 
					let new_draw_env = {mandala_list = mandalas_to_be_drawn; variables = updated_vars; java_shapes_list = env.drawing.java_shapes_list;} in 
					let new_env = {drawing = new_draw_env; functions = env.functions;} in

					(new_env, Jast.JVoid)

				else raise(Error("Draw function has incorrect parameters"^ string_of_int len))

			else 
				if (fid="addTo")
				then 
				(* Check that length is greater than 1 -- args must contain a mandala and at least one layer*)
					if (len > 1)
					then 
						(*Pull out the first argument, which should be the mandala that a layer(s) is being added to *)
						let rev_args = List.rev args in
						let update_mandala = List.hd rev_args in 
						let update_mandala_name = match update_mandala 
							with Sast.Id(update_mandala) -> update_mandala
							| _ -> raise (Error("This name is not a string! "))

						in 

						let (mandala_name, untyped_mandala) =  List.find (fun (s,_) -> s=update_mandala_name) env.drawing.variables in

						let actual_mandala = match untyped_mandala
						with Jast.JMandalat(untyped_mandala) -> untyped_mandala
						| _ -> raise(Error("The variable returned is invalid because it is not of type mandala. "))
						in
						let old_layer_list = actual_mandala.list_of_layers in 

						(*Get layers by looking up all arguments and checking whether they've been defined*)
						let new_layers_list = match rev_args 
							with hd :: tail -> get_layer_info (env, tail, old_layer_list) 
							| _ -> raise (Error("This doesn't have a mandala and layers ! "^update_mandala_name))
						in 
						let (actual_layer_list, layer_updated_env) = new_layers_list in

						let l = List.length old_layer_list in

						let updated_layer_list = actual_layer_list in

						let rec find_max l = match l with
							| [] -> 0.0
							| h :: t -> max h (find_max t) in

						let get_max_layer_radius = function
							updated_layer_list -> 
							let layer_radius_list = List.fold_left (fun a layer -> layer.radius :: a) [] updated_layer_list in
							find_max layer_radius_list in 

						let updated_current_mandala = {
							name = update_mandala_name;
							list_of_layers = updated_layer_list;
							max_layer_radius = get_max_layer_radius updated_layer_list;
							is_draw = false;
						} in
						
						let env = layer_updated_env in
						(* Leave in all mandalas except the current mandala (pull this one out) *)
						let unchanged_variables = List.filter ( fun (m_name, m_typ) -> if (m_name=update_mandala_name) then false else true) env.drawing.variables in 

						(* Then add back in the updated mandala to the list of all variables *)
						let updated_variables = unchanged_variables@[(update_mandala_name, Jast.JMandalat(updated_current_mandala))] in 

						(*Take out this mandala and add it back in with updated stuff*)
						let unchanged_mandalas = List.filter ( fun (m_name, m_typ) -> if (m_name=update_mandala_name) then false else true) env.drawing.mandala_list in 
						let updated_mandala_list = unchanged_mandalas@[update_mandala_name, updated_current_mandala] in

						let new_draw_env = {mandala_list = updated_mandala_list; variables = updated_variables; java_shapes_list = env.drawing.java_shapes_list;} in
						let new_env = {drawing = new_draw_env; functions = env.functions} in

						(new_env, Jast.JMandalat(updated_current_mandala))
					else 
						raise (Error( "addTo function has incorrect parameters "))
				else
					(env, Jast.JVoid)


	| _ -> raise(Error("Other call found"))

and separate_statements_s (stmts, env:Sast.sstmt list * environment) = match stmts 
	with [] -> env
	| [stmt] -> proc_stmt env stmt (*let new_env = proc_stmt env stmt in new_env*)
	| stmt :: other_stmts ->
		let new_env = proc_stmt env stmt in
		(* let (nm, tp) = List.hd new_env.var_scope.variables in *)
		separate_statements_s (other_stmts, new_env)

and proc_stmt (env:environment):(Sast.sstmt -> environment) = function
	Sast.Mandala(var_decl) ->
		(*Create new mandala object of name vname*)
		let {skind = typ1; svname= name1;}= var_decl in
		(* Create a new mandala *)
		let new_mandala = 
		{
			name= name1; 
			list_of_layers= []; 
			max_layer_radius= 0.0; 
			is_draw= false;
		} in 
		let new_mandalas = env.drawing.mandala_list @ [(name1, new_mandala)] in
		let new_vars = env.drawing.variables @ [(name1, Jast.JMandalat(new_mandala))] in 
		let new_drawing = {mandala_list=new_mandalas; variables = new_vars; java_shapes_list = env.drawing.java_shapes_list;} in
		let new_env = {drawing = new_drawing; functions = env.functions;} 
	in new_env
	| Sast.Layer(var_decl, v_radius, v_shape, v_count, v_offset, v_angular_shift) ->
		(* Return the var_decl for Jast*)
		let {skind = typ; svname = name;} = var_decl in 
		let (env, j_radius) = proc_expr env v_radius in
			(* Match with JData_types to get type of float *)
			let actual_radius = match j_radius 
				with Jast.JNumbert(j_radius) -> let new_num = j_radius in new_num 
				| _ -> raise (Error("Incorrect type for radius in layer"))
			in

		let (env, j_shape_typ) = proc_expr env v_shape in 
		let actual_j_shape = match j_shape_typ 
			with Jast.JShapet(j_shape_typ) -> j_shape_typ
			| _ -> raise (Error("Incorrect type for shape when adding to layer"))
		in 
		let (env, j_count) = proc_expr env v_count in 
			(* Match with jdata_typ to get the float count *)
			let actual_count = match j_count 
				with Jast.JInt(j_count) -> let new_count = j_count in new_count 
				| _ -> raise (Error("Incorrect type for count")) 
			in 
		let (env, j_offset) = proc_expr env v_offset in 
			let actual_offset = match j_offset 
				with Jast.JNumbert(j_offset) -> let new_offset = j_offset in new_offset
				| _ -> raise (Error("Incorrect type for offset"))
			in 
		let (env, j_angular_shift) = proc_expr env v_angular_shift in 
			let actual_angular_shift = match j_angular_shift
				with Jast.JInt(j_angular_shift) -> let new_angular_shift = j_angular_shift in new_angular_shift
				| _ -> raise (Error("Incorrect type for angular shift"))
			in 
		let new_layer = 
		{
			name = name;
			radius = actual_radius;
			shape = actual_j_shape;
			count = actual_count;
			offset = actual_offset;
			angularshift = actual_angular_shift;
		} in 
		(* Add to variable list and mandala list and update environment*)
		let new_variables = env.drawing.variables @ [(name, Jast.JLayert(new_layer))] in 
		let new_drawing = {mandala_list = env.drawing.mandala_list; variables = new_variables; java_shapes_list = env.drawing.java_shapes_list;} in 
		let new_env = {drawing = new_drawing; functions = env.functions;} in
		new_env

	| Sast.Shape(v_name, v_geo, v_size, v_color, v_rotation) ->
		(* 	| Shape of svar_decl * sdata_type  Sast.Geot * sdata_type * sdata_type * sdata_type *)
		let {skind = typ; svname = name;} = v_name in 
		let Sast.SGeo(s_geo) = v_geo in 

		let actual_size = match v_size with
		Sast.Float_Literal(s_size) ->  s_size
		| Sast.Id(var_name) -> let (name, value) = find_variable env var_name in
			let Jast.JNumbert(real_val) = value in real_val in

		(*let Sast.SNumber(s_size) = v_size in *)
		let Sast.SColor(s_color) = v_color in 
		(*let Sast.SNumber(s_rotation) = v_rotation in *)


		let actual_rotation = match v_rotation with
		Sast.Float_Literal(s_rotation) ->  s_rotation
		| Sast.Id(var_name) -> let (name, value) = find_variable env var_name in
			let Jast.JNumbert(real_val) = value in real_val in


		let new_shape = {
			name = name;
			geo = s_geo;
			size = actual_size;
			color = s_color;
			rotation=  actual_rotation;
		}
	in 
	let new_variables = env.drawing.variables @ [(name, Jast.JShapet(new_shape))] in 
	let new_drawing = {mandala_list= env.drawing.mandala_list; variables = new_variables; java_shapes_list= env.drawing.java_shapes_list;}
	in let new_env = {drawing = new_drawing; functions = env.functions;}
	in new_env

	(*Process an expression*)
	| Sast.Expr(expression)->
		(* Want to add this expression to the mandala list *)
		(* proc_expr returns a jexpr and an updated drawing *)
		let updated_expr = proc_expr env expression in 
		(* Return type of proc_expr is Jast.jexpr * Jast.drawing * Jast.jdata_type *)
		let (new_env, j_typ) = updated_expr in
		(* Now want to return new environment and jstmt *)
		let (update_names,updated_env_mandalas) = List.hd new_env.drawing.mandala_list in 
		let let_layers_listss = updated_env_mandalas.list_of_layers in 
		let layer_size = List.length let_layers_listss in 
		new_env

 	(*Process foreach loop*)
	| Sast.Foreach(i_var, i_start_var, i_end_var, for_statements) ->

		(*Get Jdata type values for start and end points*)
		let Sast.Id(i)= i_var in
		let i_start =
		match i_start_var with 
			Sast.Float_Literal(x) -> Jast.JNumbert(x)
			| _ -> raise(Error("Start value of this for loop is not a float")) in
		let i_end = 
		match i_end_var with 
			Sast.Float_Literal(x) -> Jast.JNumbert(x)
			| _ -> raise(Error("End value of this for loop is not a float")) in

		(*Remove i from list if it was found*)
		let new_variables = List.filter ( fun (n, v) -> if (n = i) then false else true) env.drawing.variables in  
		(*Add i with its updated value*)
		let updated_vars = new_variables @[(i, i_start)] in 
		(*Storing for later*)
		let store_old_vars = updated_vars in

		(*Create environment to pass to statement processing*)
		let updated_drawing = {env.drawing with variables = updated_vars} in
		let updated_env = {env with drawing = updated_drawing} in 

		(*Pull actual values from for loop start end end*)
		let Sast.Float_Literal(k_start) = i_start_var in
		let Sast.Float_Literal(k_end) = i_end_var in

		(*Increasing loops*)
		let rec pos_loop = function
			(env, var_name, k_cur, k_end) -> 

				(*i_cur is the data type to insert into variable table*)
				let i_cur = Jast.JNumbert(k_cur) in 

				(*Need to update actual value of i in the table and then update environment*)
				let new_variables = List.filter ( fun (n, v) -> if (n = var_name) then false else true) env.drawing.variables in  
				let updated_vars = new_variables @[(var_name, i_cur)] in 
				let updated_drawing = {env.drawing with variables = updated_vars} in
				let updated_env = {env with drawing = updated_drawing} in 

				(*Go through all statements*)
				let fresh_env = separate_statements_s(for_statements, updated_env) in
				let returning_env = 
					if not (k_cur >= k_end) then
						 pos_loop(fresh_env, var_name, k_cur +. 1.0, k_end)
					else
						fresh_env in
				returning_env in

		(*Decreasing loops*)
		let rec neg_loop = function
			(env, var_name, k_cur, k_end) -> 

				(*i_cur is the data type to insert into variable table*)
				let i_cur = Jast.JNumbert(k_cur) in 
				(*Need to update actual value of i in the table and then update environment*)
				let new_variables = List.filter ( fun (n, v) -> if (n = var_name) then false else true) env.drawing.variables in  
				let updated_vars = new_variables @[(var_name, i_cur)] in 
				let updated_drawing = {env.drawing with variables = updated_vars} in
				let updated_env = {env with drawing = updated_drawing} in 

				let fresh_env = separate_statements_s(for_statements, updated_env) in
				let returning_env = 
					if not (k_cur <= k_end) then
						 neg_loop(fresh_env, var_name, k_cur -. 1.0, k_end)
					else
						fresh_env in
				returning_env in

	    (*Process statements in the for loop*)
		let new_env = 
			 if (k_start <= k_end ) then
				pos_loop (updated_env, i, k_start, k_end)
			else
				neg_loop (updated_env, i, k_start, k_end)
		in

		(*Put last value of i into the stored variables*)
		let old_variables_minus_i = List.filter ( fun (n, v) -> if (n = i) then false else true) store_old_vars in  
		let old_vars_with_update_i = old_variables_minus_i @[(i, i_end)] in 

		let updated_drawing = {new_env.drawing with variables = old_vars_with_update_i} in
		let updated_env = {new_env with drawing = updated_drawing} in 
		updated_env

	(*Process return statement*)
	| Sast.Return(expr) -> 
		let (new_env, eval_expr) = proc_expr env expr in
		let return_val = eval_expr in
		(*Signal for a function call to grab the return statement*)
		let return_name = "return" in 
		let new_var = (return_name, return_val) in
		let updated_vars = new_env.drawing.variables @ [(return_name, return_val)] in 
		let updated_drawing = {mandala_list= new_env.drawing.mandala_list; variables = updated_vars; java_shapes_list= new_env.drawing.java_shapes_list;} 
		in let updated_env = {drawing = updated_drawing; functions = new_env.functions} in 
		updated_env


		(*Process assignment*)
	 | Sast.Assign(vardecl, assign_expr) ->
	 	(* TODO: Finish this*)
		let (new_env, eval_expr) = proc_expr env assign_expr in 
		(* now get the variable *)
		let {skind = typ; svname = name;} = vardecl in 

	(* Already checked types in semantic, so just need to make sure adding correct type for Jast *)
	(* doing this to make sure we are adding the correct value for Jast to the drawing with includes all variables *)
		let get_val_and_type = match eval_expr
			with Jast.JNumbert(eval_expr) -> Jast.JNumbert(eval_expr)
			| Jast.JBooleant(eval_expr) -> Jast.JBooleant(eval_expr)
			| Jast.JShapet(eval_expr) -> Jast.JShapet(eval_expr)
			| Jast.JGeot(eval_expr) -> Jast.JGeot(eval_expr)
			| Jast.JLayert(eval_expr) -> Jast.JLayert(eval_expr)
			| Jast.JMandalat(eval_expr) -> Jast.JMandalat(eval_expr)
			| Jast.JColort(eval_expr) -> Jast.JColort(eval_expr)
			| Jast.JVoid -> Jast.JVoid
			| Jast.JArrayt -> Jast.JArrayt
			| _ -> raise(Error("This expression does not have a supported type here!"))
		in 

		let (n,v) = try List.find (fun (s,_) -> s=name) env.drawing.variables 
			with Not_found -> (name,get_val_and_type) in

		let new_variables = List.filter ( fun (n, v) -> if (n = name) then false else true) new_env.drawing.variables in  

		let updated_vars = new_variables @[(n, get_val_and_type)] in 
		let updated_drawing = {mandala_list= new_env.drawing.mandala_list; variables = updated_vars; java_shapes_list= new_env.drawing.java_shapes_list;} 
			in let updated_env = {drawing = updated_drawing; functions = new_env.functions} in updated_env

	| _ -> raise (Error("unsupported statement found")) 


(*Simply add function declaration to our environment *)
let proc_func (env: environment):(Sast.sfuncdecl -> environment) = function
	my_func ->

		let new_env = {
			drawing = env.drawing;
			functions = env.functions @ [my_func];
		} in 
		new_env


(* Parse each statement and keep track of environment*)
(* returns Jast.jStmt list * env *)
let rec separate_functions_s (funcs, env: Sast.sfuncdecl list * environment) = match funcs
	with [] -> env
	| [func] -> proc_func env func
	| func :: other_funcs ->
		let new_env = proc_func env func in
		separate_functions_s (other_funcs, new_env)



(* TODO: Can change so gen_java doesn't return javaprogram, can just return javaclass list *)
let gen_java (env:environment):(Sast.sprogram -> environment)= function 
	Sast.SProg(s,f)-> 
		(* Check if the program has at least one statement *)
		let x = List.length s in
		if (x>0) then (
			(*Check order of statements *)
			let update_list = []  in 
			(* Already reversed the statements in semantic when going from ast to jast, so don't need to reverse again *)
			let updated_env = separate_functions_s (f, env) in 
			let updated_env = separate_statements_s (s, updated_env) in  (* List.map( fun stmt_part -> separate_statements_s prog_stmts env ) in *)
			let l = List.length updated_env.functions in
			updated_env
		)	

		else 
			raise (Error("Input has no arguments"))

(*Process a layer and load them all into the shapes structure in environment *)
let extract_shapes_from_layer (new_list:Jast.jShape list):(Jast.layer * float -> Jast.jShape list) = function
	(my_layer, big_radius) -> 

		let listed_shape = my_layer.shape in
		let multiple_mandala_offset = 
			if (big_radius > 0.0) 
			then 
				big_radius +. listed_shape.size +. 200.0
			else
				big_radius
			in
		let count = my_layer.count in
		if (count >= 1 && listed_shape.geo = "square")
		then 
			let rec loop = function
			(new_list, k) -> 
			 let rad_offset = my_layer.offset *. pi /. 180.0 in 
			 let my_angle = -1.0 *. rad_offset +. pi/.2.0 -. (float_of_int k) *. 2.0*.pi /.(float_of_int) my_layer.count in 
			 let x_pos = cos (my_angle) *. my_layer.radius in
			 let y_pos = sin (my_angle) *. my_layer.radius in
			 let extra_rotation = 
			 	if (my_layer.angularshift = 1)
			 	then
			 		(pi/.2.0 -. my_angle) *. 180.0 /. pi 
			 	else
			 		0.0
			 in
			 let rotat = listed_shape.rotation +. extra_rotation in
			 let color = listed_shape.color in 
			 let new_shape = Jast.Square(listed_shape.size, x_pos, y_pos, rotat, color) in 
				 if (k > 0) then 
				 let updated_k = k - 1 in 
				 	loop (new_list@[new_shape], updated_k)
				 else
					new_list@[new_shape]
			in 
			loop(new_list, count - 1)

		else if (count >= 1 && listed_shape.geo = "circle")
		then 
			let rec loop = function
			(new_list, k) ->
			 let rad_offset = my_layer.offset *. pi /. 180.0 in 
 			 let my_angle = -1.0 *. rad_offset +. pi/.2.0 -. (float_of_int k) *. 2.0*.pi /. (float_of_int) my_layer.count in 
			 let x_pos = cos (my_angle) *. my_layer.radius in
			 let y_pos = sin (my_angle) *. my_layer.radius  in
			 let color = listed_shape.color in 
			 let new_shape = Jast.Circle(listed_shape.size, x_pos, y_pos, color) in 
				 if (k > 0) then 
				 let updated_k = k - 1 in 
				 	loop (new_list@[new_shape], updated_k)
				 else
					new_list@[new_shape] 
			in 
			loop(new_list, count - 1)

		else if (count >= 1 && listed_shape.geo = "triangle")
		then 
			let rec loop = function
			(new_list, k) -> 
			 let rad_offset = my_layer.offset *. pi /. 180.0 in 
			 let my_angle = -1.0 *. rad_offset +. pi/.2.0 -. (float_of_int k) *. 2.0*.pi /. (float_of_int my_layer.count) in 
			 let x_pos = cos (my_angle) *. my_layer.radius in
			 let y_pos = sin (my_angle) *. my_layer.radius in
			 let extra_rotation = 
			 	if (my_layer.angularshift = 1)
			 	then
			 		(pi/.2.0 -. my_angle) *. 180.0 /. pi 
			 	else
			 		0.0
			 in
			 let rotat = listed_shape.rotation +. extra_rotation in
			 let color = listed_shape.color in 
			 let new_shape = Jast.Triangle(listed_shape.size, x_pos, y_pos, rotat, color) in 
				 if (k > 0) then 
				 let updated_k = k - 1 in 
				 	loop (new_list@[new_shape], updated_k)
				 else
					new_list@[new_shape]
			in 
			loop(new_list, count - 1)

	else 

	raise (Error ("Only circles, squares, and triangles supported."))

(*Pulls out all layers and deals with max radius given a mandala*)
let get_layers  = function 
	mandala ->
	let n = List.length mandala.list_of_layers in 
	let radius = mandala.max_layer_radius in
	let list_of_layers = mandala.list_of_layers in
	let result = List.fold_left (fun a layer -> (layer, radius) :: a) [] list_of_layers in
	result

(*Checks mandala and outputs list of shapes generated. Only draws those with is_draw boolean*)
let process_mandala = function
	mandala ->
	if (mandala.is_draw = true) then
	(*raise (Error("Some truth exists!"));*)
	let layers_with_radii = get_layers mandala in
		let num_layers = List.length layers_with_radii in
		List.fold_left extract_shapes_from_layer [] layers_with_radii
	else
		[]

(* Create empty initial environment *)
(* The environment keeps track of the drawing we are creating *)
let empty_drawing_env=
{
	mandala_list = [];
	variables = [];
	java_shapes_list = [];
}

let empty_environment = {
 	drawing = empty_drawing_env;
 	functions = [];
}

(*Go through all mandalas and eventually convert into shape structures*)
let rec process_mandalas (mandalas, shapes, total:Jast.mandala list * Jast.jShape list * float) = match mandalas 
	with [] -> shapes
	| [mandala] -> 
		let x = mandala.max_layer_radius in 
		let new_mandala = {
			name = mandala.name;
			list_of_layers = mandala.list_of_layers;
			max_layer_radius = total;
			is_draw = mandala.is_draw
		} in
		total = total +. x;
		(shapes @ process_mandala new_mandala)
	| mandala :: other_mandalas -> 
		let x = mandala.max_layer_radius in 
		let new_mandala = {
			name = mandala.name;
			list_of_layers = mandala.list_of_layers;
			max_layer_radius = total;
			is_draw = mandala.is_draw
		} in
		total = total +. x;
		(let new_shapes = process_mandala mandala in
		process_mandalas (other_mandalas, (shapes @ new_shapes),total))

(*Final conversion from Sast program to Jast program which runs all statements and moves into final structure*)
let actual_final_convert (check_program: Sast.sprogram): (Jast.javaprogram) = 
	let env = empty_environment in 
	(*Parse all statements and update environment*)
	let new_draw_env = gen_java env sast in 
	let mandala_lists = new_draw_env.drawing.mandala_list in
	let all_mandalas = List.rev (List.fold_left (fun a (_, mandala) -> mandala :: a) [] mandala_lists) in
	let total_radius = 0.0  in
	(*Get shapes from mandalas*)
	let all_shapes = process_mandalas (all_mandalas, [], total_radius) in
	(*All classes will have same name to allow java compilation*)
	let prog_name = Jast.CreateClass("Program") in 
    Jast.JavaProgram(prog_name, all_shapes)
