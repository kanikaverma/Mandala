open Ast
open Sast

exception Error of string

(*Storing all variables, including parent for scoping*)
type symbol_table={
	parent : symbol_table option;
	variables: (string * smndlt) list 
}

(*Storing all functions*)
type function_table={
	functions: (string * smndlt * svar_decl list * sstmt list) list
} 

(*Complete environment*)
type translation_environment ={
	var_scope: symbol_table;
	fun_scope: function_table;
}

(*List of java built-in colors, for use for color in shape*)
let list_of_colors = ["black"; "red"; "blue"; "cyan"; "darkGray"; "gray"; "green"; "lightGray"; "orange"; "pink"; "white"; "yellow"]

(* Returns the name, type and value *)
let find_variable (scope: symbol_table) name=
	try
		List.find (fun (s,_) -> s=name) scope.variables
 
	with Not_found -> raise (Error ("Unable to find variable in lookup table "^name))

(*Returns function with all parameters*)
let rec find_function (scope: function_table) name =
	try
		List.find (fun (s, _, _, _) -> s=name) scope.functions
	with Not_found ->
		raise (Error("Function not found in function table! "^name))

(*Adds a variable to variable table*)
let add_to_var_table (env, name, typ)  =
 	try
		let (n, t) = List.find (fun(s,_)-> s=name) env.var_scope.variables in
		env 
	with Not_found ->
		let new_vars = (name, typ)::env.var_scope.variables in
		let new_sym_table = {parent = env.var_scope.parent; 
			variables = new_vars;} in
		let new_env = { env with var_scope = new_sym_table} in
		new_env


 (*Adds a function to the table*)
 let add_to_func_table env sfunc_decl =
	let func_table = env.fun_scope in 
	let old_functions = func_table.functions in 
			let func_name = sfunc_decl.sfname in 
			let func_type = sfunc_decl.sreturntype in 
			let func_formals = sfunc_decl.sformals in 
			let func_body = sfunc_decl.sbody in 
			let new_functions = (func_name, func_type, func_formals, func_body)::old_functions
		in 
			let new_fun_scope = {functions = new_functions} in
			let final_env = {env with fun_scope = new_fun_scope} in 
			final_env 

let rec find_function (scope: function_table) name=
		List.find (fun (s, _, _, _) -> s = name) scope.functions

let rec extract_type (scope: function_table) name = function
	(smndlt, string) -> (smndlt)

let get_formal_arg_types env = function
	(smndlt, string) -> (smndlt)

(*Process a single expression, checking for type matching and compatibility*)
let rec semantic_expr (env:translation_environment):(Ast.expr -> Sast.sexpr * smndlt  * translation_environment) = function

	Ast.Id(vname) ->
		(* Check for built-in Ids for shapes like circle, triangle, and square *)
		if (vname="circle" || vname="triangle" || vname="square")
			then
			let geo_typ = Sast.Geot in 
			let name = vname in 
			(Sast.Id(name), geo_typ, env)
		

		else (*Checks for build in Id of color *)
			let return_name = try let color = List.find (fun s -> s=vname) list_of_colors in
				let color_typ = Sast.Colort in 
				let name = vname in 
				(Sast.Id(name), color_typ, env)
			
			with Not_found ->
			(*Otherwise name is treated as a variable*)
			let vdecl = try
				find_variable env.var_scope vname
			with Not_found ->
				raise (Error("undeclared identifier: "^vname))
				(* Want to add the symbol to our symbol table *)
			in 
			let (name, typ) =vdecl in 
			(Sast.Id(name), typ, env) 

		in return_name
	
		(* AST Call of string * expr list*)
	| Ast.Float_Literal(num) ->
		(Sast.Float_Literal(num), Sast.Numbert, env)
	| Ast.Literal(num) ->
		(Sast.Literal(num), Sast.Integert, env)
	| Ast.Binop(term1, operator, term2) ->
		(* convert to Sast.Binop *)
		
		let (eval_term1, typ1, new_env) = semantic_expr env term1 in 
		let (eval_term2, typ2, new_env) = semantic_expr env term2 in 
		(* now translate Ast.operator to Sast.operator *)
		if not (typ1 = typ2)
		then raise (Error("Mismatched types, invalid operation"))
		else 
			(* Checking the types for binary operators and will do evaluation of binop in sast_to_jast *)
			(Sast.Binop(eval_term1, operator, eval_term2), typ1, env)

		
	(*Process function call*)
	| Ast.Call(fid, args) ->

		if not (  ((List.length args) > 0) ) then ( 
			(*Make sure that func_decl has no formal arguments*)
			let (_, ret_typ, decl_list, _) = find_function env.fun_scope fid in
			let decl_size = List.length decl_list in
			if (decl_size > 0) then
				raise (Error("This function expects paramaters but none were provided"))
			else 
				(Sast.Call(fid, []), ret_typ, env)
		)
		 else 

		
		let actual_types = List.map (fun expr -> semantic_expr env expr) args in
		let actual_len = List.length args in
		 (*get list of just types from list of (type, string) tuples, [] is an accumulator*)
		let actual_types_list = List.fold_left (fun a (_,typ, ret_env) -> typ :: a) [] actual_types in    
		let actual_expr_list = List.fold_left (fun a (expr,_, ret_env) -> expr :: a) [] actual_types in
		let len = List.length actual_expr_list in
		if (fid = "draw")
		then 
			
			if (len == 1) 
			then (Sast.Call(fid, actual_expr_list), Sast.Voidt, env)
			else raise(Error("Draw function has incorrect parameters"^ string_of_int actual_len)) 	
		else 
			if (fid ="addTo")
			then (* Check that length is greater than 1, or at least two args *)
				if (len > 1)
				then 
					(Sast.Call(fid, actual_expr_list), Sast.Mandalat, env)
				else raise(Error("addTo function has incorrect parameters"^ string_of_int actual_len))

			else
				try (let (fname, fret, fargs, fbody) =
				
				find_function env.fun_scope fid in
				
				
				let formal_types =  List.map (fun farg -> let arg_type =
					get_formal_arg_types env (farg.skind, farg.svname) in arg_type)
				fargs in
				if not (actual_types_list=formal_types) 
				then
					raise (Error("Mismatching types in function call"))
				else 
					let actual_expr_list = List.fold_left (fun a (expr,_, ret_env) -> expr :: a) [] actual_types in
					(Sast.Call(fname, actual_expr_list), fret, env)
					(* Call of string * sexpr list*)

				)
				with Not_found -> 
					let numFuncs = List.length env.fun_scope.functions in
					raise (Error(fid^"undeclared function "^string_of_int numFuncs)) 
	
	| _ -> raise (Error("invalid expression, was not able to match expression")) 

(*Convert matching types*)
let proc_type = function
  	 Ast.Booleant -> Sast.Booleant
  	| Ast.Shapet -> Sast.Shapet
	| Ast.Layert -> Sast.Layert
	| Ast.Mandalat -> Sast.Mandalat
	| Ast.Arrayt -> Sast.Arrayt
	| Ast.Numbert -> Sast.Numbert
	| Ast.Voidt -> Sast.Voidt

(*Process a variable declaration*)
let proc_var_decl = function 
	(var_decl, env) -> 
		let k = var_decl.kind in
		let v = var_decl.vname in 
		let sskind = 
 		if (k = Ast.Numbert) then
 			Sast.Numbert
 		else if (k = Ast.Geot) then
 			Sast.Geot
 		else if (k = Ast.Colort) then
 			Sast.Colort
 		else
 			proc_type k in

		let new_svar_decl = {
			skind = sskind;
			svname  = v;
		} in 
		let new_env = add_to_var_table (env, new_svar_decl.svname, new_svar_decl.skind) in 
	(new_svar_decl, new_env)

(*Check all formal arguments of a function*)
let rec proc_formals (var_decl_list, env, update_var_decl_list: Ast.var_decl list * translation_environment * Sast.svar_decl list) = match var_decl_list
	with [] -> (update_var_decl_list, env)
	| [var_decl] -> let (new_var_decl, new_env) = proc_var_decl(var_decl, env) in (update_var_decl_list@[new_var_decl], new_env)
	| var_decl :: other_var_decls ->
		let (new_var_decl, new_env) = proc_var_decl(var_decl, env) in
		proc_formals (other_var_decls, new_env, update_var_decl_list@[new_var_decl])

let var_empty_table_init = {parent=None; variables=[]}
let fun_empty_table_init = { functions = [];}
let empty_environment = 
{
	var_scope =  var_empty_table_init;
	fun_scope = fun_empty_table_init;
}

(*Check individual statement*)
let rec semantic_stmt (env:translation_environment):(Ast.stmt -> Sast.sstmt * smndlt * translation_environment) = function
	Ast.Mandala(mandala_arg) ->


		let {vname=name} = mandala_arg in
		let typ= Sast.Mandalat in
		(* add to current env *)
		let new_env = add_to_var_table (env, name, typ) in

		(Sast.Mandala({skind = typ; svname = name}), typ, new_env)
	| Ast.Layer(v_name, v_radius, v_shape, v_count, v_offset, v_angular_shift) ->
		let {vname=name} = v_name in 
		let typ = Sast.Layert in 
		let (s_radius, s_r_typ, env) = semantic_expr env v_radius in 
		let (s_shape, s_s_typ, env) = semantic_expr env v_shape in 
		let (s_count, s_c_typ, env) = semantic_expr env v_count in 
		let (s_offset, s_o_typ, env) = semantic_expr env v_offset in 
		let (s_angular_shift, s_a_typ, env) = semantic_expr env v_angular_shift in 
		let new_env = add_to_var_table (env, name, typ) in 
		(Sast.Layer({skind = typ; svname = name;}, s_radius, s_shape, s_count, s_offset, s_angular_shift), typ, new_env)



	| Ast.Shape(v_name, v_geo, v_size, v_color, v_rotation) ->
		
		
		let {vname=name} = v_name in 
		let typ = Sast.Shapet in 
		let s_geo = match v_geo with 
			
			Ast.Id(v_geo) -> let new_geo = v_geo in new_geo 
			| _ -> raise (Error("WRONG FORMAT FOR GEO IN SHAPE!"))
		in 
		let updated_s_geo = Sast.SGeo(s_geo) in 
		let (size_stmt, typ, env) = semantic_expr env v_size in 
		(* Checking that the shape's size is a float and returning a sexpr *)

		let size_value = match typ with
			Sast.Numbert -> size_stmt
			| _ -> raise (Error ("Size wasn't a numbert!"))

		
		in 

		let s_color = match v_color with 
			Ast.Id(v_color) -> let new_color = v_color in new_color
			| _ -> raise (Error("WRONG FORMAT FOR COLOR IN SHAPE!"))
		in 
		let updated_s_color = Sast.SColor(s_color) in 

		
		let (rotation_stmt, typ, env) = semantic_expr env v_rotation in 

		let rotation_value = match typ with
			Sast.Numbert -> rotation_stmt
			| _ -> raise (Error ("Rotation wasn't a numbert!"))
		in 

		let new_env = add_to_var_table (env, name, typ) in 
		
		(Sast.Shape({skind = typ; svname=name;}, updated_s_geo, size_value, updated_s_color, rotation_value), typ, new_env)
	

	

	| Ast.Expr(expression) -> 
		let newExpr = try
			semantic_expr env expression 
		with Not_found ->
			raise (Error("undefined expression"))  			
		in let (x, typ, ret_env)= newExpr in 
		(Sast.Expr(x), typ, env)
	

	(*Assign is of form var_decl*expr  *)
	| Ast.Assign(lefthand, righthand) ->
		let right_assign =
			semantic_expr env righthand
		in let (assign_val, typ, ret_env) = right_assign in
		let {kind=typ2; vname=name2} = lefthand 
		

		in let result = match typ with (*Assign of svar_decl * sexpr*)
			 typ2 -> let new_env = add_to_var_table (env, name2, typ2) 
				in (Sast.Assign(({skind = typ2; svname = name2}), assign_val), typ, new_env) (* check strctural equality *)
			| _ -> raise (Error("Assignment could not be typechecked")) 
		in result 

	| Ast.Return(x) -> 
		let (_, returntype) = List.find (fun (s,_) -> s="return") env.var_scope.variables in
		let newExpr = semantic_expr env x in
		let (x, typ, ret_env)= newExpr in 
		let result = match typ with 
			| _ -> raise (Error("User defined function is returning something of the wrong type"))
				
		in result 

	| Ast.Foreach(varName, countStart, countEnd, body) ->
		(*create custom env for the scope of the for loop*)
		let body = List.rev body in
		let func_env=
			{
				var_scope = {parent = env.var_scope.parent; variables=(varName,Sast.Numbert)::env.var_scope.variables};
				fun_scope = env.fun_scope;
			} in 
		let empty_list=[] in
		let (statements, func_env) = separate_statements (body, func_env, empty_list) in
		(Sast.Foreach(Sast.Id(varName), Sast.Float_Literal(countStart), Sast.Float_Literal(countEnd), statements), Sast.Loopt, env)

	| _ -> raise (Error("Unable to match statement"))
	
	(*Check a list of statements by recursively going through each one*)
	and separate_statements (stmts, env, update_list:Ast.stmt list * translation_environment * Sast.sstmt list) = match stmts 
		with [] -> (update_list, env)
		| [stmt] -> let (new_stmt, typ, new_env) = semantic_stmt env stmt in (update_list@[new_stmt], new_env)
		| stmt :: other_stmts ->
			
			let (new_stmt, typ, new_env) = semantic_stmt env stmt in
			separate_statements (other_stmts, new_env, update_list@[new_stmt])
			

	(*Process individual function*)
	let rec semantic_func (env: translation_environment): (Ast.func_decl -> Sast.sfuncdecl * translation_environment) = function
		my_func ->
		let fname = my_func.fname in
		let returntype = my_func.returntype in 
		let formals = my_func.formals in 
		let body = my_func.body in


		let empty_list = [] in
		let new_returntype = proc_type returntype in
		let func_env=
			{
				var_scope = {parent = env.var_scope.parent; variables=[("return",new_returntype)]};
				fun_scope = fun_empty_table_init;
			} in 
		(*gets list of formals in sast format, fills the func_env with the inputs in the var table*)
		let (new_formals, func_env) = proc_formals (formals, func_env, empty_list) in
		(*walks through body of function, checking types etc.*)
		let (new_stmts, func_env) = separate_statements(body, func_env, empty_list) in
		(*check that function returned the right thing-- get the return stmt from stmt list, check its typ against returntyp*)

		let sfuncdecl = {
			sfname = fname;
			sreturntype = new_returntype;
			sformals = new_formals;
			sbody = new_stmts;
		} in

		let env = add_to_func_table env sfuncdecl in

		(sfuncdecl, env)

    (*Process list of functions*)
	let rec separate_functions (functions, env, update_list: Ast.func_decl list * translation_environment * Sast.sfuncdecl list) = match functions 
		with [] -> (update_list, env)
		| [func] -> 
		
			let (new_func, new_env) = semantic_func env func in (update_list@[new_func], new_env)
		| func :: other_funcs ->
			let (new_func, new_env) = semantic_func env func in
			
			separate_functions (other_funcs, new_env, update_list@[new_func])

	(*Process entire AST program and convert to SAST program*)	
	let rec semantic_check (check_program: Ast.program): (Sast.sprogram) =  
		let (prog_stmts, prog_funcs) = check_program in 
		let env = empty_environment in 
		let empty_list = []  in 
		let reverse_prog_stmts = List.rev prog_stmts in 
		let (resulting_functions, env) = separate_functions (prog_funcs, env, empty_list) in
		let (statements, env) = separate_statements (reverse_prog_stmts, env, empty_list) in  
		

		Sast.SProg(statements, resulting_functions)
		