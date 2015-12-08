open Ast
open Sast

exception Error of string

(*symbol table *)
type symbol_table={
	parent : symbol_table option;
	variables: (string * sdata_type) list
}

type function_table={
	functions: (string * sdata_type * svar_decl list * sstmt list) list
} 

(*functions: (string * mndlt * var_decl list * stmt list) list*)
(*envioronment*)
type translation_enviornment ={
	(* return_type: datatype; fnctions return type 
		return_seen: bool does the function have a  return statemtn
	location: string;  using for global or local checking 
	global_scope: symbol_table; *)
	var_scope: symbol_table;
	fun_scope: function_table;
}
(* scope is env.var_scope *)
(* scope.variabkles is a list of (string * sdata_type) *)
let find_variable (scope: symbol_table) name=
	try
		List.find (fun (s,_) -> s=name) scope.variables 
	(* with Not_found -> try List.find (fun (s, _) -> s=name) scope.parent.variables*) 
	with Not_found -> raise (Error ("THIS FAILED AGAIN!!! "^name))
		(*	Some(parent)-> find_variable parent name
		| _ -> raise (Error("THIS IS NOT FOUND "^name)) *)

let rec find_function (scope: function_table) name =
	try
		List.find (fun (s, _, _, _) -> s=name) scope.functions
	with Not_found ->
		raise (Error("adding function to func_table FAILED! "^name))
(* PROBABLY WON'T USE
update variables 
let update_var env (name, datatype) = 
	let ((_,_), location ) =
		try (fun var_scope -> ((List.find (fun (dtype, id) -> id = name) var_scope), 1)) env.var_scope.variables
	with Not_found -> try (fun var_scope -> ((List.find (fun (_,id) -> id=name) var_scope), 2))
	env.global_scope.variables
with Not_found -> raise Not_found in
let new_envf =
match location with
	1 ->
		let new_vars = List.map(fun(t, n) ->
			if (n=name) then (datatype, name)
		else (t, n)) env.var_scope.variables in
		let new_sym_table = {parent = env.var_scope.parent; variables = new_vars;} in
		let new_env = {env with var_scope = new_sym_table} in
		new_env
	| 2 ->
		let new_vars = List.map (fun (t, n) ->
			if (n=name) then (datatype, name) else (t,n)) env.global_scope.variables in
		let new_sym_table = {parent = env.var_scope.parent; variables = new_vars;}
	in 
	let new_env = {env with global_scope = new_sym_table} in
	new_env
	| _ -> raise(Error("undefined scope"))
in new_envf
*)
(* CURRENTLY CANNOT UPDATE VARIABLES, CAN ONLY DECLARE THEM *)


let add_to_var_table env name t =
	let new_vars = (name, t)::env.var_scope.variables in
	let new_sym_table = {parent = env.var_scope.parent; 
		variables = new_vars;} in
	(*let test_name = "heeyy" in
	 let tester = try 
		raise (Error("FAIL "^name))
	with Not_found ->
		raise (Error("VAR NEVER ADDED "^name)) 
	in *)
	let new_env = { env with var_scope = new_sym_table} in
	new_env
(*ALSO DEFINE SFUNC_DECL in your thing!!! *)
(* let get_return_datatype env  *)
(* let get_type_from_datatype *)
(* let add_to_func_table env sfunc_decl =
	let func_table = env.fun_scope in 
	let old_functions = func_table.functions in 
	match sfunc_decl with
		SFunc_Decl(sfuncdecl, sdatatype) ->
			let func_name = sfuncdecl.sfname in 
			let func_type = sfuncdecl.sreturntype in (may need to call function on sreturntypes to get the type )
			let func_formals = sfuncdecl.sformals in 
			let func_body = sfuncdecl.sbody in 
			let new_functions = (func_name, func_type, func_formals, func_body)::old_functions
		in 
			let new_fun_scope = {functions = new_functions} in
			let final_env = {env with fun_scope = new_fun_scope} in 
			final_env *)

let rec find_function (scope: function_table) name=
		List.find (fun (s, _, _, _) -> s = name) scope.functions
	(*with Not_found -> raise Not_found*)
let rec extract_type (scope: function_table) name = function
	(sdata_type, string) -> (sdata_type)
let get_formal_arg_types env = function
	(sdata_type, string) -> (sdata_type)

let rec semantic_expr (env:translation_enviornment):(Ast.expr -> Sast.sexpr * sdata_type * translation_enviornment) = function

	Ast.Id(vname) ->
		(* if (vname = "m") then 
			let v_typ = Sast.Mandalat in
			(Sast.Id(vname), v_typ, env)
		else *)
		let vdecl = try
			find_variable env.var_scope vname
		with Not_found ->
			raise (Error("undeclared identifier: "^vname))
			(* Want to add the symbol to our symbol table *)
		in 
		let (name, typ) =vdecl in 
		(Sast.Id(name), typ, env)
		(* AST Call of string * expr list*)
	| Ast.Float_Literal(num) ->
		(Sast.Float_Literal(num), Sast.Numbert(num), env)
	| Ast.Literal(num) ->
		(Sast.Literal(num), Sast.Literalt, env)



(*
	 Literal of int
	| Float_Literal of float
	| Number of float
	| Noexpr
	| Id of string
	| Binop of expr * op * expr
	| Call of string * expr list

*)


	| Ast.Call(fid, args) ->
		
		let actual_types = List.map (fun expr -> semantic_expr env expr) args in
		(*let actual_type_names = List.iter extract_type actual_types*)
		let actual_len = List.length args in
		let actual_types_list = List.fold_left (fun a (_,typ, ret_env) -> typ :: a) [] actual_types in     (*get list of just types from list of (type, string) tuples, [] is an accumulator*)
		let actual_expr_list = List.fold_left (fun a (expr,_, ret_env) -> expr :: a) [] actual_types in
		let len = List.length actual_expr_list in
		if (fid = "draw")
		then 
			
			if (len == 1) 
			then (Sast.Call(fid, actual_expr_list), Sast.Void, env)
			else raise(Error("Draw function has incorrect parameters"^ string_of_int actual_len)) 	
		else 
			if (fid ="addTo")
			then (* Check that length is greater than 1, or at least two args *)
				if (len > 1)
				then 
					(Sast.Call(fid, actual_expr_list), Sast.Mandalat, env)
				else raise(Error("addTo function has incorrect parameters"^ string_of_int actual_len))
					(* The first argument should be the mandala that you are adding the layer to *)
					(* let update_mandala = List.hd actual_expr_list in 
					let update_mandala_name = match update_mandala 
						with Ast.Id(update_mandala) -> let m_name = update_mandala in m_name 
						| _ -> raise (Error("This name is not a string! "^update_mandala_name));
					(* we know that this mandala has been declared, 
					because above when the args are mapped, they are each checked with find_variable *)
					in *)
			else
				try (let (fname, fret, fargs, fbody) =
				find_function env.fun_scope fid in
				
				(*let actual_type_names = 
					List.find (fun (_,s) -> s) actual_types*)

				(*let (actual_arg_type, _) = actual_types in*)
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
		with Not_found -> raise (Error("undeclared function ")) 
	(* WORKING ONE Ast.Call(vname, func_args) ->
		let func_call = try
			find_function env.fun_scope vname 
		with Not_found ->
			raise (Error("undeclared identifier"^vname))
		in let (fname, freturntype, fformals, fargs) = func_call in 
			Sast.Call(fname, fargs), freturntype *)
		(* | Call of string * sexpr list *)
	(* check type of right ahndside and recurse on that to check that it matches lefthand side*)
	(*once it is confirmed, compare left type and righthand type and then add it to the symbol table *)
	| _ -> raise (Error("invalid  assignment")) 

let rec semantic_stmt (env:translation_enviornment):(Ast.stmt -> Sast.sstmt * sdata_type * translation_enviornment) = function
	Ast.Mandala(mandala_arg) ->

		(*let stmt_decl =try
				find_variable env.var_scope mandala_arg.vname
		with Not_found ->
			raise (Error ("undeclared"^mandala_arg.vname))
		in 
		let (typ, name) = stmt_decl in
		Sast.Mandala({skind=typ; svname =name}), typ*)

		let {vname=name} = mandala_arg in
		let typ= Sast.Mandalat in
		(*let typ = mandala_arg.kind in
		let name = mandala_arg.vname in *)
		(* add to current env *)
		let new_env = add_to_var_table env name typ in
		(* let head_var = List.hd new_env.var_scope.variables in
		let (a, b) = head_var in
		 raise (Error("HELLO "^a)); *)

		(Sast.Mandala({skind = typ; svname = name}), typ, new_env)
	| Ast.Layer(v_name, v_radius, v_shape, v_count, v_offset, v_angular_shift) ->
		(* raise (Error ("ADD CODE FOR LAYER CREATION!")) *)
		(* semantic_expr returns Sast.sexpr * sdata_type * translation_enviornment *)
		let {vname=name} = v_name in 
		let typ = Sast.Layert in 
		let (s_radius, s_r_typ, env) = semantic_expr env v_radius in 
		(* ADD IN CHECKS like if s_r_typ is float continue, else WRONG TYPE! *)
		let (s_shape, s_s_typ, env) = semantic_expr env v_shape in 
		let (s_count, s_c_typ, env) = semantic_expr env v_count in 
		let (s_offset, s_o_typ, env) = semantic_expr env v_offset in 
		let (s_angular_shift, s_a_typ, env) = semantic_expr env v_angular_shift in 
		let new_env = add_to_var_table env name typ in 
		(Sast.Layer({skind = typ; svname = name;}, s_radius, s_shape, s_count, s_offset, s_angular_shift), typ, new_env)

	(* IN AST Layer of var_decl * expr * expr * expr * expr * expr  *)
		(* IN SASST: | Shape of svar_decl * sexpr * sexpr * sexpr * sexpr *)

	| Ast.Shape(v_name, v_geo, v_size, v_color, v_rotation) ->
		(* raise (Error ("ADD CODE FOR SHAPE CREATION!")) *)
		let {vname=name} = v_name in 
		let typ = Sast.Shapet in 
		let s_geo = match v_geo with 
			Ast.Id(v_geo) -> let new_geo = v_geo in new_geo (* semantic_expr env v_geo in *)
			| _ -> raise (Error("WRONG FORMAT FOR GEO IN SHAPE!"))
		in 
		let updated_s_geo = Sast.Geot(s_geo) in 
		let (s_size, s_s_typ, env) = semantic_expr env v_size in 
		(* let (s_color, s_c_typ, env) = semantic_expr env v_color in *)
		let s_color = match v_color with 
			Ast.Id(v_color) -> let new_color = v_color in new_color
			| _ -> raise (Error("WRONG FORMAT FOR COLOR IN SHAPE!"))
		in 
		let updated_s_color = Sast.Colort(s_color) in 

		let (s_rotation, s_r_typ, env) = semantic_expr env v_rotation in 
		let new_env = add_to_var_table env name typ in 
		(* 	| Shape of svar_decl * sdata_type * sdata_type * sexpr * sdata_type *)
		(Sast.Shape({skind = typ; svname=name;}, updated_s_geo, s_s_typ, updated_s_color, s_r_typ), typ, new_env)
	(* IN AST Shape of var_decl * expr * expr * expr * expr *)
	(* IN SAST: | Layer of svar_decl * sexpr * sexpr * sexpr * sexpr * sexpr  *)

	| Ast.Expr(expression) -> 
		let newExpr = try
			semantic_expr env expression 
		with Not_found ->
			raise (Error("undefined expression"))  			(*this error should be fixed to something more relevant*)
		in let (x, typ, ret_env)= newExpr in 
		(Sast.Expr(x), typ, env)

	| Ast.Assign(lefthand, righthand) ->
		
		let right_assign =
			semantic_expr env righthand
		in let (assign_val, typ, ret_env) = right_assign in
		let {kind=typ2; vname=name2} = lefthand 
		(*let lefthand {kind = typ2; vname = name2} = x *)
		(*let (typ2, name2) =(lefthand.kind, lefthand.vname) *)

		in match typ with (*Assign of svar_decl * sexpr*)
			 typ2 -> let new_env = add_to_var_table env name2 typ2 in (Sast.Assign(({skind = typ2; svname = name2}), assign_val), typ, new_env) (* check strctural equality *)
			| _ -> raise (Error("Assignment could not be typechecked")) 
	(* 
	let rec extract_type (scope: function_table) name = function
		(sdata_type, string) -> (sdata_type)
	let get_formal_arg_types env = function
		(sdata_type, string) -> (sdata_type) 
	*)

	(*let rec get_sdatatype env name = function 
		(_,) *)
	(* wnat to go from ast function declaration to sast function declaration and return the function's return type and the updated translation environment *)
	(* let semantic_func_decl (env: translation_enviornment) (func_declaration:Ast.func_decl):(Ast.func_decl -> Sast.sfuncdecl * Sast.sdata_type * translation_enviornment) =  *)
		(* two types of function declarations, ones that return anytype and ones that return arrays of any type *)
		(* we declare them as type and name *)
		(* let formal_names = List.map (fun (typ, _) ->  ) func_declaration.formals in*)
		(* let new_locals = List.fold_left (fun a var -> let {kind=typ; vname=name;} = var in (typ, name) :: a) [] func_declaration.formals in *)    (*get list of just types from list of (type, string) tuples, [] is an accumulator*)
		(* let newer_locals = List.fold_left (fun a (typ, _) -> typ :: a) [] new_locals in  *)
		(*let new locals = List.map ()
		let new_locals = List.fold_left (fun a vs -> (
			get_formal_arg_types env vs):: a) [] func_declaration.formals in *)
	(*	let new_var_scope = {parent = Some(env.var_scope);
		variables = new_locals;} in
		let new_env = { var_scope = new_var_scope; fun_scope = env.fun_scope} in

		let final_env = List.fold_left (fun env stmt -> snd (
		semantic_stmt env stmt)) new_env func_declaration.body in 
		final_env *)

	let var_empty_table_init = {parent=None; variables=[]}
	let fun_empty_table_init = { functions = [];}
	let empty_environment = 
	{
		var_scope =  var_empty_table_init;
		fun_scope = fun_empty_table_init;
	}
	(* let initialize_functions env func_list =
		let (func_list, last_env) = List.fold_left (
			fun(sfunc_decl_list, env) func_list -> let 
			()) *)
	 (* let initialize_functions env function_list = 
			let (typed_functions, last_env) = List.fold_left 
			(fun (sfuncdecl_list, env) func ->  let (sfuncdecl, _,new_env) = 
			semanantic_func_decl env func in last_env) env function_list
		in last_env *)
		(* returns an environment *)
(* let return_typ = Sast.Mandalat in
let test_name = "heyy" in 
let return_stmt = Sast.Mandala({skind = return_typ; svname = test_name}) in *) 
(*let rec check_statements stmts env = match stmts
	with [] -> env (*let test_stmt = Sast.Expr in let test_typ = Sast.Void in test_stmt test_typ env*)
	| [stmt] -> let (new_stmt, typ, env) = semantic_stmt env stmt in env
	| stmt :: other_stmts ->
		let (new_stmt, typ, env) = semantic_stmt env stmt in 
		check_statements other_stmts env *)
	
	let rec separate_statements (stmts, env, update_list:Ast.stmt list * translation_enviornment * Sast.sstmt list) = match stmts 
		with [] -> (update_list, env)
		| [stmt] -> let (new_stmt, typ, new_env) = semantic_stmt env stmt in (update_list@[new_stmt], new_env)
		| stmt :: other_stmts ->
			let (new_stmt, typ, new_env) = semantic_stmt env stmt in
			(* let (nm, tp) = List.hd new_env.var_scope.variables in *)
			separate_statements (other_stmts, new_env, update_list@[new_stmt])

	(* let do_this = function
		(stmt, typ, env) -> semantic_stmt env stmt in *)

		(*somehow it is not successfully leaving this | block, and is going to the next or block instead of returning*)
	let rec semantic_check (check_program: Ast.program): (Sast.sprogram) =  
		let (prog_stmts, prog_funcs) = check_program in 
		let env = empty_environment in 
		(* need to update the environment here I think *)
		(* let (typed_tuples, new_env) = initialize_functions env prog_funcs in *)
		(* let (new_env) = initialize_functions env prog_funcs in  *)
		(* let (new_stmt, typ, env) = List.map (fun stmt_part -> semantic_stmt env stmt_part) in *) 
		(* ERROR OF GOING FROM AST TO SASST *)
		let update_list = []  in 
		let reversse_prog_stmts = List.rev prog_stmts in 
		 let resulting_statments = separate_statements (reversse_prog_stmts, env, update_list) in  (* List.map( fun stmt_part -> separate_statements prog_stmts env ) in *)
		let (statements, env) = resulting_statments 

		(* let test_results = check_statements prog_stmts env in *)
		 (*let result_tuples = List.fold_left ( fun stmt_part -> semantic_stmt env stmt_part) prog_stmts in  *)
		(* let result_tuples = prog_stmts in *)
		(* let result_stmts = *) 
		(* WORKING CODE! let result_tuples = List.map (fun stmt_part -> semantic_stmt env stmt_part) prog_stmts in 
		 let result_stmts = List.fold_left (fun a (stmt,_, ret_env) -> stmt :: a) [] result_tuples 
		in Sast.SProg(result_stmts) *)
		(* let testing = List.fold_left (do_this env) prog_stmts *)

		in Sast.SProg(statements)
		(* NEED TO ADD FUNCTION DECLARATION! *)
	(* | _ -> raise (Error("undeclared identifier")) *)
(* for function call we can check if it's drwa then check input typ *)
(* chekc if number of arguments are matching *)
(* since draw is built in function *)