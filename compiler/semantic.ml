open Ast
open Sast

exception Error of string

(* In semantic just want to check that statments have the correct types *)
(* In our symbol table just want to store the type of the variable, the variable name and the value if it is a float, int or geo of string, 
otherwise it is just an sdata_type like Sast.SMandala for the value *)
(* The value itself of that variable is passed along in our statement list and handled in sast_to_jast *)
(* For example, when we are doing Number n = 5; we want to check that 5 is of type Number *)
(* We only care about checking the types here and the actual value of 5 is passed along in the list of Sast.sstmt's *)
(* When we define a Mandala, we want to store the type of Mandala, we can just store it as name_of_mandala, Sast.Mandalat, Sast.SMandala *)
(* When declaring variables they have to be done in the form Number n = 5; cannot just declare a variable and you cannot just assign a value
to a variable *)
(* When reassigning a value to a variable, you must also write the type fo the variable, like Number n = 7; *)
(* When you encounter an assignment, check if that variable has already been declared in the symbol table, if yes, just update the value in
the table, otherwise add the variable to the symbol table *)
(* When updating the symbol table, look for the triplet, and match on the variable name, if it exists, modify it to include that new value
and then add it back in, if it is new just add it *)
(* To modify the element you can search using List.filter *)

(*symbol table *)
type symbol_table={
	parent : symbol_table option;
	variables: (string * smndlt (** sdata_type*) ) list (* adding the name, type *)
}

type function_table={
	functions: (string * smndlt * svar_decl list * sstmt list) list
} 

(*envioronment*)
type translation_environment ={
	(* return_type: datatype; fnctions return type 
		return_seen: bool does the function have a  return statemtn
	location: string;  using for global or local checking 
	global_scope: symbol_table; *)
	var_scope: symbol_table;
	fun_scope: function_table;
}
(* scope is env.var_scope *)
(* scope.variabkles is a list of (string * sdata_type) *)
(* returns the name, type and value *)
let find_variable (scope: symbol_table) name=
	try
		List.find (fun (s,_) -> s=name) scope.variables
 
	(* with Not_found -> try List.find (fun (s, _) -> s=name) scope.parent.variables*) 
	with Not_found -> raise (Error ("Semantic not finding variable in lookup table"^name))
		(*	Some(parent)-> find_variable parent name
		| _ -> raise (Error("THIS IS NOT FOUND "^name)) *)

let rec find_function (scope: function_table) name =
	try
		List.find (fun (s, _, _, _) -> s=name) scope.functions
	with Not_found ->
		raise (Error("adding function to func_table FAILED! "^name))
(* PROBABLY WON'T USE
Part of comment
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

(* TODO: check to make sure it doesn't already exist before adding  *)
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


(* TODO: see why functions are using sdata_type, maybe they shouldn't be using it? are they using it wrong. sdata_type should be a value *)
 let add_to_func_table env sfunc_decl =
	let func_table = env.fun_scope in 
	let old_functions = func_table.functions in 
			let func_name = sfunc_decl.sfname in 
			let func_type = sfunc_decl.sreturntype in (*may need to call function on sreturntypes to get the type *)
			let func_formals = sfunc_decl.sformals in 
			let func_body = sfunc_decl.sbody in 
			let new_functions = (func_name, func_type, func_formals, func_body)::old_functions
		in 
			let new_fun_scope = {functions = new_functions} in
			let final_env = {env with fun_scope = new_fun_scope} in 
			final_env 

let rec find_function (scope: function_table) name=
		List.find (fun (s, _, _, _) -> s = name) scope.functions
	(*with Not_found -> raise Not_found*)
let rec extract_type (scope: function_table) name = function
	(smndlt, string) -> (smndlt)
let get_formal_arg_types env = function
	(smndlt, string) -> (smndlt)
(* let eval_math
let eval_conditionals *)
(* SBinop ( sexpr , binop , sexp r2 , datatype) −> Binop ( gen_tb_expr
sexp r , binop , gen_tv_expr sexpr2 )*)

let rec semantic_expr (env:translation_environment):(Ast.expr -> Sast.sexpr * smndlt  * translation_environment) = function

	Ast.Id(vname) ->
		if (vname="circle" || vname="triangle" || vname="square")
			(* Check for built-in Ids for shapes like circle, triangle, and square *)
			then
			let geo_typ = Sast.Geot in 
			let name = vname in 
			(Sast.Id(name), geo_typ, env)
		
		else if (vname="yellow" || vname="green" || vname="blue" || vname="red" || vname="black" || vname="blue")
			(* TODO: @Edo INSTEAD MAKE DICTIONARY OF COLORS AND CHECK ALL COLORS *)
			then let color_typ = Sast.Colort in 
			let name = vname in 
			(Sast.Id(name), color_typ, env)
		else
		let vdecl = try
			find_variable env.var_scope vname
		with Not_found ->
			raise (Error("undeclared identifier: "^vname))
			(* Want to add the symbol to our symbol table *)
		in 
		let (name, typ (*, val*)) =vdecl in 
		(Sast.Id(name), typ, (*val,*) env)
	
		(* AST Call of string * expr list*)
	| Ast.Float_Literal(num) ->
		(Sast.Float_Literal(num), Sast.Numbert, (*Sast.SNumber(num),*) env)
	| Ast.Literal(num) ->
		(Sast.Literal(num), Sast.Integert, env)
	| Ast.Binop(term1, operator, term2) ->
		(* convert to Sast.Binop *)
		(* raise (Error("Tried to use binary operator here!")) *)
		(* evaluate term1 and evaluate term 2 *)
		(* and use the opertaor *)
		(* this will also check if the term, if it is a variable has been defined *)
		(* TODO: possibly add chekc to make sure terms are of type Float_Literal *)
		let (eval_term1, typ1, new_env) = semantic_expr env term1 in 
		let (eval_term2, typ2, new_env) = semantic_expr env term2 in 
		(* now translate Ast.operator to Sast.operator *)
		if not (typ1 = typ2)
		then raise (Error("Mismatched types, invalid operation"))
		else 
			(* Checking the types for binary operators and will do evaluation of binop in sast_to_jast *)
			(Sast.Binop(eval_term1, operator, eval_term2), typ1, env)

			(* Check what the operator is *)
			(* let opertaor_typ = match operator 
				with Add -> eval_math typ1 typ2 
				| Sub -> eval_math typ1 typ2 
				| Mult -> eval_math typ1 typ2 
				| Div -> eval_math typ1 typ2
				| Equal -> eval_conditionals typ1 typ2
				| Neq -> eval_conditionals typ1 typ2 
				| Less -> eval_conditionals typ1 typ2 
				| Leq -> eval_conditionals typ1 typ2 
				| Greater -> eval_conditionals typ1 typ2
				| Geq -> eval_conditionals typ1 typ2  *)



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
			then (Sast.Call(fid, actual_expr_list), Sast.Voidt, env)
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

let proc_type = function
  	 Ast.Booleant -> Sast.Booleant
  	| Ast.Shapet -> Sast.Shapet
	| Ast.Layert -> Sast.Layert
	| Ast.Mandalat -> Sast.Mandalat
	| Ast.Arrayt -> Sast.Arrayt
	| Ast.Numbert -> Sast.Numbert

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

let rec proc_formals (var_decl_list, env, update_var_decl_list: Ast.var_decl list * translation_environment * Sast.svar_decl list) = match var_decl_list
	with [] -> (update_var_decl_list, env)
	| [var_decl] -> let (new_var_decl, new_env) = proc_var_decl(var_decl, env) in (update_var_decl_list@[new_var_decl], new_env)
	| var_decl :: other_var_decls ->
		let (new_var_decl, new_env) = proc_var_decl(var_decl, env) in
		proc_formals (other_var_decls, new_env, update_var_decl_list@[new_var_decl])


let rec semantic_stmt (env:translation_environment):(Ast.stmt -> Sast.sstmt * smndlt * translation_environment) = function
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
		let new_env = add_to_var_table (env, name, typ) in
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
		let new_env = add_to_var_table (env, name, typ) in 
		(Sast.Layer({skind = typ; svname = name;}, s_radius, s_shape, s_count, s_offset, s_angular_shift), typ, new_env)

	(* IN AST Layer of var_decl * expr * expr * expr * expr * expr  *)
		(* IN SASST: | Shape of svar_decl * sexpr * sexpr * sexpr * sexpr *)

	| Ast.Shape(v_name, v_geo, v_size, v_color, v_rotation) ->
		(* raise (Error ("ADD CODE FOR SHAPE CREATION!")) *)
		
		let {vname=name} = v_name in 
		let typ = Sast.Shapet in 
		let s_geo = match v_geo with 
			(*TODO add check for ast.id, circle triangle and square can be built in ids *)
			Ast.Id(v_geo) -> let new_geo = v_geo in new_geo (* semantic_expr env v_geo in *)
			| _ -> raise (Error("WRONG FORMAT FOR GEO IN SHAPE!"))
		in 
		let updated_s_geo = Sast.SGeo(s_geo) in 
		(* 
			Sast.sexpr * smndlt  * translation_environment
		*)
		let (size_stmt, typ, env) = semantic_expr env v_size in 
		(* Checking that the shape's size is a float and returning a sexpr *)

		let size_value = match typ with
			Sast.Numbert -> size_stmt
			| _ -> raise (Error ("Size wasn't a numbert!"))

		(*let check_size = match size_stmt with
			Sast.Float_Literal(v_size) -> let update_sast_type = Sast.SNumber(v_size) in update_sast_type
			|Sast.Id(var) -> 
				let vdecl = try 

			| _  -> raise(Error("Invalid radius, needs to be of type of SNumber"))*)
		in 
		(* let (s_color, s_c_typ, env) = semantic_expr env v_color in *)
		let s_color = match v_color with 
			Ast.Id(v_color) -> let new_color = v_color in new_color
			| _ -> raise (Error("WRONG FORMAT FOR COLOR IN SHAPE!"))
		in 
		let updated_s_color = Sast.SColor(s_color) in 

		(* 
			Sast.sexpr * smndlt  * translation_environment
		*)
		let (rotation_stmt, typ, env) = semantic_expr env v_rotation in (*
		(* Check the rotation type and make sure it is a float *)
		let check_rotation = match rotation_stmt with 
			Sast.Float_Literal(v_rotation) -> let updated_rotation_type = Sast.SNumber(v_rotation) in updated_rotation_type
			| _ -> raise(Error("Invalid rotation, needs to be a float number. "))*)

		let rotation_value = match typ with
			Sast.Numbert -> rotation_stmt
			| _ -> raise (Error ("Rotation wasn't a numbert!"))
		in 

		let new_env = add_to_var_table (env, name, typ) in 
		(* 	| Shape of svar_decl * sdata_type * sdata_type * sexpr * sdata_type *)
		(Sast.Shape({skind = typ; svname=name;}, updated_s_geo, size_value, updated_s_color, rotation_value), typ, new_env)
	(* IN AST Shape of var_decl * expr * expr * expr * expr *)
	(* IN SAST: | Layer of svar_decl * sexpr * sexpr * sexpr * sexpr * sexpr  *)

	| Ast.Expr(expression) -> 
		let newExpr = try
			semantic_expr env expression 
		with Not_found ->
			raise (Error("undefined expression"))  			(*this error should be fixed to something more relevant*)
		in let (x, typ, ret_env)= newExpr in 
		(Sast.Expr(x), typ, env)


	| Ast.Return(x) -> 
			let newExpr = try
				semantic_expr env x 
			with Not_found ->
				raise (Error("Invalid return value"))  			(*this error should be fixed to something more relevant*)
			in let (x, typ, ret_env)= newExpr in 
			(Sast.Return(x), typ, env)

	(*Assign is of form var_decl*expr  *)
	| Ast.Assign(lefthand, righthand) ->
		(* TODO *)
		let right_assign =
			semantic_expr env righthand
		(* semantic_expr returns Sast.sexpr * smndlt * sdata_type * translation_enviornment *)
		in let (assign_val, typ, ret_env) = right_assign in
		let {kind=typ2; vname=name2} = lefthand 
		(*let lefthand {kind = typ2; vname = name2} = x *)
		(*let (typ2, name2) =(lefthand.kind, lefthand.vname) *)

		in match typ with (*Assign of svar_decl * sexpr*)
			 typ2 -> let new_env = add_to_var_table (env, name2, typ2) 
				in (Sast.Assign(({skind = typ2; svname = name2}), assign_val), typ, new_env) (* check strctural equality *)
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
	
	let rec separate_statements (stmts, env, update_list:Ast.stmt list * translation_environment * Sast.sstmt list) = match stmts 
		with [] -> (update_list, env)
		| [stmt] -> let (new_stmt, typ, new_env) = semantic_stmt env stmt in (update_list@[new_stmt], new_env)
		| stmt :: other_stmts ->
			let (new_stmt, typ, new_env) = semantic_stmt env stmt in
			separate_statements (other_stmts, new_env, update_list@[new_stmt])

	(*let findReturnStmt stmtList =
		try 
			List.find (fun(x)-> match x with Sast.Return(y)) stmtList
		with Not_found -> raise (Error ("No return statement in user defined function")) *)

	(*NEED TO CREATE NEW ENVIRONMENT WHILE PROCESSING BODY STATEMENTS*)
	let rec semantic_func (env: translation_environment): (Ast.func_decl -> Sast.sfuncdecl * translation_environment) = function
		my_func ->
		let fname = my_func.fname in
		let returntype = my_func.returntype in 
		let formals = my_func.formals in 
		let body = my_func.body in

		(* need to:
			process formals, while doing this, create a new enviornment w/ inputs filled in that will be used to typecheck the statements
			typecheck all the statements, get back an Sast stmt list 
			the last statement in this stmt list will be the return stmtm
				as part of this, make sure the function is returning the right thing, and create the sast version of the return type
		*)

		let empty_list = [] in
		let new_returntype = proc_type returntype in
		let func_env=
			{
				var_scope = {parent = env.var_scope.parent; variables=[]};
				fun_scope = fun_empty_table_init;
			} in 
		(*gets list of formals in sast format, fills the func_env with the inputs in the var table*)
		let (new_formals, func_env) = proc_formals (formals, func_env, empty_list) in
		(*walks through body of function, checking types etc.*)
		let (new_stmts, func_env) = separate_statements(body, func_env, empty_list) in
		(*check that function returned the right thing-- get the return stmt from stmt list, check its typ against returntyp*)
		(*let rettyp = findReturnStmt new_stmts in *)
		(*CHECK IF rettyp is same as new_returntype*)

		let sfuncdecl = {
			sfname = fname;
			sreturntype = new_returntype;
			sformals = new_formals;
			sbody = new_stmts;
		} in

		let env = add_to_func_table env sfuncdecl in

		(sfuncdecl, env)


	let rec separate_functions (functions, env, update_list: Ast.func_decl list * translation_environment * Sast.sfuncdecl list) = match functions 
		with [] -> (update_list, env)
		(*Create an empty environment and save the new one??*)
		| [func] -> 
		(* Change in some way to handle scope*)
			(*let scoped_environment = {
				var_scope = {parent = env.var_scope.parent; variables=[]};
				fun_scope = fun_empty_table_init;
			} in *) 
			let (new_func, new_env) = semantic_func (*scoped_environment*) env func in (update_list@[new_func], new_env)
		| func :: other_funcs ->
			let (new_func, new_env) = semantic_func env func in
			(* let (nm, tp) = List.hd new_env.var_scope.variables in *)
			separate_functions (other_funcs, new_env, update_list@[new_func])

		(*somehow it is not successfully leaving this | block, and is going to the next or block instead of returning*)
	let rec semantic_check (check_program: Ast.program): (Sast.sprogram) =  
		let (prog_stmts, prog_funcs) = check_program in 
		let env = empty_environment in 
		(* need to update the environment here I think *)
		let empty_list = []  in 
		let reverse_prog_stmts = List.rev prog_stmts in 
		let (resulting_functions, func_env) = separate_functions (prog_funcs, env, empty_list) in
		let (statements, env) = separate_statements (reverse_prog_stmts, env, empty_list) in  (* List.map( fun stmt_part -> separate_statements prog_stmts env ) in *)
		(*Using the scope of the env returning from statements..?*)

		Sast.SProg(statements, resulting_functions)
		(* NEED TO ADD FUNCTION DECLARATION! *)
