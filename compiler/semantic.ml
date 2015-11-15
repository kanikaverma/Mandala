open Ast
open Sast

exception Error of string

(*symbol table *)
type symbol_table={
	parent : symbol_table option;
	variables: (sdata_type*string) list 
}

type function_table={
	functions: (string * sdata_type * svar_decl list * sexpr list) list
} 
(* et (fname, fret, fargs, fbody)*)
(* want to store the function name and the function arguments *)
(*
type func_decl = {
	fname : string;
	returntype : mndlt;
	formals : var_decl list;
	body : stmt list;
}*)
(*
and sfunc_decl = {
	fname : string;
	returntype : sdata_type;
	formals : svar_decl list;
	body : sstmt list;
}
*)
(*functions: (string * mndlt * var_decl list * stmt list) list*)
(*envioronment*)
type translation_enviornment ={
	var_scope: symbol_table;
	fun_scope: function_table;
}

let rec find_variable (scope: symbol_table) name=
	try
		List.find (fun (_,s) -> s = name) scope.variables
	with Not_found ->
		match scope.parent with
			Some(parent)-> find_variable parent name
		| _ -> raise Not_found

let rec find_function (scope: function_table) name=
		List.find (fun (s, _, _, _) -> s = name) scope.functions
	(*with Not_found -> raise Not_found*)
let rec extract_type (scope: function_table) name = function
	(sdata_type, string) -> (sdata_type)
let get_formal_arg_types env = function
	(sdata_type, string) -> (sdata_type)

let rec semantic_expr (env:translation_enviornment):(Ast.expr -> Sast.sexpr * sdata_type) = function

	Ast.Id(vname) ->
		let vdecl = try
			find_variable env.var_scope vname
		with Not_found ->
			raise (Error("undeclared identifier"^vname))
		in 
		let (typ, name) =vdecl in 
		Sast.Id(name), typ
		(* AST Call of string * expr list*)
	| Ast.Call(fid, args) ->
		try (let (fname, fret, fargs, fbody) =
			find_function env.fun_scope fid in
			let actual_types = List.map (fun expr -> semantic_expr env expr) args in
			(*let actual_type_names = List.iter extract_type actual_types*)
			let actual_types_list = List.fold_left (fun a (_,typ) -> typ :: a) [] actual_types    (*get list of just types from list of (type, string) tuples, [] is an accumulator*)

			
			(*let actual_type_names = 
				List.find (fun (_,s) -> s) actual_types*)

			(*let (actual_arg_type, _) = actual_types in*)
		in 
			let formal_types =  List.map (fun farg -> let arg_type =
				get_formal_arg_types env (farg.skind, farg.svname) in arg_type)
			fargs in
			if not (actual_types_list=formal_types) 
			then
				raise (Error("Mismatching types in function call"))
			else 
				let actual_expr_list = List.fold_left (fun a (expr,_) -> expr :: a) [] actual_types in
				Sast.Call(fname, actual_expr_list), fret
				(* Call of string * sexpr list*)

		)
		with Not_found ->
			raise (Error("undeclared function "))
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

let rec semantic_stmt (env:translation_enviornment):(Ast.stmt -> Sast.sstmt * sdata_type) = function
	Ast.Mandala(mandala_arg) ->
		let stmt_decl = try
			find_variable env.var_scope mandala_arg.vname
		with Not_found ->
			raise (Error("undeclared identifier"^mandala_arg.vname))
		in 
		let (typ, name) = stmt_decl in
		Sast.Mandala({skind = typ; svname = name}), typ
	| Ast.Assign(lefthand, righthand) ->
		let right_assign =
			semantic_expr env righthand
		in let (assign_val, typ) = right_assign in
		let (typ2, name2) =(lefthand.kind, lefthand.vname) 

		in match typ with (*Assign of svar_decl * sexpr*)
			 typ2 -> Sast.Assign(({skind = typ2; svname = name2}), assign_val), typ (* check strctural equality *)
			(* | _ -> raise (Error("it didn't work")) *)

	| Ast.Expr(expression) -> let newExpr = semantic_expr env expression 
		in let (x, typ)= newExpr in 
		Sast.Expr(x), typ
		
	(* | _ -> raise (Error("undeclared identifier")) *)
(* for function call we can check if it's drwa then check input typ *)
(* chekc if number of arguments are matching *)
(* since draw is built in function *)

