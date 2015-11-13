open Ast
open Sast

exception Error of string

(*symbol table *)
type symbol_table={
	parent : symbol_table option;
	variables: (sdata_type*string) list 
}

type function_table={
	functions: func_decl list
} 

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

let rec semantic_expr (env:translation_enviornment):(Ast.expr -> Sast.sexpr * sdata_type) = function

	Ast.Id(vname) ->
		let vdecl = try
			find_variable env.var_scope vname
		with Not_found ->
			raise (Error("undeclared identifier"^vname))
		in 
		let (typ, name) =vdecl in 
		Sast.Id(name), typ
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


		
	(* | _ -> raise (Error("undeclared identifier")) *)
(* for function call we can check if it's drwa then check input typ *)
(* chekc if number of arguments are matching *)
(* since draw is built in function *)

