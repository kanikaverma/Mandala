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

let rec expr (env:translation_enviornment):(Ast.expr -> Sast.sexpr * sdata_type) = function

	Ast.Id(vname) ->
		let vdecl = try
			find_variable env.var_scope vname
		with Not_found ->
			raise (Error("undeclared identifier"^vname))
		in 
		let (typ, name) =vdecl in 
		Sast.Id(name), typ




