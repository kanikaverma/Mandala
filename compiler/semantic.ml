open Ast
open Sast

(*symbol table *)
type symbol_table={
	parent : symbol_table option;
	variables: variable_decl list
}

type function_table={
	functions: function_decl list
} 

(*envioronment*)
type translation_enviornment ={
	var_scope: symbol_table;
	fun_scope: function_table;
}

