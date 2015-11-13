open Ast
open Sast

type symbol_table={
	parent : symbol_table option;
	variables: variable_decl list
}

