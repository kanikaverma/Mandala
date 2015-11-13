open Ast

type sdata_type =
	Int
	| Float
	| Void
	| Numbert
	| Booleant
	| Shapet
	| Geot
	| Layert
	| Mandalat
	| Arrayt

type expr_wrapper =
	Expr of sexpr * sdata_type
and sexpr =
 	Literal of int
	| Float_Literal of float
	| Number of float
	| Noexpr
	| Id of svar_decl
	| Binop of expr_wrapper * op * expr_wrapper
	| Call of sfunc_decl * expr_wrapper list


and svar_decl = {
	kind : sdata_type;
	vname : string;
}

and sfunc_decl = {
	fname : string;
	returntype : sdata_type;
	formals : svar_decl list;
	body : sstmt list;
}

and sstmt =
	| Block of sstmt list
	(*| Expr of expr commented out because we are seeing if it is necessary *)
	| Assign of svar_decl * expr_wrapper
	| Return of expr_wrapper
	| IF of expr_wrapper * sstmt * sstmt
	| Foreach of expr_wrapper * expr_wrapper * sstmt
	| While of expr_wrapper * sstmt
	| Shape of svar_decl * expr_wrapper * expr_wrapper * expr_wrapper * expr_wrapper
	| Mandala of svar_decl
	| Layer of svar_decl * expr_wrapper * expr_wrapper * expr_wrapper * expr_wrapper * expr_wrapper 
	| FuncCall of expr_wrapper
type sprogram =
	sfunc_decl list
