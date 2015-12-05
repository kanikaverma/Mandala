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

(*type expr_wrapper =
	Expr of sexpr * sdata_type*)
type sexpr =
 	Literal of int
	| Float_Literal of float
	| Number of float
	| Noexpr
	| Id of string
	| Binop of sexpr * op * sexpr
	| Call of string * sexpr list


and svar_decl = {

	skind : sdata_type;
	svname : string;
	
}

and sfuncdecl = {
	sfname : string;
	sreturntype : sdata_type;
	sformals : svar_decl list;
	sbody : sstmt list;
}

and sstmt =
	| Block of sstmt list
	| Assign of svar_decl * sexpr
	| Expr of sexpr
	| Return of sexpr
	| IF of sexpr * sstmt * sstmt
	| Foreach of sexpr * sexpr * sstmt
	| While of sexpr * sstmt
	| Shape of svar_decl * sexpr * sexpr * sexpr * sexpr
	| Mandala of svar_decl
	| Layer of svar_decl * sexpr * sexpr * sexpr * sexpr * sexpr 
	| FuncCall of sexpr
type sfunc_decltype =
	SFunc_Decl  of sfuncdecl * sdata_type
type sprogram =
	SProg of sstmt list (* need to add back * sfunc_decl list *)
