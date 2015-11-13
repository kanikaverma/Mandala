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
	| Call of sfunc_decl * sexpr list


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
	| Assign of svar_decl * sexpr
	| Return of sexpr
	| IF of sexpr * sstmt * sstmt
	| Foreach of sexpr * sexpr * sstmt
	| While of sexpr * sstmt
	| Shape of svar_decl * sexpr * sexpr * sexpr * sexpr
	| Mandala of svar_decl
	| Layer of svar_decl * sexpr * sexpr * sexpr * sexpr * sexpr 
	| FuncCall of sexpr
type sprogram =
	sfunc_decl list
