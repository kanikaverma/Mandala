open Ast

type smndlt =
	| Numbert
	| Booleant
	| Shapet
	| Geot
	| Layert
	| Mandalat
	| Arrayt
	| Colort
	| Integert
	| Voidt
	| Loopt

(* RENAME SDATA_TYPE to svalue because it is really the VALUES!!!!! *)
type sdata_type =
	SInt
	| SLiteral
	| SFloat
	| SVoid
	| SNumber of float 
	| SBoolean of int 
	| SShape
	| SGeo of string 
	| SLayer
	| SMandala 
	| SArray
	| SColor of string

type sexpr =
 	Literal of int
	| Float_Literal of float
	| Noexpr
	| Id of string
	| Binop of sexpr * op * sexpr
	| Call of string * sexpr list

(*Formal variable has type and name*)
and svar_decl = {

	skind : smndlt;
	svname : string;
}

and sfuncdecl = {
	sfname : string;
	sreturntype : smndlt;
	sformals : svar_decl list;
	sbody : sstmt list;
}

and sstmt =
	| Block of sstmt list
	| Assign of svar_decl * sexpr
	| Expr of sexpr
	| Return of sexpr
	| IF of sexpr * sstmt * sstmt
	| Foreach of sexpr * sexpr * sexpr * sstmt list
	| While of sexpr * sstmt
	| Shape of svar_decl * sdata_type * sexpr * sdata_type * sexpr
	| Mandala of svar_decl
	| Layer of svar_decl * sexpr * sexpr * sexpr * sexpr * sexpr 
	| FuncCall of sexpr

type sfunc_decltype =
	SFunc_Decl  of sfuncdecl * smndlt(** sdata_type*)

type sprogram =
	SProg of sstmt list  * sfuncdecl list(* need to add back * sfunc_decl list *)