open Ast

(* Mandala specific data types *)
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

(* Stores the values and types *)
type sdata_val =
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
	| Id of string
	| Binop of sexpr * op * sexpr
	| Call of string * sexpr list

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
	| Assign of svar_decl * sexpr
	| Expr of sexpr
	| Return of sexpr
	| Foreach of sexpr * sexpr * sexpr * sstmt list
	| Shape of svar_decl * sdata_val * sexpr * sdata_val * sexpr
	| Mandala of svar_decl
	| Layer of svar_decl * sexpr * sexpr * sexpr * sexpr * sexpr 

type sfunc_decltype =
	SFunc_Decl  of sfuncdecl * smndlt

type sprogram =
	SProg of sstmt list  * sfuncdecl list