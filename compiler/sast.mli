open Ast

type sdata_type =
	Int
	| Literalt
	| Float
	| Void
	| Numbert of float 
	| Booleant
	| Shapet
	| Geot of string 
	| Layert
	| Mandalat 
	| Arrayt
	| Colort of string

(* Adding in types for mandala, layer and shapes *)
(* type sshape = SShape of string * string * float * string * float *)
(* {
	sname: string;
	sgeo : string;
	ssize : float;
	scolor: string;
	srotation: float
} *)

(* type slayer = SLayer of string * float * sshape * int * float * float *) 
(*{
	sname: string;
	sradius : float;
	sshape : sshape;
	scount : int;
	soffset : float;
	sangularshift : float
}*)

(* type smandala= SMandala of string * slayer list * float * bool *)
(* {
	sname: string;
	slist_of_layers : slayer list;
	smax_layer_radius : float; (* define the max layer radius as the maximum of the sum of the the layer radius + shape radius *)
	sis_draw: bool
} *)
(*type expr_wrapper =
	Expr of sexpr * sdata_type*)
type sexpr =
 	Literal of int
	| Float_Literal of float
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
	| Shape of svar_decl * sdata_type * sdata_type * sdata_type * sdata_type
	| Mandala of svar_decl
	| Layer of svar_decl * sexpr * sexpr * sexpr * sexpr * sexpr 
	| FuncCall of sexpr
type sfunc_decltype =
	SFunc_Decl  of sfuncdecl * sdata_type
type sprogram =
	SProg of sstmt list  * sfuncdecl list(* need to add back * sfunc_decl list *)
