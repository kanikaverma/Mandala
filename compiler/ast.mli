type op = Add | Sub | Mult | Div 

(* Mandala varaible types. *)
type mndlt =
	| Numbert
	| Booleant
	| Shapet
	| Geot
	| Layert
	| Mandalat
	| Arrayt
	| Colort
	| Voidt

type expr =
	 Literal of int
	| Float_Literal of float
	| Id of string
	| Binop of expr * op * expr
	| Call of string * expr list

type var_decl = {
	kind : mndlt;
	vname : string;
}

type stmt =
	| Expr of expr 
	| Assign of var_decl * expr
	| Return of expr
	| Foreach of string * float * float * stmt list
	| Shape of var_decl * expr * expr * expr * expr
	| Mandala of var_decl
	| Layer of var_decl * expr * expr * expr * expr * expr 

type func_decl = {
	fname : string;
	returntype : mndlt;
	formals : var_decl list;
	body : stmt list;
}

type program = stmt list * func_decl list