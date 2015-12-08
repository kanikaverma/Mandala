type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type mndlt =
	| Numbert
	| Booleant
	| Shapet
	| Geot
	| Layert
	| Mandalat
	| Arrayt
	| Colort

type expr =
	 Literal of int
	| Float_Literal of float
	| Noexpr
	| Id of string
	| Binop of expr * op * expr
	| Call of string * expr list

type var_decl = {
	kind : mndlt;
	vname : string;
}

type stmt =
	| Block of stmt list
	| Expr of expr (* commented out because we are seeing if it is necessary *)
	| Assign of var_decl * expr
	| ArrAssign of var_decl * expr list
	| Return of expr
	| IF of expr * stmt * stmt
	| Foreach of string * int * int * stmt list
	| While of expr * stmt
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
