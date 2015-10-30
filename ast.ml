type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type mndlt =
	| Numbert
	| Stringt
	| Booleant
	| Shapet
	| Geot
	| Layert
	| Mandalat
	| Arrayt

type expr =
	| Literal of int
	| Number of int
	| Shape of expr 
	| Geo of string 
	| Mandala of expr
	| Layer of expr
	| Arrayt of expr
	| Stringt of string 
	| Noexpr
	| Id of string
	| Assign of string * expr
	| Binop of expr * op * expr
	| Call of string * expr list

type stmt =
	| Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt
	| Foreach of expr * expr * expr * stmt
	| While of expr * stmt

type func_decl = {
	fname : string;
	returntype : mndlt;
	formals : string list;
	locals : string list;
	body : stmt list;
}
type var_decl = {
	kind : mndlt;
	vname : Stringt;
}

type program = var_decl list * func_decl list
