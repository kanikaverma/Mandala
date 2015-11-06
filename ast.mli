type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type mndlt =
	| Numbert
	| Booleant
	| Shapet
	| Geot
	| Layert
	| Mandalat
	| Arrayt

type expr =
	 Literal of float
	| Number of float
	
	| Noexpr
	| Id of string
	| Binop of expr * op * expr
	| Call of string * expr list



type stmt =
	| Vdecl of var_decl
	| Block of stmt list
	| Expr of expr
	| Assign of expr * expr
	| Return of expr
	| Init of vdecl * expr 
	| If of expr * stmt * stmt
	| Foreach of expr * expr * expr * stmt
	| While of expr * stmt
	| Shape of expr * expr * expr * expr * expr
	| Mandala of expr
	| Layer of expr * expr * expr * expr * expr * expr 
	| ArrAssign of expr * expr
	

(*int[] x = [1.,2.,3.,4.,5.,6.]*)

type var_decl = {
	kind : mndlt;
	vname : string;
	value: int list;
}
type func_decl = {
	fname : string;
	returntype : mndlt;
	formals : var_decl list;
	locals : var_decl list;
	body : stmt list;
	returnvar : string;
}

type program = var_decl list * func_decl list
