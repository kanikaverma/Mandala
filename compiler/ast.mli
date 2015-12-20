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
	| Color of string
	| Number of float
	| Geo of string 


 type value =
	ExprVal of expr 
	| ArrVal of expr list 

type var_decl = {
	kind : mndlt;
	vname : string;
}
 (* type var_decl_assign = 
	VarDeclAssign of var_decl * value  *)

type stmt =
	| Block of stmt list
	| Expr of expr 
	| Assign of var_decl * expr
	| ArrAssign of var_decl * expr list
	| Return of expr
	| IF of expr * stmt * stmt
	| Foreach of string * float * float * stmt list
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
