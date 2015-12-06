open Sast

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq


type jdata_type =
	JInt
	| JFloat
	| JVoid
	| JNumbert
	| JBooleant
	| JShapet
	| JGeot
	| JLayert
	| JMandalat
	| JArrayt
	
type jPrimative =
	| JBooleant of bool 
	| JInt of int

type jValue =
	JValue of jPrimative
	(*| JMap of string * string list * string list *) 
(*java function call of func return type and id *)
(* type java_mandala={
	name: string;
	list_of_layers : layer list;
	max_layer_radius : float; (* define the max layer radius as the maximum of the sum of the the layer radius + shape radius *)
	is_draw: bool
} *)

type shape = {
	name: string;
	geo : string;
	size : float;
	color: string;
	rotation: float
}

type layer = {
	name: string;
	radius : float;
	shape : shape;
	count : int;
	offset : float;
	angularshift : float
}

type mandala={
	name: string;
	list_of_layers : layer list;
	max_layer_radius : float; (* define the max layer radius as the maximum of the sum of the the layer radius + shape radius *)
	is_draw: bool
}

type drawing={
	mandala_list : (string * mandala) list
}

type java_shapes = {
	shape_list : shape list
}

type symbol_table = {
	draw_stmts : drawing	
}
type jFuncID = 
	JFunc of jdata_type * string
type jexpr =
	JLiteral of int
	| JFloat_Literal of float
	| JId of string
	| JBinop of jexpr * op * jexpr
	| JCall of string * jexpr list
	| JLayer of string * layer
	| JMandala of string * mandala 
and jIF = 
	JavaIF of jexpr * javaBlock
and jStmt =
	JStmt of jexpr
and javaBlock =
	JavaBlock of jStmt list
and javaFuncCall =
	JFuncCall of jdata_type * string

(* type javaFunctionHeader =
	JavaFunctionHeader of javaFunctionType *string *)
type javaMethod =
	JavaMain of javaBlock
	| JavaDefaultConstructor of string * javaBlock
	| JavaFunction of javaFuncCall * javaBlock
	(* JavaFunction of Ast.functionHeader * javaBlock *)
	(* type functionHeader = {
returnType : platoFunctionType;
functionName : string;
parameters : parameter list;
}) *)

type javaClass = 
	JavaClass of javaMethod (* string * string * javaMethod *)

type javaprogram = 
	JavaProgram of javaClass list