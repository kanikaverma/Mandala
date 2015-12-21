open Sast

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
type jmndlt =
	| Numbert
	| Booleant
	| Shapet
	| Geot
	| Layert
	| Mandalat
	| Arrayt
	| Colort

type jPrimative =
	| JBooleant of bool 
	| JInt of int

type jValue =
	JValue of jPrimative

type shape = {
	name: string;
	geo : string;
	size : float;
	color: string;
	rotation: float
}

and layer = {
	name: string;
	radius : float;
	shape : shape;
	count : float;
	offset : float;
	angularshift : int
}

and mandala={
	name: string;
	list_of_layers : layer list;
	max_layer_radius : float; (* define the max layer radius as the maximum of the sum of the the layer radius + shape radius *)
	is_draw: bool
}

and jdata_type =
	JInt of int 
	| JVoid
	| JNumbert of float
	| JBooleant of int 
	| JShapet of shape 
	| JGeot of string 
	| JLayert of layer
	| JMandalat of mandala
	| JArrayt
	| JColort of string
type jShape = 
	(*size, x, y, color*)
	Circle of float * float * float * string
		(*size, x, y, rotation, color*)
	| Square of float * float * float * float * string
		(*size, x, y, rotation, color*)
	| Triangle of float * float * float * float * string

type drawing={
	mandala_list : (string * mandala) list;
	variables: (string * jdata_type) list;
	java_shapes_list: jShape list;
}

type java_shapes = {
	shape_list : shape list
}

type symbol_table = {
	draw_stmts : drawing	
}

(* type jFuncID = 
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
	JFuncCall of jdata_type * string *)

(* type javaFunctionHeader =
	JavaFunctionHeader of javaFunctionType *string *)
(* type javaMethod =
	JavaMain of javaBlock
	| JavaDefaultConstructor of string * javaBlock
	| JavaFunction of javaFuncCall * javaBlock *)
	(* JavaFunction of Ast.functionHeader * javaBlock *)
	(* type functionHeader = {
returnType : platoFunctionType;
functionName : string;
parameters : parameter list;
}) *)

type javaClass = CreateClass of string
	(*JavaClass of javaMethod (* string * string * javaMethod *)
	 type javaClassName =*) 

type javaprogram = 
	JavaProgram of javaClass * jShape list