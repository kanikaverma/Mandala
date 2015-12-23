open Sast

(* Operators for jast *)
type op = Add | Sub | Mult | Div 

(* Mandala specific types for java ast *)
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

(* Create shape to store attributes of shape *)
type shape = {
	name: string;
	geo : string;
	size : float;
	color: string;
	rotation: float
}
(* Create layer to define shape drawn in layer *)
and layer = {
	name: string;
	radius : float;
	shape : shape;
	count : int;
	offset : float;
	angularshift : int
}

(* Create mandala to store list of layers *)
and mandala={
	name: string;
	list_of_layers : layer list;
	max_layer_radius : float;
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

(* Defines orientation of the shapes *)	
type jShape = 
	Circle of float * float * float * string
	| Square of float * float * float * float * string
	| Triangle of float * float * float * float * string

(* drawing stores information about figures we will draw *)
type drawing={
	mandala_list : (string * mandala) list; 	(* figures to be drawn *)
	variables: (string * jdata_type) list; 		(* store variables and type *)
	java_shapes_list: jShape list; 				(* store shapes coordinates *)
}

type java_shapes = {
	shape_list : shape list
}
(* Our environment stores a drawing *)
type symbol_table = {
	draw_stmts : drawing	
}

type javaClass = CreateClass of string 

type javaprogram = 
	JavaProgram of javaClass * jShape list