open Ast
open Sast
open Sast_to_jast
open Jast
open Semantic
open Lexing

exception Error of string

let jast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	let sast = Semantic.semantic_check ast in
	Sast_to_jast.actual_final_convert sast

(*let proc_expr =function
	Sast.Call(fname, actual_args)->
		print_string "    t.setPosition(0, 0);\n";      (*should be in sast.mandala case*)
		print_string "    t.dot();\n"      (*should be in sast.mandala case*)
let proc_stmt = function
	Sast.Mandala(vname) ->
		(*print java code for mandala of this name*)
		print_string "    Turtle t = new Turtle();\n";    (*should be in sast.mandala case*)
		print_string "    t.hide();\n"   (*should be in sast.mandala case*)
	| Sast.Expr(expression)->
		proc_expr expression
	| _ -> raise (Error("unsupported statement found")) 
let gen_java = function
	Sast.SProg(s)-> 
		let x = List.length s in
		if (x>0) then (
			print_string "public class Program {\n\n";
 			print_string "  public static void main(String[] args) {\n\n";
		
			List.map proc_stmt s; 
			print_string "	t.save(\"Program.jpg\");";
			print_string "  }\n\n}"
		)	
		else print_string "No input provided" *)

let draw_circle = function
	(radius, x, y, color) -> 
		print_string "    drawCircle(t,";
		print_float radius;
		print_string ",";
		print_float x;
		print_string ",";
		print_float y;
    print_string ",";
    print_string "\"";
    print_string color;
    print_string "\"";
		print_string ");\n"

let draw_square = function
	(side, x, y, rotation, color) ->
		print_string "    drawSquare(t,";
		print_float side;
		print_string ",";
		print_float x;
		print_string ",";
		print_float y;
		print_string ",";
		print_float rotation;
    print_string ",";
    print_string "\"";
    print_string color;
    print_string "\"";
		print_string ");\n"

let draw_triangle = function
  (side, x, y, rotation, color) ->
    print_string "    drawTriangle(t,";
    print_float side;
    print_string ",";
    print_float x;
    print_string ",";
    print_float y;
    print_string ",";
    print_float rotation;
    print_string ",";
    print_string "\"";
    print_string color;
    print_string "\"";
    print_string ");\n"

let proc_shape = function
	Jast.Circle(radius,x,y,color) ->
		draw_circle(radius,x,y,color)
	| Jast.Square(side,x,y,rotation,color) ->
		draw_square(side,x,y,rotation,color)
  | Jast.Triangle(side,x,y,rotation,color) ->
    draw_triangle(side,x,y,rotation,color)
	|  _ -> raise (Error("other shapes unsupported"))

let define_methods = function
	x -> if (x> 0) then (
			(* CIRCLES *)
			print_string "public static void drawCircle(Turtle t, double radius, double x, double y, String color) {\n";
      print_string "   t.penColor(color);\n";
      print_string "   t.up(); t.setPosition(x , y + radius); t.down();\n";
    	print_string "		for (int i = 0; i < 360; i++) {\n";
    	print_string "			t.forward(radius * 2 * Math.PI / 360);\n";
    	print_string "			t.right(1);\n" ;
    	print_string "       }\n}\n";

    	(* SQUARES *)
    	print_string "public static void drawSquare(Turtle t, double size, double x, double y, double rotation, String color) {\n";
    	print_string "    t.penColor(color);\n";
      print_string "	  t.up();\n";
    	print_string "    t.setPosition(x - size/2, y + size/2);\n";
    	print_string "    rotation = rotation % 90;\n";
    	print_string "    double radius = Math.sqrt(2) * size / 2;\n";
    	print_string "    if (rotation > 0 ) t.left(45);\n";
      print_string "	  for (int i = 0; i < rotation; i++) {\n";
    	print_string "			t.forward(radius * 2 * Math.PI / 360);\n";
    	print_string "			t.right(1);\n" ;
    	print_string "       }\n";
    	print_string "	  t.down();\n";
    	print_string "    if (rotation > 0) t.right(45);\n";
    	print_string "    int turn = 90;\n";
			print_string "    t.forward( size ); t.right( turn );\n";
      print_string "    t.forward( size ); t.right( turn );\n";
      print_string "    t.forward( size ); t.right( turn );\n";
      print_string "    t.forward( size ); t.right( turn );\n";
			print_string "    t.left( rotation );\n";
			print_string "}\n";

			(* TRIANGLES *)
			print_string "public static void drawTriangle(Turtle t, double size, double x, double y, double rotation, String color) {\n";
      print_string "    t.penColor(color);\n";
      print_string "    t.up(); t.setPosition(x - size/2, y + Math.sqrt(3)*size/6);\n";
      print_string "    rotation = rotation % 120;\n";      
      print_string "    double radius = size / Math.sqrt(3);\n";
      print_string "    if (rotation > 0) t.left(60);\n";
      print_string "    for (int i = 0; i < rotation; i++) {\n";
      print_string "      t.forward(radius*2*Math.PI / 360); t.right(1);\n";
      print_string "    }\n";
      print_string "    t.down(); if (rotation > 0) t.right(60); int turn = 120;\n";
      print_string "    t.forward(size); t.right(turn);\n";
      print_string "    t.forward(size); t.right(turn);\n";
      print_string "    t.forward(size); t.right(turn);\n";
      print_string "    t.left( rotation );\n";
      print_string "}\n"
		)

		else print_string "not defining methods"


let get_string_of_classname = function 
	Jast.CreateClass(string_of_classname) -> string_of_classname

let gen_java_modified = function
	Jast.JavaProgram(classname, shapes) ->
		let string_of_classname = get_string_of_classname classname in
			print_string "public class ";
			print_string string_of_classname; (*Print the string of class name for class header *)
			print_string "{\n\n";

			define_methods 1;

	 		print_string "  public static void main(String[] args) {\n\n";
	 		print_string "    Turtle t = new Turtle();\n";
	 		print_string "    t.hide();\n";
			print_string "    t.speed(0);\n";

			(*Go through and print all the shapes!*)
			let l = List.length shapes in 
			if (l > 0) then 
				(List.map proc_shape shapes)
			else if (l == 0) then
				(*(List.map proc_shape shapes);*)
				(*Just draw a dot if we have no shapes*)
				(print_string "    t.setPosition(0,0);\n    t.dot();\n";
				List.map proc_shape shapes)
			else 
				(List.map proc_shape shapes);
				

			print_string "  t.save(\"";
			print_string string_of_classname; (*Print the string of class name for jpg file*)
			print_string ".jpg\");\n";
			print_string "  }\n\n}\n"

let _ =	
	gen_java_modified jast