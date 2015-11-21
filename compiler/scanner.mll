(* Authors: Edo Roth, Harsha Vemuri *)

{ open Parser;; }

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let number = '-'? digit+ '.' digit* | '-'? digit* '.' digit+


rule token = parse

(* white space *)
| [' ' '\t' '\r' '\n']                { token lexbuf }

(* literals and variables *)
| '-'? digit+ as lit                       { LITERAL(int_of_string lit) }
| number as lit                		    { FLOAT_LITERAL(float_of_string lit) }
| ['a'-'z']+ (alpha | digit)* as lxm        { ID(lxm) }

(* comments *)
| "/#"         { comment lexbuf }

(* arithmetic operators *)
| '+'          { PLUS }     | '*'     { TIMES }
| '-'          { MINUS }    | '/'     { DIVIDE }
(*| '%'          { MODULUS }  | '^'     { EXP } *)

(* conditional operators *)
| "=="         { EQ }       | "!="    { NEQ }
| "<"          { LT }       | ">"     { GT }
| "<="         { LEQ }      | ">="    { GEQ }

(* boolean operators *)
(*| "true"       { TRUE }     | "false" { FALSE }
| "and"        { AND }      | "or"    { OR }
| "not"        { NOT }      | "xor"   { XOR }*)

(* assignment *)
| '='          { ASSIGN }   | ':'     { COLON }  

(* conditional words *)
| "Rf"         { IF }
| "Else"       { ELSE }

(* loop words *)
| "Break" { BREAK }			| "Foreach"    { FOREACH }  
| "To"    { TO }			| "Continue"   { CONTINUE } 

(* punctuation and delimiters *)
| '('          { LPAREN }     | ')'       { RPAREN }
| '['          { LBRACKET }   | ']'       { RBRACKET }
| '{'          { LBRACE }     | '}'       { RBRACE }
| ','          { COMMA }      (*| '.'       { PERIOD }*)
| ';' 		   { SEMI }

(* built-in functions and constructors *)
| "Def"        { DEF }      | "Return"  { RETURN }
(*| "draw"       { DRAW }     | "addTo"   { ADDTO }*)    
| "Create"     { CREATE }

(* language specific keywords *)
| "Radius"     { RADIUS }   | "Count"   { COUNT }
| "Size"       { SIZE }     | "Color"   { COLOR }
| "Rotation"   { ROTATION } | "Offset"  { OFFSET }
| "AngularShift" { ANGULARSHIFT }

(* types *)
| "Number"     { NUMBER }   (*| "String"     { STRING }*)
| "Boolean"    { BOOLEAN }  | "Void"       { VOID }
| "Shape"      { SHAPE }    | "Geo"        { GEO }
| "Layer"      { LAYER }    | "Mandala"    { MANDALA }  

(* geo *)
| "Circle"     { CIRCLE }
| "Square"     { SQUARE }
| "Triangle"   { TRIANGLE }

(* end of file *)
| eof          { EOF }

and comment = parse 

| "#/"         { token lexbuf } 
