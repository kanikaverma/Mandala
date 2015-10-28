(* Authors: Edo Roth, Harsha Vemuri *)

{ open Parser;; }

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let number = '-'? digit* '.'? digit*

rule token = parse

(* white space *)
| [' ' '\t' '\r' '\n']                { token lexbuf }

(* literals and variables *)
| digit+ as lit                       { LITERAL(int_of_string lit) }
| number as lit                       { LITERAL(float_of_string lit) }
| alpha+ (alpha | digit)* as lit      { ID(int_of_char lit - 97) }

(* comments *)
| "/#"         { comment lexbuf }

(* arithmetic operators *)
| '+'          { PLUS }     | '*'     { TIMES }
| '-'          { MINUS }    | '/'     { DIVIDE }
| '%'          { MODULUS }  | '^'     { EXP }

(* conditional operators *)
| "=="         { EQ }       | "!="    { NEQ }
| "<"          { LT }       | ">"     { GT }
| "<="         { LEQ }      | ">="    { GEQ }

(* boolean operators *)
| "true"       { TRUE }     | "false" { FALSE }
| "and"        { AND }      | "or"    { OR }
| "not"        { NOT }      | "xor"   { XOR }

(* assignment *)
| '='          { ASSIGN }   | ':'     { COLON }  

(* conditional words *)
| "if"         { IF }
| "elif"       { ELIF }
| "else"       { ELSE }

(* loop words *)
| "break" { BREAK }			| "foreach"    { FOREACH }  
| "to"    { TO }			| "continue"   { CONTINUE } 

(* punctuation and delimiters *)
| '('          { LPAREN }   | ')'       { RPAREN }
| '['          { LBRACK }   | ']'       { RBRACK }
| '{'          { LBRACE }   | '}'       { RBRACE }
| ','          { COMMA }    | '.'       { PERIOD }

(* built-in functions and constructors *)
| "def"        { DEF }      | "return"  { RETURN }
| "draw"       { DRAW }     | "addTo"   { ADDTO }      
| "create"     { CREATE }

(* language specific keywords *)
| "radius"     { RADIUS }   | "count"   { COUNT }
| "size"       { SIZE }     | "color"   { COLOR }
| "rotation"   { ROTATION } | "offset"  { OFFSET }
| "angularShift" { ANGULARSHIFT }

(* types *)
| "Number"     { NUMBER }   | "String"     { STRING }
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
