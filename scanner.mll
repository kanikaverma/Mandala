(* Authors: Edo Roth, Harsha Vemuri *)

{ open Parser }

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let number = '-'? num* '.'? num*

rule token = parse

(* white space *)
| [' ' '\t' '\r' '\n']             { token lexbuf }

(* literals and variables *)
| digit+ as lit                    { LITERAL(int_of_string lit) }
| alpha+ (alpha | digit)* as lit   { VARIABLE(int_of_char lit - 97) }
| number as lit                    { LITERAL(float_of_string lit) }

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
| "while"      { WHILE }    | "break" { BREAK }
| "foreach"    { FOREACH }  | "to"    { TO }
| "continue"   { CONTINUE } | "pass"  { PASS }

(* punctuation and delimiters *)
| '('          { LPAREN }   | ')'       { RPAREN }
| '['          { LBRACK }   | ']'       { RBRACK }
| '{'          { LBRACE }   | '}'       { RBRACE }
| ','          { COMMA }    | '.'       { PERIOD }

(* functions and constructors *)
| "addTo"      { ADDTO }    | "create"  { CREATE }
| "draw"       { DRAW }     | "return"  { RETURN }

(* language specific keywords *)
| "radius"     { RADIUS }   | "count"   { COUNT }
| "size"       { SIZE }     | "color"   { COLOR }
| "rotation"   { ROTATION } | "offset"  { OFFSET }
| "Template"   { TEMPLATE }

(* types *)
| "Shape"      { SHAPE } 
| "Layer"      { LAYER }  
| "Mandala"    { MANDALA }  
| "Geo"        { GEO }

(* geo *)
| "Circle"     { CIRCLE }
| "Square"     { SQUARE }
| "Triangle"   { TRIANGLE }

(* end of file *)
| eof          { EOF }

and comment = parse 

| "#/"         { token lexbuf } 