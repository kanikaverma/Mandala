(* Authors: Edo Roth, Harsha Vemuri *)

{ open Parser;; }

(*numbers and literals*)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let number = '-'? digit+ '.' digit* | '-'? digit* '.' digit+

rule token = parse

(* white space *)
| [' ' '\t' '\r' '\n']                { token lexbuf }

(* literals and variables *)
| '-'? digit+ as lit                  { LITERAL(int_of_string lit) }
| number as lit                       { FLOAT_LITERAL(float_of_string lit) }
| ['a'-'z']+ (alpha | digit)* as lxm  { ID(lxm) }

(* comments *)
| "/#"         { comment lexbuf }

(* arithmetic operators *)
| '+'          { PLUS }     | '*'     { TIMES }
| '-'          { MINUS }    | '/'     { DIVIDE }

(* conditional operators *)
| "=="         { EQ }       | "!="    { NEQ }
| "<"          { LT }       | ">"     { GT }
| "<="         { LEQ }      | ">="    { GEQ }

(* assignment *)
| '='          { ASSIGN }   | ':'     { COLON }  

(* loop words *)
| "To"         { TO }       | "Foreach" { FOREACH }  

(* punctuation and delimiters *)
| '('          { LPAREN }   | ')'       { RPAREN }
| '['          { LBRACKET } | ']'       { RBRACKET }
| '{'          { LBRACE }   | '}'       { RBRACE }
| ','          { COMMA }      
| ';'          { SEMI }

(* built-in functions and constructors *)
| "Def"        { DEF }      | "Return"  { RETURN }
| "Create"     { CREATE }

(* language specific keywords *)
| "Radius"     { RADIUS }   | "Count"   { COUNT }
| "Size"       { SIZE }     | "Color"   { COLOR }
| "Rotation"   { ROTATION } | "Offset"  { OFFSET }
| "AngularShift" { ANGULARSHIFT }

(* types *)
| "Number"     { NUMBER }   | "Void"       { VOID }
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