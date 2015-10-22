(* Authors: Edo Roth, Harsha Vemuri *)

{ open Parser }

let digit = ['0'-'9']
let alpha = ['a'-'z']

rule token = parse

(* white space *)
| [' ' '\t' '\r' '\n'] { token lexbuf }

(* comments *)
| '#'                  { comment lexbuf }

(* arithmetic operators *)
| '+'                  { PLUS }
| '-'                  { MINUS }
| '*'                  { TIMES }
| '/'                  { DIVIDE }
| '%'                  { MODULUS }
| '^'                  { EXP }

(* conditional operators *)
| "=="                 { EQ }
| "!"                  { NOT }
| "!="                 { NEQ }
| "<"                  { LT }
| ">"                  { GT }
| "<="                 { LEQ }
| ">="                 { GEQ }

(* boolean operators *)
| "true"               { TRUE }
| "false"              { FALSE }
| "and"                { AND }
| "not"                { NOT }
| "or"                 { OR }
| "xor"                { XOR }

(* assignment *)
| '='                  { ASSIGN }
| ':'                  { COLON }  

(* conditional words *)
| "if"                 { IF }
| "elif"               { ELIF }
| "else"               { ELSE }

(* loop words *)
| "while"              { WHILE }
| "break"              { BREAK }
| "foreach"            { FOREACH }
| "to"                 { TO }
| "continue"           { CONTINUE }
| "pass"               { PASS }

(* punctuation and delimiters *)
| '('                  { LPAREN }
| ')'                  { RPAREN }
| '['                  { LBRACK }
| ']'                  { RBRACK }
| '{'                  { LBRACE }
| '}'                  { RBRACE }
| ','                  { COMMA }
| '.'                  { PERIOD }

(* functions *)
| "addTo"              { ADDTO }
| "create"             { CREATE }
| "draw"               { DRAW }
| "return"             { RETURN }

(* language specific keywords *)
| "radius"             { RADIUS }
| "count"              { COUNT }
| "size"               { SIZE }
| "color"              { COLOR }
| "rotation"           { ROTATION }
| "offset"             { OFFSET }
| "Template"           { TEMPLATE }

(* types *)
| "type"               { TYPE }
| "Shape"              { SHAPE }
| "Circle"             { CIRCLE }
| "Triangle"           { TRIANGLE }
| "Square"             { SQUARE }
| "Layer"              { LAYER }
| "Mandala"            { MANDALA }

(* literals and variables *)
| digit+ as lit        { LITERAL(int_of_string lit) }
| alpha as lit         { VARIABLE(int_of_char lit - 97) }

(* end of file *)
| eof                  { EOF }