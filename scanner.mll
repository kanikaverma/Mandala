{ open Parser }

let digit = ['0'-'9']
let alpha = ['a'-'z']
let float = '-'? num+ '.' num* | '.' num+ 

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

(* punctuation *)
| '('                  { LPAREN }
| ')'                  { RPAREN }
| ','                  { COMMA }
| '.'                  { PERIOD }

(* literals and variables *)
| digit+ as lit        { LITERAL(int_of_string lit) }
| float as lit         { LITERAL(float_of_string lit) }
| alpha as lit         { VARIABLE(int_of_char lit - 97) }

(* end of file *)
| eof                  { EOF }