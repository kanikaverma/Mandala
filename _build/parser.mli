type token =
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COLON
  | COMMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULUS
  | EXP
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURN
  | IF
  | ELSEIF
  | ELSE
  | FOREACH
  | BREAK
  | CONTINUE
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | XOR
  | TO
  | DEF
  | DRAW
  | CREATE
  | ADDTO
  | RADIUS
  | COUNT
  | SIZE
  | COLOR
  | ROTATION
  | OFFSET
  | ANGULARSHIFT
  | NUMBER
  | BOOLEAN
  | SHAPE
  | STRING
  | VOID
  | GEO
  | LAYER
  | MANDALA
  | CIRCLE
  | TRIANGLE
  | SQUARE
  | LITERAL of (int)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
