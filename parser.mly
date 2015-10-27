%{ open Ast %}

%token 
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMMA PLUS MINUS TIMES DIVIDE MODULUS EXP COLON 
%token ASSIGN EQ NEQ LT LEQ GT GEQ RETURN IF ELSEIF ELSE FOREACH TRUE FALSE AND OR NOT XOR
%token BREAK CONTINUE TO PASS DEF DRAW CREATE ADDTO 
%token RADIUS COUNT SIZE COLOR ROTATION OFFSET EOF
%token NUMBER BOOLEAN SHAPE STRING VOID GEO SHAPE LAYER MANDALA
%token CIRCLE TRIANGLE SQUARE
%token <int> LITERAL VARIABLE
%token <string> ID 

%left  COMMA
%right COLON
%right EQ
%left  PLUS MINUS
%left  TIMES DIVIDE

%start expr
%type  <Ast.expr> expr

%%

program: 
	decls EOF		{ $1 }

decls:
	fdecl
	vdecl
	cdecl

fdecl:
	DEF RETURN_TYPE ID LPAREN formals_opt RPAREN COLON vdecl_list stmt_list 
		{ { fname = $3;
		returntype = $2;
		formals = $4;
		locals = List.rev $7;
		body = List.rev $8 }}

formals_opt:
	/* nothing */                { [] }
	| formal_list 
TYPE:
	SHAPE
	| LAYER
	| MANDALA
	| NUMBER
	| STRING
	| GEO
formal_list:
	TYPE ID				{ [$1] }
	| formal_list COMMA TYPE ID 	{ ($3, $4) :: $1 }
vdecl_list:
	/* nothing */ 				{ [] }
	| vdecl_list vdecl 			{ $2 :: $1 }
vdecl:
	TYPE ID 				{ $2 }
stmt_list:
	/* nothing */ 				{ [] }
	| stmt_list stmt 			{ $2 :: $1 }

stmt:
	expr					{ Expr($1) }
	| RETURN expr
	| IF LPAREN expr RPAREN stmt %prec NOELSE
	| IF LPAREN expr RPAREN stmt ELSE stmt
	| FOREACH expr_opt TO expr_opt COLON stmt
expr_opt:
	/* nothing */ 		{ Noexpr }
	| expr 			{ $1 }

expr:
	LITERAL 					{ Literal($1) }
	| ID 						{ Id($1) }
	| expr PLUS expr			{ Binop($1, Add, $3) }
	| expr MINUS expr			{ Binop($1, Sub, $3) }
	| expr TIMES expr 			{ Binop($1, Mult, $3) }
	| expr DIVIDE expr 			{ Binop($1, Div, $3) }
	| expr EQ expr 			{ Binop($1, Equal, $3) }
	| expr NEQ expr 			{ Binop($1, Neq, $3) }
	| expr LT expr 				{ Binop($1, Less, $3) }
	| expr LEQ expr 			{ Binop($1, Leq, $3) }
	| expr GT expr				{ Binop($1, Greater, $3) }
	| expr GEQ expr 			{ Binop($1, Geq, $3) }
	| ID ASSIGN expr 			{ Assign($1, $3) }
	| ID COLON actuals_opt 		{ Call($1, $3) }
	| expr COMMA expr			{ Binop($1, Comm, $3) }
	| LPAREN expr RPAREN 		{ $2 }
	| ID LBRACKET expr RBRACKET	{ Array($1, $3) }
	| PLUS PLUS expr			{ PrefixPlus($3) }
	| MINUS MINUS expr 			{ PrefixMinus($3) }
	| expr PLUS PLUS			{ PostfixPlus($1) }
| expr MINUS MINUS 			{ PostfixMinus($1) }		

actuals_opt:
	/* nothing */ 				{ [ ] }
	| actuals_list 				{ List.rev $1 }
actuals_list:
	expr
	| actuals_list COMMA expr 			{ $3 :: $1 }
	
cdecl:
	/* nothing */
	| CREATE TYPE COLON construct_args
construct_args:
	mandala_args
	| layer_args
	| shape_args

mandala_args:
	/* nothing */
layer_args:
	SHAPE expr RADIUS expr COUNT expr OFFSET expr ANGULARSHIFT expr 
	| SHAPE expr RADIUS expr COUNT expr OFFSET expr
	| SHAPE expr RADIUS expr COUNT expr ANGULARSHIFT expr
	| SHAPE expr RADIUS expr COUNT expr 
			{{ lshape = $2;
				lradius = $4;
			lcount = $6;
			loffset = $8;
			langularshift = $10 }}
shape_args:
GEO expr SIZE expr COLOR expr ROTATION expr
| GEO expr SIZE expr COLOR expr
| GEO expr SIZE expr ROTATION expr
| GEO expr SIZE expr
{{ sgeoname = $2;
ssize = $4;
scolor = $6;
srotation = $8 }}

expr:
  expr PLUS   expr     { Binop($1, Add, $3) }
| expr MINUS  expr     { Binop($1, Sub, $3) }
| expr TIMES  expr     { Binop($1, Mul, $3) }
| expr DIVIDE expr     { Binop($1, Div, $3) }
| LITERAL              { Lit($1) }
| VARIABLE             { Var($1) }
| VARIABLE EQUALS expr { Asn($1, $3) } 
| expr COMMA expr      { Seq($1, $3) }
| expr COLON expr      { Col($1, $3) }