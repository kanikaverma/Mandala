%{ open Ast;; %}

%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COLON COMMA 
%token PLUS MINUS TIMES DIVIDE MODULUS EXP  
%token ASSIGN EQ NEQ LT LEQ GT GEQ 
%token RETURN IF ELSE FOREACH BREAK CONTINUE
%token TRUE FALSE AND OR NOT XOR
%token TO DEF DRAW CREATE ADDTO 
%token RADIUS COUNT SIZE COLOR ROTATION OFFSET ANGULARSHIFT
%token NUMBER BOOLEAN STRING VOID GEO SHAPE LAYER MANDALA
%token CIRCLE TRIANGLE SQUARE
%token <int> LITERAL
%token <string> ID
%token EOF

/* Precedence and associativity of each operator */
%nonassoc NOELSE
%nonassoc ELSE 
%right ASSIGN 
%left  COMMA
%right COLON
%right EQ NEQ 
%left LT GT LEQ GEQ 
%left  PLUS MINUS
%left  TIMES DIVIDE

%start program
%type  <Ast.program> program

%%

program:
	decls EOF		{ $1 }

decls:
	/* nothing */ { [], [] }
	| decls fdecl { fst $1, ($2 :: snd $1) }
	| decls vdecl { ($2 :: fst $1), snd $1 }
	| decls cdecl { fst $1, ($2 :: snd $1) }

fdecl:
	DEF MANDALA ID LPAREN formals_opt RPAREN COLON vdecl_list stmt_list 
		{ { fname = $3;
		returntype = Mandalat;
		formals = $5;
		locals = List.rev $8;
		body = List.rev $9 }}
	| DEF LAYER ID LPAREN formals_opt RPAREN COLON vdecl_list stmt_list 
		{ { fname = $3;
		returntype = Layert;
		formals = $5;
		locals = List.rev $8;
		body = List.rev $9 }}
	| DEF SHAPE ID LPAREN formals_opt RPAREN COLON vdecl_list stmt_list 
		{ { fname = $3;
		returntype = Shapet;
		formals = $5;
		locals = List.rev $8;
		body = List.rev $9 }}
	| DEF GEO ID LPAREN formals_opt RPAREN COLON vdecl_list stmt_list 
		{ { fname = $3;
		returntype = Geot;
		formals = $5;
		locals = List.rev $8;
		body = List.rev $9 }}
	| DEF NUMBER ID LPAREN formals_opt RPAREN COLON vdecl_list stmt_list 
		{ { fname = $3;
		returntype = Numbert;
		formals = $5;
		locals = List.rev $8;
		body = List.rev $9 }}
	| DEF STRING ID LPAREN formals_opt RPAREN COLON vdecl_list stmt_list 
		{ { fname = $3;
		returntype = Stringt;
		formals = $5;
		locals = List.rev $8;
		body = List.rev $9 }}
	
	/* need to add in all options for RETURN_TYPE and see if cdecl fits under here */
	/* NUMBER STRING GEO MANDALA LAYER SHAPE NUMBER[] STRING[] GEO[] LAYER[] SHAPE[] MANDALA[]*/

formals_opt:
	/* nothing */                { [] }
	| formal_list 				{ List.rev $1 }
/*maybe add TYPE as a type in the AST */
/* add variable declaration to AST  that has a type, name and value */
/*TYPE:
	SHAPE
	| LAYER
	| MANDALA
	| NUMBER
	| STRING
	| GEO */ 
formal_list:
	formal				{ [$1] }
	| formal_list COMMA formal	{ $3 :: $1 }
/* WHATSHOULD BE IN VVALUE FOR VARIABLES? */
formal:
	| SHAPE ID 		{{ kind = Shapet; vname = $2;}}			
	| MANDALA ID 	{{ kind = Mandalat; vname = $2;}}
	| GEO ID 		{{ kind = Geot; vname = $2;}}
	| STRING ID 	{{ kind = Stringt; vname = $2;}}
	| NUMBER ID 	{{ kind = Numbert; vname = $2;}}
	| LAYER ID 		{{ kind = Layert; vname = $2;}}
vdecl_list:
	/* nothing */ 				{ [] }
	| vdecl_list vdecl 			{ $2 :: $1 }
vdecl:
	| SHAPE ID 		{{ kind = Shapet; vname = $2;}}			
	| MANDALA ID 	{{ kind = Mandalat; vname = $2;}}
	| GEO ID 		{{ kind = Geot; vname = $2;}}
	| STRING ID 	{{ kind = Stringt; vname = $2;}}
	| NUMBER ID 	{{ kind = Numbert; vname = $2;}}
	| LAYER ID 		{{ kind = Layert; vname = $2;}}
stmt_list:
	/* nothing */ 				{ [] }
	| stmt_list stmt 			{ $2 :: $1 }

stmt:
	expr										{ Expr($1) }
	| RETURN expr 								{ Return($2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE 	{ IF($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt 		{ IF($3, $5, $7) }
	| FOREACH expr_opt TO expr_opt COLON stmt 	{ Foreach($2, $4, $6) }

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
	expr 								{ [$1] }
	| actuals_list COMMA expr 			{ $3 :: $1 }
	

/* SPECIFY DIFFERENT TYPES OF CREATION! */
cdecl:
	CREATE MANDALA ID COLON construct_args
		{{ fname = $3;
			returntype = Mandalat;
			formals = $5;
		}}

construct_args:
	mandala_args 		{ [] }
	| layer_args		{ List.rev $1 }
	| shape_args 		{ List.rev $1 }

mandala_args:
	/* nothing */		{ [] }
layer_args:
	SHAPE expr RADIUS expr COUNT expr OFFSET expr ANGULARSHIFT expr 
			{{ lshape = $2;
				lradius = $4;
				lcount = $6;
				loffset = $8;
				langularshift = $10 }}

shape_args:
	GEO expr SIZE expr COLOR expr ROTATION expr
		{{ sgeoname = $2;
		ssize = $4;
		scolor = $6;
		srotation = $8 }}