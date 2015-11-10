%{ open Ast;; %}
/* punctuation and delimiters */
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SEMI/* PERIOD add accessors*/
/* arithmetic operators */
%token PLUS MINUS TIMES DIVIDE /* MODULUS EXP */
/* conditional operators */  
%token EQ NEQ LT LEQ GT GEQ 
/* conditional keywords */
%token IF ELSE
/* loop keywords */
%token BREAK FOREACH TO CONTINUE
/* boolean operators */
/* %token TRUE FALSE AND OR NOT XOR */
/* assignment operators */
%token ASSIGN COLON 
/* built in functions and constructors */
%token DEF RETURN CREATE /* DRAW ADDTO */
/* language specific keywords */
%token RADIUS COUNT SIZE COLOR ROTATION OFFSET ANGULARSHIFT
/* types */
%token NUMBER BOOLEAN VOID SHAPE GEO LAYER MANDALA 
/* geo types */
%token CIRCLE TRIANGLE SQUARE
/* need to add geo_type to AST possibly, define cases for different GEO TYPES */
/* literals and variables, or ids */
%token <float> FLOAT_LITERAL
%token <int> LITERAL
%token <string> ID
/* end of file */
%token EOF

/* Precedence and associativity of each operator */
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN 
/*%left  COMMA*/
/* %right COLON */
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
	| decls stmt { ($2 :: fst $1), snd $1 }
	
/* function declaration
def ReturnType functionName(Type param1, Type param2, ...):
      functionBody */
fdecl:
	DEF any_id ID LPAREN formals_opt RPAREN COLON 
	LBRACE stmt_list RBRACE
		{{ 
			fname = $3; 				(* function name *)
			returntype = $2;  	(* return type *)
			formals = $5; 				(* formal parameters, list of args *)
			body = List.rev $9 			(* statement list *)
		}}

	| DEF any_id LBRACKET RBRACKET ID LPAREN formals_opt RPAREN COLON 
	LBRACE stmt_list RBRACE
		{{ 
			fname = $5; 				(* function name *)
			returntype = $2;  	(* return type *)
			formals = $7; 				(* formal parameters, list of args *)
			body = List.rev $11 			(* statement list *)
		}}
	
	/* need to add in all options for RETURN_TYPE and see if cdecl fits under here */
	/* NUMBER STRING GEO MANDALA LAYER SHAPE NUMBER[] STRING[] GEO[] LAYER[] SHAPE[] MANDALA[]*/

formals_opt:
	/* nothing */                { [] }
	| formal_list 				{ List.rev $1 }
/*maybe add TYPE as a type in the AST */
/* add variable declaration to AST  that has a type, name and value */

formal_list:
	formal				{ [$1] }
	| formal_list COMMA formal	{ $3 :: $1 }

formal:
	any_id ID 		
		{{ 
			kind = $1; 		(* variable type *)
			vname = $2; 	(* variable name *)
		}}	

any_id:
	type_id 			{ $1 }
	| basic_types 		{ $1 }	

type_id:
	MANDALA 				{ Mandalat }
	| LAYER 				{ Layert }
	| SHAPE 				{ Shapet }
	

basic_types:
	NUMBER 					{ Numbert }
	| BOOLEAN 				{ Booleant }
	| GEO 					{ Geot }

stmt_list:
	/* nothing */ 				{ [] }
	| stmt_list stmt 			{ $2 :: $1 }
/* Note syntax change for construction of Mandala, Layer and Shape */
stmt:
	/*expr SEMI									{ Expr($1) }*/
	| RETURN expr SEMI 								{ Return($2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE 	{ IF($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt 		{ IF($3, $5, $7) }
	| FOREACH ID ASSIGN LITERAL TO LITERAL COLON 
		LBRACE stmt_list RBRACE					{ Foreach($2, $4, $6, $9) }
	| assign_expr ASSIGN CREATE SHAPE COLON LBRACE GEO expr 
		SIZE expr 
		COLOR expr 
		ROTATION expr RBRACE SEMI			{ Shape($1, $8, $10, $12, $14) }
	| assign_expr ASSIGN CREATE MANDALA SEMI	{ Mandala($1) }
	| assign_expr ASSIGN CREATE LAYER COLON LBRACE RADIUS expr
		SHAPE expr
		COUNT expr
		OFFSET expr 
		ANGULARSHIFT expr RBRACE SEMI			{ Layer($1, $8, $10, $12, $14, $16) }
	| assign_expr ASSIGN expr SEMI		{ Assign($1, $3) }
	| array_expr ASSIGN LBRACE actuals_opt RBRACE SEMI 	{ ArrAssign($1, $4) } 
/*	| array_expr ASSIGN func_call SEMI 	{ Assign($1, $3) }
	| assign_expr ASSIGN func_call SEMI 	{Assign($1,$3)}
	| func_call SEMI 						{ FuncCall($1) }*/


/*func_call:
	| ID COLON actuals_opt      { Call($1, $3) }*/
	
expr_opt:
	/* nothing */ 		{ Noexpr }
	| expr 			{ $1 }

expr:
	LITERAL 					{ Literal($1) }
	| FLOAT_LITERAL				{ Float_Literal($1) }
	| ID 						{ Id($1) }
	| expr PLUS expr			{ Binop($1, Add, $3) }
	| expr MINUS expr			{ Binop($1, Sub, $3) }
	| expr TIMES expr 			{ Binop($1, Mult, $3) }
	| expr DIVIDE expr 			{ Binop($1, Div, $3) }
	| expr EQ expr 				{ Binop($1, Equal, $3) }
	| expr NEQ expr 			{ Binop($1, Neq, $3) }
	| expr LT expr 				{ Binop($1, Less, $3) }
	| expr LEQ expr 			{ Binop($1, Leq, $3) }
	| expr GT expr				{ Binop($1, Greater, $3) }
	| expr GEQ expr 			{ Binop($1, Geq, $3) }
	| LPAREN expr RPAREN 		{ $2 }
	| ID COLON LPAREN actuals_opt RPAREN      { Call($1, []) }
	
	
array_expr:
	any_id LBRACKET RBRACKET ID
		{{ 
			kind = $1; 		(* variable type *)
			vname = $4; 	(* variable name *)
		}}

 assign_expr:
 	any_id ID 			
		{{ 
			kind = $1; 		(* variable type *)
			vname = $2; 	(* variable name *)
		}}

 actuals_opt:
	/*nothing*/					{ [] }
	| actuals_list 				{ List.rev $1 }
actuals_list:
	expr  								{ [$1] }
	| actuals_list COMMA expr 			{ $3 :: $1 }

/*actuals_opt:
 nothing  							{ [] }
|expr           						{[$1]}g
| actuals_opt COMMA expr 				{$1 @ [ $3 ]}*/