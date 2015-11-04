%{ open Ast;; %}
/* punctuation and delimiters */
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SEMI/* PERIOD add accessors */
/* arithmetic operators */
%token PLUS MINUS TIMES DIVIDE /* MODULUS EXP */
/* conditional operators */  
%token EQ NEQ LT LEQ GT GEQ 
/* conditional keywords */
%token IF ELSE
/* loop keywords */
%token /*BREAK*/ FOREACH TO /*CONTINUE*/
/* boolean operators */
/* %token TRUE FALSE AND OR NOT XOR */
/* assignment operators */
%token ASSIGN COLON 
/* built in functions and constructors */
%token DEF RETURN /* DRAW ADDTO */ /*CREATE*/
/* language specific keywords */
/*%token RADIUS COUNT SIZE COLOR ROTATION OFFSET ANGULARSHIFT*/
/* types */
%token NUMBER /*BOOLEAN VOID */ SHAPE GEO LAYER MANDALA
/* geo types */
/* %token CIRCLE TRIANGLE SQUARE */
/* need to add geo_type to AST possibly, define cases for different GEO TYPES */
/* literals and variables, or ids */
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
	| decls vdecl SEMI { ($2 :: fst $1), snd $1 }
	| decls fdecl { fst $1, ($2 :: snd $1) }
	| decls stmt { ($2 :: fst $1), snd $1 }
	
	/* | decls cdecl { fst $1, ($2 :: snd $1) }*/

/* function declaration
def ReturnType functionName(Type param1, Type param2, ...):
      functionBody */
fdecl:
	DEF type_id ID LPAREN formals_opt RPAREN COLON 
	LBRACE vdecl_list stmt_list RBRACE
		{{ 
			fname = $3; 				(* function name *)
			returntype = $2;  	(* return type *)
			formals = $5; 				(* formal parameters, list of args *)
			locals = List.rev $8; 		(* varaiable list *)
			body = List.rev $9 			(* statement list *)
		}}
/*	| DEF LAYER ID LPAREN formals_opt RPAREN COLON vdecl_list stmt_list 
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
	| DEF VOID ID LPAREN formals_opt RPAREN COLON vdecl_list stmt_list 
		{ { fname = $3;
		returntype = Stringt;
		formals = $5;
		locals = List.rev $8;
		body = List.rev $9 }}*/
	
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

formal:
	type_id ID 		
		{{ 
			kind = $1; 		(* variable type *)
			vname = $2; 	(* variable name *)
		}}	

/* constructor */
 
/* end of constructor info */

/* formal paramaters, can pass in array TYPE[] name */
/*formal:
	type_id ID 		
		{{ 
			kind = $1; 		(* variable type *)
			vname = $2; 	(* variable name *)
		}}	*/		

type_id:
	MANDALA 				{ Mandalat }
	| LAYER 				{ Layert }
	| SHAPE 				{ Shapet }
	| GEO 					{ Geot }
	| NUMBER 				{ Numbert }

vdecl_list:
	/* nothing 	*/			{ [] }
	| vdecl_list vdecl		{ $2 :: $1 }

vdecl:
	/*NUMBER ID 		{ $2 }*/
	type_id ID 	
	{{ 
		kind = $1; 		(* variable type *)
		vname = $2;  	(* variable name *)
	}}


stmt_list:
	/* nothing */ 				{ [] }
	| stmt_list stmt 			{ $2 :: $1 }

stmt:
	expr SEMI									{ Expr($1) }
	| RETURN expr SEMI 								{ Return($2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE 	{ IF($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt 		{ IF($3, $5, $7) }
	| FOREACH expr_opt TO expr_opt COLON stmt 	{ Foreach($2, $4, $6) }
	
expr_opt:
	/* nothing */ 		{ Noexpr }
	| expr 			{ $1 }

expr:
	
	/*| literal_expr 				{ $1 }*/
	 LITERAL 					{ Literal($1) }
	| ID 						{ Id($1) }
	/*GEO expr SIZE expr COLOR expr ROTATION expr*/
	/*| GEO * expr * 
		SIZE * expr * 
		COLOR * expr * 
		ROTATION * expr 		{ Shape($2, $4, $6, $8) }*/
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
	| ID ASSIGN expr 			{ Assign($1, $3) }
	| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
	| LPAREN expr RPAREN 		{ $2 }

actuals_opt:
	/* nothing */ 				{ [] }
	| actuals_list 				{ List.rev $1 }
actuals_list:
	expr 								{ [$1] }
	| actuals_list COMMA expr 			{ $3 :: $1 }



	/* add in postfix and prefix operators for addition and subtraction */
	/*| PLUS PLUS expr			{ PrefixPlus($3) }
	| MINUS MINUS expr 			{ PrefixMinus($3) }
	| expr PLUS PLUS			{ PostfixPlus($1) }
	| expr MINUS MINUS 			{ PostfixMinus($1) }*/
/*literal_expr:
	| MANDALA 		{ Literal_mandala($1) }
	| LAYER 		{ Literal_layer($1) }
	| SHAPE 		{ Literal_shape($1) }
	| GEO 			{ Literal_geo($1) }
	| NUMBER 		{ Literal_number($1) }
	| STRING 		{ Literal_string($1) }
	| array_types 	{ $1 }*/
/* variable assignment expression */
/*assign_expr:
	| array_expr LBRACKET expr RBRACKET	{ VarAssign($1, $3) }
	| ID		{ int_of_char($1) }*/
	
/*array_expr:
	| ID 			{ Id($1) }
	| array_types 	{ $1 }

array_types:
	| LBRACE mandala_list RBRACE 	{ Literal_mandala_array(List.rev $2) }
	| LBRACE layer_list RBRACE 		{ Literal_layer_array(List.rev $2) }
	| LBRACE shape_list RBRACE 		{ Literal_shape_array(List.rev $2) }
	| LBRACE geo_list RBRACE 		{ Literal_geo_array(List.rev $2) }
	| LBRACE number_list RBRACE 	{ Literal_number_array(List.rev $2) }
	| LBRACE string_list RBRACE 	{ Literal_string_array(List.rev $2) }
mandala_list:
	| MANDALA 						{ [$1] }
	| mandala_list COMMA MANDALA 	{ $3 :: $1 }
layer_list:
	| LAYER 					{ [$1] }
	| layer_list COMMA LAYER 	{ $3 :: $1 }
shape_list:
	| SHAPE 					{ [$1] }
	| shape_list COMMA SHAPE 	{ $3 :: $1 }
geo_list:
	| GEO 						{ [$1] }
	| geo_list COMMA GEO 		{ $3 :: $1 }

number_list:
	| NUMBER 					{ [$1] }
	| number_list COMMA NUMBER 	{ $3 :: $1 }
string_list:
	| STRING 					{ [$1] }
	| string_list COMMA STRING 	{ $3 :: $1 }*/



	

