%{ open Ast;; %}

/* punctuation and delimiters */
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SEMI
/* arithmetic operators */
%token PLUS MINUS TIMES DIVIDE 
/* loop operators */
%token FOREACH TO
/* assignment */
%token ASSIGN COLON 
/* built-in functions and constructors */
%token DEF RETURN CREATE
/* language specific keywords */
%token RADIUS COUNT SIZE COLOR ROTATION OFFSET ANGULARSHIFT
/* types */
%token NUMBER BOOLEAN VOID SHAPE GEO LAYER MANDALA 
/* geo types */
%token CIRCLE TRIANGLE SQUARE
/* literals and variables */
%token <float> FLOAT_LITERAL
%token <int> LITERAL
%token <string> ID
/* end of file */
%token EOF

%right ASSIGN
%left  PLUS MINUS
%left  TIMES DIVIDE

%start program
%type  <Ast.program> program

%%

program:
  decls EOF                                                  { $1 }

/* Parse function declarations and statements */
decls:
  /* nothing */                                              { [], [] } 
  | decls fdecl                                              { fst $1, ($2 :: snd $1) }
  | decls stmt                                               { ($2 :: fst $1), snd $1 }

fdecl:
  DEF any_id ID LPAREN formals_opt RPAREN COLON LBRACE stmt_list RBRACE SEMI
    {{ 
      fname = $3;         
      returntype = $2;    
      formals = $5;       
      body = List.rev $9      
    }}
   
/* Formal parameters used in function declaration */
formals_opt: 
  /* nothing */                                              { [] }
  | formal_list                                              { List.rev $1 }

formal_list:
    formal                                                   { [$1] }
  | formal_list COMMA formal                                 { $3 :: $1 }

/* Formal parameters */
formal:
  any_id ID
  {{
    kind = $1;
    vname = $2;
  }}

any_id:
    custom_types                                             { $1 }
  | basic_types                                              { $1 }

/* Custom types to create Mandalas */
custom_types:
    MANDALA                                                  { Mandalat }
  | LAYER                                                    { Layert }
  | SHAPE                                                    { Shapet }

/* Variable types */
basic_types:
    NUMBER                                                   { Numbert }
  | BOOLEAN                                                  { Booleant }
  | GEO                                                      { Geot }
  | COLOR                                                    { Colort }
  | VOID                                                     { Voidt }

stmt_list:
  /* nothing */                                              { [] }
  | stmt_list stmt                                           { $2 :: $1 }

stmt:
   expr SEMI                                                { Expr($1) }
  | RETURN expr SEMI                                         { Return($2) }
  | FOREACH ID ASSIGN FLOAT_LITERAL TO FLOAT_LITERAL COLON 
    LBRACE stmt_list RBRACE SEMI                                  { Foreach($2, $4, $6, $9) }
  /* Constructor statements for Mandala, Shape and Layer */
  | assign_expr ASSIGN CREATE MANDALA SEMI                   { Mandala($1) }
  | assign_expr ASSIGN CREATE SHAPE COLON LBRACE GEO expr 
    SIZE expr 
    COLOR expr 
    ROTATION expr RBRACE SEMI                                { Shape($1, $8, $10, $12, $14) }
  | assign_expr ASSIGN CREATE LAYER COLON LBRACE RADIUS expr
    SHAPE expr
    COUNT expr
    OFFSET expr 
    ANGULARSHIFT expr RBRACE SEMI                            { Layer($1, $8, $10, $12, $14, $16) }
  | assign_expr ASSIGN expr SEMI                             { Assign($1, $3) }


expr:
    LITERAL                                                  { Literal($1) }
  | FLOAT_LITERAL                                            { Float_Literal($1) }
  | ID                                                       { Id($1) }
  | expr PLUS expr                                           { Binop($1, Add, $3) }
  | expr MINUS expr                                          { Binop($1, Sub, $3) }
  | expr TIMES expr                                          { Binop($1, Mult, $3) }
  | expr DIVIDE expr                                         { Binop($1, Div, $3) }
  | LPAREN expr RPAREN                                       { $2 }
  | ID COLON LPAREN actuals_opt RPAREN                       { Call($1, $4) }

assign_expr:
  any_id ID       
  {{ 
    kind = $1;    
    vname = $2;   
  }}

/* actual parameters passed into functions */
actuals_opt:
  /* nothing */                                              { [] }
  | actuals_list                                             { List.rev $1 }

actuals_list:
    expr                                                     { [$1] }
  | actuals_list COMMA expr                                  { $3 :: $1 }