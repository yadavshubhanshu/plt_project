%{ open Ast 
  let parse_error s =  print_endline s(*("parse error on line " ^ (string_of_int start_pos.pos_lnum));*)
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE BREAK CONTINUE
%token INT IMAGE PIXEL FLOAT STRING BOOL ARRAY VOID
%token <bool> TRUE FALSE
%token <int> INTEGERS
%token <float> FLOATS
%token <string> ID STRINGS
%token <string * string> OBJECT
%token EOF

%nonassoc NOELSE NOASSIGN OBJVAR
%nonassoc ELSE OBJCALL
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
  /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdefn { fst $1, ($2 :: snd $1) }
 | program error {(*print_endline ("Error while parsing ") ;*) (fst $1,snd $1)}
  

fdefn:
    v_type ID LPAREN formal_opt RPAREN stmt_block
     { { 
      rtype = $1;
      fname = $2;
      formals = $4;
      body = (List.rev $6) 
      } }

vdecl:
    v_type id_list SEMI %prec NOASSIGN     { Vdefn($1,$2) }
  | v_type id_list ASSIGN expr SEMI  { Vassign($1,$2,$4) }

id_list:
    ID  { [$1] }
  | id_list COMMA ID { $3 :: $1  }

v_type:
  | VOID    { "void" }
  | INT     { "int" }
  | FLOAT   { "float" }
  | PIXEL   { "pixel" }
  | IMAGE   { "image" }
  | ARRAY   { "array" }
  | STRING  { "string" }
  | BOOL    { "bool" }

stmt_block:
    LBRACE stmt_list RBRACE  { $2 }

stmt_list:
    /*nothing*/     { [] }
  | stmt_list stmt  { $2 :: $1 }

expr_opt:
    /*nothing*/  { Noexpr }
  | expr         { $1 }

formal_opt:
    /*nothing*/   { [] }
  | formal_list   { $1 }

formal_list:
    v_type ID    { [($1,$2)] }
  | formal_list COMMA v_type ID   { ($3,$4) :: $1 }

actual_opt:
    expr_opt   { [$1] }
  | actual_opt COMMA expr  { $3::$1 }

stmt:
  expr SEMI { Expr($1) }
| vdecl { Vdecl($1) }
| stmt_block { Block($1) }
| IF LPAREN expr RPAREN stmt %prec NOELSE  { If($3,$5,Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt  { If($3,$5,$7) }
| FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt { For($3,$5,$7,$9) }
| WHILE LPAREN expr RPAREN stmt { While($3,$5) }
| RETURN expr_opt SEMI  { Return($2) }
| BREAK SEMI { Break }
| CONTINUE SEMI { Continue }

expr:
    ID ASSIGN expr { Assign($1,$3) }
  | ID { Id($1) }
  | INTEGERS { Integers($1) }
  | STRINGS { Strings($1) }
  | FLOATS { Floats($1) }
  | TRUE  { Boolean($1) }
  | FALSE { Boolean($1) }
  | expr PLUS   expr { Binop($1, Add,    $3) }
  | expr MINUS  expr { Binop($1, Sub,    $3) }
  | expr TIMES  expr { Binop($1, Mult,   $3) }
  | expr DIVIDE expr { Binop($1, Div,    $3) }
  | expr EQ     expr { Binop($1, Equal,  $3) }
  | expr NEQ    expr { Binop($1, Neq,    $3) }
  | expr LT     expr { Binop($1, Less,   $3) }
  | expr LEQ    expr { Binop($1, Leq,    $3) }
  | expr GT     expr { Binop($1, Greater,$3) }
  | expr GEQ    expr { Binop($1, Geq,    $3) }
  | ID LPAREN actual_opt RPAREN { Call($1,$3) }
  | OBJECT LPAREN actual_opt RPAREN %prec OBJCALL { Objcall(fst $1,snd $1,$3) }
  | OBJECT %prec OBJVAR { Objid(fst $1,snd $1) }