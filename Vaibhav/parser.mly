%{ open Ast ;;
  
  (*Parsing.set_trace true;;*)
  let error_position () =
  incr Ast.error_count;
  let spos = Parsing.symbol_start_pos() and epos = Parsing.symbol_end_pos() in
      "on line " ^ string_of_int spos.Lexing.pos_lnum ^ " at characters " ^ string_of_int (spos.Lexing.pos_cnum - spos.Lexing.pos_bol) ^ " - " ^ string_of_int (epos.Lexing.pos_cnum - epos.Lexing.pos_bol)



 (* let parse_error s =  incr Ast.error_count; print_string ("Error "^string_of_int !Ast.error_count ^ " : ")*)

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

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
  /* nothing */ { ([], [], !Ast.error_count) }
 | program vdecl_opt {let (a,b,_)=$1 in  (($2 :: a), b ,!Ast.error_count)}
 | program fdefn {let (a,b,_)=$1 in (a, ($2 :: b),!Ast.error_count) }
 /*| program error {print_endline ("Error while parsing " ^ error_position()) ; (fst $1,snd $1)}*/
  

fdefn:
    v_type ID LPAREN formal_opt RPAREN stmt_block
     { { 
      rtype = $1;
      fname = $2;
      formals = $4;
      body = $6 
      } }
  | error LPAREN formal_opt RPAREN stmt_block
     { print_endline ("No return type or illegal identifier for function "  ^ error_position ());{ 
      rtype = "";
      fname = "";
      formals = [];
      body = [] 
      } }
  | error LPAREN error RPAREN stmt_block
     { print_endline ("No return type or illegal identifier for function and illegal args "  ^ error_position ());{ 
      rtype = "";
      fname = "";
      formals = [];
      body = [] 
      } }
  | error LPAREN formal_opt RPAREN error
     { print_endline ("No return type or illegal identifier for function and no statement block "  ^ error_position ());{ 
      rtype = "";
      fname = "";
      formals = [];
      body = [] 
      } }
  | error LPAREN error RPAREN error
     { print_endline ("Everything wrong in the function "  ^ error_position ());{ 
      rtype = "";
      fname = "";
      formals = [];
      body = [] 
      } }
  | v_type ID LPAREN error RPAREN stmt_block
     { print_endline ("Illegal formal arguments "  ^ error_position ());{ 
      rtype = "";
      fname = "";
      formals = [];
      body = [] 
      } }
  | v_type ID LPAREN error RPAREN error
     { print_endline ("Illegal formal arguments and no statement block "  ^ error_position ());{ 
      rtype = "";
      fname = "";
      formals = [];
      body = [] 
      } }
  | v_type ID LPAREN formal_opt RPAREN error
     { print_endline ("Error : No statement block "  ^ error_position ());{ 
      rtype = "";
      fname = "";
      formals = [];
      body = [] 
      } }


vdecl_opt:
  SEMI    { Vdefn("",[]) } 
  | vdecl SEMI  { $1 }
  | error  { print_endline ("Error declaring variable "  ^ error_position ());Vdefn("",[])}
  
vdecl:
    v_type id_list      { Vdefn($1,List.rev $2) }
  | v_type id_list ASSIGN expr   { Vassign($1,List.rev $2,$4) }


id_list:
    ID  { [$1] }
  | id_list COMMA ID { ($3) :: $1  }

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
    LBRACE stmt_list RBRACE  { List.rev $2 } 
  
stmt_list:
    /*nothing*/     { [] }
  | stmt_list stmt  { $2 :: $1 }

expr_opt:
    /*nothing*/  { Noexpr }
  | expr         { $1 }

formal_opt:
    /*nothing*/   { [] }
  | formal_list   { List.rev $1 }

formal_list:
    v_type ID    { [($1,$2)] }
  | formal_list COMMA v_type ID   { ($3,$4) :: $1 }

actual_opt:
    expr_opt   { [$1] }
  | actual_opt COMMA expr  { $3::$1 }

v_expr_opt:
  |  vdecl      { Vdecl($1) } 
  |  expr_opt   { Expr($1) }

stmt:
  v_expr_opt SEMI { Vexpr($1) }
| stmt_block { Block($1) }
| IF LPAREN expr RPAREN stmt %prec NOELSE  { If($3,$5,Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt  { If($3,$5,$7) }
| FOR LPAREN v_expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt { For($3,$5,$7,$9) }
| WHILE LPAREN expr RPAREN stmt { While($3,$5) }
| RETURN expr_opt SEMI  { Return($2) }
| BREAK SEMI { Break }
| CONTINUE SEMI { Continue }
| error    { print_endline ("Error in statement " ^ error_position() ^ " or maybe a missing semicolon in a previous statement"); Vexpr(Expr(Noexpr)) }


expr:
    ID ASSIGN expr { Assign($1,$3) }
  /*| vdecl       { Vdecl($1) }*/
  | ID { Id($1) }
  | INTEGERS { Integers($1) }
  | STRINGS { Strings($1) }
  | FLOATS { Floats($1) }
  | TRUE  { Boolean($1) }
  | FALSE { Boolean($1) }
  | LPAREN expr RPAREN { $2 }
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
  | ID LPAREN actual_opt RPAREN { Call($1,List.rev $3) }
  | OBJECT LPAREN actual_opt RPAREN { Objcall(fst $1,snd $1,List.rev $3) }
  | OBJECT  { Objid(fst $1,snd $1) }
 