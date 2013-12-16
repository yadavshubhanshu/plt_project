%{ open Ast ;;
  
  (*Parsing.set_trace true;;*)
  let error_position () =
  incr Ast.error_count;
  let spos = Parsing.symbol_start_pos() and epos = Parsing.symbol_end_pos() in
      "on line " ^ string_of_int spos.Lexing.pos_lnum ^ " at characters " ^ string_of_int (spos.Lexing.pos_cnum - spos.Lexing.pos_bol) ^ " - " ^ string_of_int (epos.Lexing.pos_cnum - epos.Lexing.pos_bol)



 (* let parse_error s =  incr Ast.error_count; print_string ("Error "^string_of_int !Ast.error_count ^ " : ")*)

%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA TO LBRACKET RBRACKET DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ
%token EQ NEQ LT LEQ GT GEQ NOT 
%token RETURN IF ELSE FOR WHILE BREAK CONTINUE IN WITH DO
%token INT IMAGE PIXEL FLOAT STRING BOOL VOID VIDEO
%token <bool> TRUE FALSE
%token <int> INTEGERS
%token <float> FLOATS
%token <string> ID STRINGS
%token <string * string> OBJECT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ
%nonassoc  UMINUS NOT INCR DECR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE 
%left MOD

%start program
%type <Ast.program> program

%%
program:
  rev_program { List.rev $1 }

rev_program:
  /* nothing */ { [] }
 | rev_program stmt { Stmt($2)::$1 }
 | rev_program fdefn { Fdefn($2)::$1 }
 /*| program error {print_endline ("Error while parsing " ^ error_position()) ; (fst $1,snd $1)}*/
  

fdefn:
    v_type ID LPAREN formal_opt RPAREN stmt_block
     { { 
      rtype = $1;
      fname = $2;
      formals = $4;
      body = $6 
      } }
/*  | error LPAREN formal_opt RPAREN stmt_block
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
*/
/*
vdecl_opt:
  SEMI    { Vdefn("",[]) } 
  | vdecl SEMI  { $1 }
  | error  { print_endline ("Error declaring variable "  ^ error_position ());Vdefn("",[])}
*/  
vdecl:
    v_type id_list      { Vdefn($1,List.rev $2) }
  | v_type id_list ASSIGN expr   { Vassign($1,List.rev $2,$4) }


id_array:
   ID  { Single($1) }
  |ID LBRACKET INTEGERS RBRACKET { Array($1,$3,0) }
  |ID LBRACKET INTEGERS RBRACKET LBRACKET INTEGERS RBRACKET { Array($1,$3,$6) }

id_array_access:
   ID  { ASingle($1) }
  |ID LBRACKET range RBRACKET { AArray($1,$3,Range(0,0)) }
  |ID LBRACKET range RBRACKET LBRACKET range RBRACKET { AArray($1,$3,$6) }

range:
|INTEGERS   { Index($1) }
|INTEGERS TO INTEGERS     { Range($1,$3) }
|TO     { Range(0,-1) }

id_list:
    id_array  { [$1] }
  | id_list COMMA id_array { ($3) :: $1  }


v_type:
  | VOID    { "void" }
  | INT     { "int" }
  | FLOAT   { "float" }
  | PIXEL   { "pixel" }
  | IMAGE   { "image" }
  | STRING  { "string" }
  | BOOL    { "bool" }
  | VIDEO   { "video" }

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
    v_type id_array    { [($1,$2)] }
  | formal_list COMMA v_type id_array   { ($3,$4) :: $1 }

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
| FOR LPAREN v_expr_opt IN id_array_access RPAREN stmt { For_list($3,$5,$7) }
| WHILE LPAREN expr RPAREN stmt { While($3,$5) }
| DO stmt WHILE LPAREN expr RPAREN { Do_while($5,$2) }
| RETURN expr_opt SEMI  { Return($2) }
| BREAK SEMI { Break }
| CONTINUE SEMI { Continue }
| error    { print_endline ("Error: Syntax error in statement " ^ error_position() ^ " or maybe a missing semicolon in a previous statement"); Vexpr(Expr(Noexpr)) }


expr:
    id_array_access ASSIGN expr { Assign($1,$3) }
  | binop_assign { $1 }
  /*| vdecl       { Vdecl($1) }*/
  | id_array_access { Id($1) }
  | INTEGERS { Integers($1) }
  | STRINGS { Strings($1) }
  | FLOATS { Floats($1) }
  | TRUE  { Boolean($1) }
  | FALSE { Boolean($1) }
  | uop  { $1 }
  | binop { $1 }
  | LPAREN expr RPAREN { Paren($2) }
  | ID LPAREN actual_opt RPAREN { Call($1,List.rev $3) }
  | OBJECT LPAREN actual_opt RPAREN { Objcall(fst $1,snd $1,List.rev $3) }
  | OBJECT  { Objid(fst $1,snd $1) }

uop:
  | MINUS expr  %prec UMINUS { Uop(Sub,$2) }
  | NOT expr  %prec NOT { Uop(Not,$2) }


binop:
  | expr PLUS expr     { Binop($1, Add , $3) }
  | expr MINUS expr    { Binop($1, Sub , $3) }
  | expr TIMES expr    { Binop($1, Mult , $3) }
  | expr DIVIDE expr   { Binop($1, Div , $3) }
  | expr MOD expr       { Binop($1, Mod , $3) }
  | expr EQ expr       { Binop($1, Equal , $3) }
  | expr NEQ expr      { Binop($1, Neq , $3) }
  | expr LT  expr      { Binop($1, Less , $3) }
  | expr LEQ expr      { Binop($1, Leq , $3) }
  | expr GT expr     { Binop($1, Greater , $3) }
  | expr GEQ expr     { Binop($1,Geq , $3) }

  binop_assign:
  | id_array_access PLUSEQ expr     { Assign($1,Binop(Id($1),Add,$3)) }
  | id_array_access MINUSEQ expr    { Assign($1,Binop(Id($1),Sub,$3)) }
  | id_array_access TIMESEQ expr    { Assign($1,Binop(Id($1),Mult,$3)) }
  | id_array_access DIVIDEEQ expr   { Assign($1,Binop(Id($1),Div,$3)) }
  | id_array_access MODEQ expr      { Assign($1,Binop(Id($1),Mod,$3)) }
