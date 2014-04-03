%{ open Ast ;;
  
  (*Parsing.set_trace true;;*)
  let error_position () =
  incr Ast.error_count;
  let spos = Parsing.symbol_start_pos() and epos = Parsing.symbol_end_pos() in
      "on line " ^ string_of_int spos.Lexing.pos_lnum ^ " at characters " ^ string_of_int (spos.Lexing.pos_cnum - spos.Lexing.pos_bol) ^ " - " ^ string_of_int (epos.Lexing.pos_cnum - epos.Lexing.pos_bol)



 (* let parse_error s =  incr Ast.error_count; print_string ("Error "^string_of_int !Ast.error_count ^ " : ")*)

%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA TO LBRACKET RBRACKET DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ POWER
%token EQ NEQ LT LEQ GT GEQ NOT 
%token RETURN IF ELSE FOR WHILE BREAK CONTINUE IN WITH DO
%token INT IMAGE PIXEL FLOAT STRING BOOL VOID VIDEO
%token <bool> TRUE FALSE
%token <int> INTEGERS
%token <float> FLOATS
%token <string> ID STRINGS
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ
%nonassoc  UMINUS NOT INCR DECR
%left EQ NEQ
%left LT GT LEQ GEQ
%left POWER
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

  

fdefn:
    v_type ID LPAREN formal_opt RPAREN stmt_block
     { { 
      rtype = $1;
      fname = $2;
      formals = $4;
      body = $6 
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

vdecl:
    v_type id_list      { Vdefn($1,List.rev $2) }
  | v_type id_array ASSIGN expr   { Vassign($1,$2,$4) }


id_array:
   ID  { Single($1) }
  |ID LBRACKET expr RBRACKET { Array($1,$3,Integers(0),Integers(0)) }
  |ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET { Array($1,$3,$6,Integers(0)) }
  |ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET LBRACKET expr RBRACKET { Array($1,$3,$6,$9) }


id_list:
    id_array  { [$1] }
  | id_list COMMA id_array { ($3) :: $1  }


v_type:
  | VOID    { "void" }
  | INT     { "int" }
  | FLOAT   { "float" }
  | PIXEL   { "Pixel" }
  | IMAGE   { "Image*" }
  | STRING  { "string" }
  | BOOL    { "bool" }
  | VIDEO   { "Video" }

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
    /*nothing*/  { [] }
  | expr         { [$1] }
  |actual_opt COMMA expr  { $3::$1 }

v_expr_opt:
  |  vdecl      { Vdecl($1) } 
  |  expr_opt   { Expr($1) }

stmt:
  v_expr_opt SEMI { Vexpr($1) }
| stmt_block { Block($1) }
| IF LPAREN expr RPAREN stmt %prec NOELSE  { If($3,$5,Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt  { If($3,$5,$7) }
| FOR LPAREN v_expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt { For($3,$5,$7,$9) }
/*| FOR LPAREN v_expr_opt IN ID RPAREN stmt { For_list($3,$5,$7) }
| FOR LPAREN v_expr_opt IN range RPAREN stmt { For($3,,,$9) }
| FOR LPAREN v_expr_opt IN range WITH expr RPAREN stmt { For($3,,$9,$11) }*/
| WHILE LPAREN expr RPAREN stmt { While($3,$5) }
| DO stmt WHILE LPAREN expr RPAREN { Do_while($5,$2) }
| RETURN expr_opt SEMI  { Return($2) }
| BREAK SEMI { Break }
| CONTINUE SEMI { Continue }
| error    { print_endline ("Error: Syntax error in statement " ^ error_position() ^ " or maybe a missing semicolon in a previous statement"); Vexpr(Expr(Noexpr)) }


range:
  |expr  { (false,$1,Noexpr) }
  |expr TO expr { (true,$1,$3) }

id_array_access:
  | ID LBRACKET range RBRACKET { Var_access(1,Id($1),$3,(false,Noexpr,Noexpr),(false,Noexpr,Noexpr)) }
  | ID LBRACKET range RBRACKET LBRACKET range RBRACKET { Var_access(2,Id($1),$3,$6,(false,Noexpr,Noexpr)) }
  | ID LBRACKET range RBRACKET LBRACKET range RBRACKET LBRACKET range RBRACKET { Var_access(3,Id($1),$3,$6,$9) }
  | LPAREN expr RPAREN LBRACKET range RBRACKET { Var_access(1,Paren($2),$5,(false,Noexpr,Noexpr),(false,Noexpr,Noexpr)) }
  | LPAREN expr RPAREN LBRACKET range RBRACKET LBRACKET range RBRACKET { Var_access(2,Paren($2),$5,$8,(false,Noexpr,Noexpr)) }


integer_list:
| expr  { [$1] }
| integer_list COMMA expr { $3 :: $1 }

expr:
  | ID ASSIGN expr { Assign(Id($1),$3) }
  | id_array_access ASSIGN expr { Assign($1,$3) }
  | objid ASSIGN expr { Assign($1,$3) }
  | binop_assign { $1 }
  | objid     { $1 }
  | LBRACKET integer_list RBRACKET  { Int_list(List.rev $2) }
  | ID     { Id($1) }
  | id_array_access   { $1 }
  | INTEGERS { Integers($1) }
  | STRINGS { Strings($1) }
  | FLOATS { Floats($1) }
  | TRUE  { Boolean($1) }
  | FALSE { Boolean($1) }
  | uop  { $1 }
  | binop { $1 }
  | LPAREN expr RPAREN { Paren($2) }
  | ID LPAREN actual_opt RPAREN { Call($1,List.rev $3) }
  | objid LPAREN actual_opt RPAREN { Objcall($1,List.rev $3) }

objid:
  | LPAREN expr RPAREN DOT ID { Objid($2,$5) }
  | id_array_access DOT ID  { Objid($1,$3) }
  | ID DOT ID   {Objid(Id($1),$3)}


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
  | expr POWER expr   { Binop($1,Pow,$3) }

  binop_assign:
  | ID PLUSEQ expr     { Assign(Id($1),Binop(Id($1),Add,$3)) }
  | id_array_access PLUSEQ expr     { Assign($1,Binop($1,Add,$3)) }
  | ID MINUSEQ expr    { Assign(Id($1),Binop(Id($1),Sub,$3)) }
  | id_array_access MINUSEQ expr    { Assign($1,Binop($1,Sub,$3)) }
  | ID TIMESEQ expr    { Assign(Id($1),Binop(Id($1),Mult,$3)) }
  | id_array_access TIMESEQ expr    { Assign($1,Binop($1,Mult,$3)) }
  | ID DIVIDEEQ expr   { Assign(Id($1),Binop(Id($1),Div,$3)) }
  | id_array_access DIVIDEEQ expr   { Assign($1,Binop($1,Div,$3)) }
  | ID MODEQ expr      { Assign(Id($1),Binop(Id($1),Mod,$3)) }
  | id_array_access MODEQ expr      { Assign($1,Binop($1,Mod,$3)) }
