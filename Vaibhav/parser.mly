%{ open Ast ;;
  let error_count = ref 0
  (*Parsing.set_trace true;;*)
  let error_position () =
  let spos = Parsing.symbol_start_pos() and epos = Parsing.symbol_end_pos() in
      "on line " ^ string_of_int spos.Lexing.pos_lnum ^ " at characters " ^ string_of_int (spos.Lexing.pos_cnum - spos.Lexing.pos_bol) ^ " - " ^ string_of_int (epos.Lexing.pos_cnum - epos.Lexing.pos_bol)



 (* let parse_error s =  incr error_count; print_string ("Error "^string_of_int !error_count ^ " : ")*)

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
  /* nothing */ { [], [] }
 | program vdecl_opt { ($2 :: fst $1), snd $1 }
 | program fdefn { fst $1, ($2 :: snd $1) }
 /*| program error {print_endline ("Error while parsing " ^ error_position()) ; (fst $1,snd $1)}*/
  

fdefn:
    v_type ID LPAREN formal_opt RPAREN stmt_block
     { { 
      rtype = $1;
      fname = $2;
      formals = $4;
      body = $6 
      } }
  | error ID LPAREN formal_opt RPAREN stmt_block
     { print_endline ("No return type "  ^ error_position ());{ 
      rtype = "";
      fname = $2;
      formals = $4;
      body = $6 
      } }
 

vdecl_opt:
    SEMI    { Vdefn("",[]) }
  | vdecl   { $1 }
  

vdecl:
    v_type id_list SEMI     { Vdefn($1,List.rev $2) }
  | v_type id_list ASSIGN expr SEMI  { Vassign($1,List.rev $2,$4) }
  | v_type error SEMI   {print_endline ("Illegal identifier "  ^ error_position ());Vdefn("",[])}
  | v_type id_list ASSIGN error SEMI   {print_endline ("Error in RHS "  ^ error_position ());Vdefn("",[])}
  | error SEMI   {print_endline ("Type not declared "  ^ error_position ());Vdefn("",[])}
  

id_list:
    ID  { [$1] }
  | id_list COMMA ID { $3 :: $1  }
/*  | id_list COMMA error { print_endline ("Illegal Identifier " ^ error_position ()); $1 }
  | error { print_endline ("Illegal Identifier "  ^ error_position ()); [] }*/

v_type:
  | VOID    { "void" }
  | INT     { "int" }
  | FLOAT   { "float" }
  | PIXEL   { "pixel" }
  | IMAGE   { "image" }
  | ARRAY   { "array" }
  | STRING  { "string" }
  | BOOL    { "bool" }
 /* | error   { print_endline ("Error :Type unspecified "  ^ error_position ()); "" }*/

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

stmt:
  expr_opt SEMI { Expr($1) }
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
  | ID LPAREN actual_opt RPAREN { Call($1,List.rev $3) }
  | OBJECT LPAREN actual_opt RPAREN { Objcall(fst $1,snd $1,List.rev $3) }
  | OBJECT  { Objid(fst $1,snd $1) }