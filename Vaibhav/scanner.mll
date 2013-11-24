{ open Parser
	let line = ref 1
(*	type position = {
     pos_fname : string;	(* file name *)
     pos_lnum : int;		(* line number *)
     pos_bol : int;		(* the offset of the beginning of the line *)
     pos_cnum : int;		(* the offset of the position *)
  } 
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
  ;;*)
 }

rule token = parse
  [' ' '\t'] { token lexbuf } (* Whitespace *)
| ['\r' '\n'] {(*incr_linenum lexbuf ;*) incr line;token lexbuf }  
| "/*"     { comment 0 lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "image"  { IMAGE }
| "pixel"  { PIXEL }
| "float"  { FLOAT }
| "string" { STRING }
| "bool"   { BOOL }
| "true"   { TRUE(true) }
| "false"  { FALSE(false) }
| "void"   { VOID }
| "break"  	 	{ BREAK }
| "continue" 	{ CONTINUE}
| "Array"  		{ ARRAY }
| '"'([^'"']+ as str)'"'	{ STRINGS(str) }
| ['0'-'9']+ as lxm { INTEGERS(int_of_string lxm) }
| (['0'-'9']*'.'['0'-'9']+)|(['0'-'9']+'.'['0'-'9']*) as flt {FLOATS(float_of_string flt)}
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| (['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as var_id)'.'(['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as mthd) { OBJECT(var_id,mthd) }
| eof { EOF }
| _ as chr { raise (Failure ("Illegal Character '"^ Char.escaped chr ^ "' at line " ^ string_of_int !line)) }

and comment level = parse
  "*/" { if (level=0) then (token lexbuf) else (comment (level-1) lexbuf)}
| "/*" { comment (level+1) lexbuf}
| ['\r' '\n'] {(*incr_linenum lexbuf ;*)incr line; comment level lexbuf }  
| eof  { raise (Failure ("comment not closed"))}
| _    { comment level lexbuf }


