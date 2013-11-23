{ open Parser
 let line = ref 1
 let errors = ref 0
 let error_list = []
 exception Illegal_character of string * int
 }

rule token = parse
  [' ' '\t'] { token lexbuf } (* Whitespace *)
| ['\r' '\n'] {incr line ; token lexbuf }  
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
| "true"   { TRUE }
| "false"  { FALSE }
| "break"  	 	{ BREAK }
| "continue" 	{ CONTINUE}
| "Array"  		{ ARRAY }
| '"'([^'"']+ as str)'"'	{ STRINGS(str) }
| ['0'-'9']+ as lxm { INTEGERS(int_of_string lxm) }
| (['0'-'9']*'.'['0'-'9']+)|(['0'-'9']+'.'['0'-'9']*) as flt {FLOATS(float_of_string flt)}
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as chr { raise (Failure ("Illegal Character '"^ Char.escaped chr ^ "' at line " ^ string_of_int !line)) }

and comment level = parse
  "*/" { if (level=0) then (token lexbuf) else (comment (level-1) lexbuf)}
| "/*" { comment (level+1) lexbuf}
| ['\r' '\n'] {incr line ; comment level lexbuf }  
| eof  { raise (Failure ("comment not closed"))}
| _    { comment level lexbuf }
