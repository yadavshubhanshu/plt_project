{ open Parser
  
 }

let digit = ['0' - '9']
let integer = (digit)+'.'(digit)*('e'('+'|'-')?((digit)+))? 
let fraction = '.'(digit)+('e'(('+'|'-')?)((digit)+))? 
let exponent = (digit)+'e'('+'|'-')?(digit)+



rule token = parse
  [' ' '\t'] { token lexbuf } (* Whitespace *)
| ['\r' '\n'] {Lexing.new_line lexbuf; token lexbuf }  
| "/*"     { comment 0 lexbuf }           (* Comments *)
| "//"[^ '\n' '\r']*['\n' '\r']     {Lexing.new_line lexbuf; token lexbuf }
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
| digit+ as lxm { INTEGERS(int_of_string lxm) }
| (integer|fraction|exponent) as flt {FLOATS(float_of_string flt)}
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| (['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as var_id)'.'(['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as mthd) { OBJECT(var_id,mthd) }
| eof { EOF }
| _ as chr { raise (Failure ("Illegal Character '"^ Char.escaped chr ^ "' at line " ^ string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum)) }

and comment level = parse
  "*/" { if (level=0) then (token lexbuf) else (comment (level-1) lexbuf)}
| "/*" { comment (level+1) lexbuf}
| ['\r' '\n'] {Lexing.new_line lexbuf ; comment level lexbuf }  
| eof  { raise (Failure ("comment not closed"))}
| _    { comment level lexbuf }


