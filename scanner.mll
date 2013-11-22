{ open Parser
 let line = ref 1
 let errors = ref 0
 let error_list = []
 exception Illegal_character of string * int
 }

rule token = parse
  [' ' '\t'] { token lexbuf } (* Whitespace *)
| ['\r' '\n'] {incr line ; token lexbuf }  
| "/*"     { comment lexbuf }           (* Comments *)
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
| '"'([^'"']+ as str)'"'	{ STRINGS(str) }
| ['0'-'9']+ as lxm { INTEGERS(int_of_string lxm) }
| (['0'-'9']*'.'['0'-'9']+)|(['0'-'9']+'.'['0'-'9']*) as flt {FLOATS(float_of_string flt)}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as chr { ERROR(Char.escaped chr,!line) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

{


}