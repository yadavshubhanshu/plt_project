{ open Parser }

let digit = ['0' - '9']
let integer = (digit)+'.'(digit)*('e'('+'|'-')?((digit)+))? 
let fraction = '.'(digit)+('e'(('+'|'-')?)((digit)+))? 
let exponent = (digit)+'e'('+'|'-')?(digit)+
let identifier = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule token = parse
  [' ' '\t'] { token lexbuf } (* Whitespace *)
| ['\r' '\n'] {Lexing.new_line lexbuf; token lexbuf }  
| "/*"     { comment 0 lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum lexbuf }
| "//" [^ '\n' '\r']* eof     { EOF }           (* Comments *)
| "//"[^ '\n' '\r']*['\n' '\r']     {Lexing.new_line lexbuf; token lexbuf }
| '('		{ LPAREN }
| ')'		{ RPAREN }
| '{'		{ LBRACE }
| '}'		{ RBRACE }
| '['		{ LBRACKET }
| ']'		{ RBRACKET }
| ';'		{ SEMI }
| ':'		{ TO }
| ','		{ COMMA }
| '.'		{ DOT }
| '+'		{ PLUS }
| '-'		{ MINUS }
| '*'		{ TIMES }
| '/'		{ DIVIDE }
| '='		{ ASSIGN }
| '^'		{ POWER }
| "=="		{ EQ }
| "!="		{ NEQ }
| '<'		{ LT }
| '>'		{ GT }
| "<="		{ LEQ }
| ">="		{ GEQ }
| '%'		{ MOD }
| "-="		{ MINUSEQ }
| "*="		{ TIMESEQ }
| "/="		{ DIVIDEEQ }
| "%="		{ MODEQ }
| "+="		{ PLUSEQ }
| '!'		{ NOT }
| "in"		{ IN }
| "with"	{ WITH }
| "if"		{ IF }
| "else"	{ ELSE }
| "for"		{ FOR }
| "do"		{ DO }
| "while"	{ WHILE }
| "return"	{ RETURN }
| "int"		{ INT }
| "image"	{ IMAGE }
| "pixel"	{ PIXEL }
| "float"	{ FLOAT }
| "string"	{ STRING }
| "bool"	{ BOOL }
| "video"	{ VIDEO }
| "true"	{ TRUE(true) }
| "false"	{ FALSE(false) }
| "void"	{ VOID }
| "break"	{ BREAK }
| "continue"{ CONTINUE}
| '"'([^'"']* as str)'"'						{ STRINGS(str) }
| digit+ as lxm 								{ INTEGERS(int_of_string lxm) }
| (integer|fraction|exponent) as flt 			{ FLOATS(float_of_string flt) }
| identifier as lxm 							{ ID(lxm) }
| eof 		{ EOF }
| _ as chr 	{ print_endline ("Illegal Character '"^ Char.escaped chr ^ "' at line " ^ string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum); incr Ast.error_count; token lexbuf }

and comment level start_line = parse
  "*/" { if (level=0) then (token lexbuf) else (comment (level-1) start_line lexbuf)}
| "/*" { comment (level+1) start_line lexbuf}
| ['\r' '\n'] {Lexing.new_line lexbuf ; comment level start_line lexbuf }  
| eof  { print_endline ("A comment started on line "^string_of_int start_line^" was not closed. Or maybe a comment nested within the first one was not closed.");EOF}
| _    { comment level start_line lexbuf }