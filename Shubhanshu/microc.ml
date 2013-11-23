type action = Ast | Interpret | Bytecode | Compile
exception Illegal_character of string * int

let _ =
  let filename = 
  if Array.length Sys.argv > 2 then
    Sys.argv.(2)
  else Sys.argv.(1)
  in
  let action = if Array.length Sys.argv > 2 then
     List.assoc Sys.argv.(1) [ ("-a", Ast);
          ("-i", Interpret);
          ("-c", Compile);
          ("-b", Bytecode)
            ]
  else Compile in
    let lexbuf = Lexing.from_channel (open_in filename) in
    let program = Parser.program Scanner.token lexbuf   in
  match action with

  | Ast -> let listing = Ast.string_of_program (program)
           in print_endline listing
  | Interpret -> (*ignore (Interpret.run (List.rev program))*)print_endline "Not Implemented Yet"
  | Bytecode -> print_endline "Not Implemented Yet"
  | Compile -> print_endline "Not Implemented Yet"


