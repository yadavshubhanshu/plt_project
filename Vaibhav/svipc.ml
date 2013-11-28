type action = Ast | Interpret | Bytecode | Compile
exception Illegal_character of string * int

let _ =
  let (filename,action) = 
  if Array.length Sys.argv > 2 then
    (Sys.argv.(2),List.assoc Sys.argv.(1) [ ("-a", Ast);
          ("-i", Interpret);
          ("-c", Compile);
          ("-b", Bytecode)
            ])
  else (Sys.argv.(1),Compile)
  in
  let len = String.length filename in 
  if filename.[len-5]='.' && filename.[len-4]='s' && filename.[len-3]='v' && filename.[len-2]='i' && filename.[len-1]='p' then
    let lexbuf = Lexing.from_channel (open_in filename) in
    let program = Parser.program Scanner.token lexbuf   in
    match action with

    | Ast -> (*let listing = Ast.string_of_program (List.rev (fst program),List.rev (snd program))
             in print_endline listing;*)
          let (_,_,errors) = program in
             print_endline ("Program has beed parsed with " ^ string_of_int errors ^ " errors.")
    | Interpret -> (*ignore (Interpret.run (List.rev program))*)print_endline "Not Implemented Yet"
    | Bytecode -> print_endline "Program has been parsed"
    | Compile -> print_endline "Program has been parsed"
  else print_endline "Error reading the file. It is only possible to compile a '.svip' file"


