type action = Ast | Interpret | Bytecode | Compile | Sast
exception Illegal_character of string * int

let _ =
  let (filename,action) = 
  if Array.length Sys.argv > 2 then
    (Sys.argv.(2),List.assoc Sys.argv.(1) [ ("-a", Ast);
          ("-i", Interpret);
          ("-c", Compile);
          ("-b", Bytecode);
          ("-s", Sast)])
  else (Sys.argv.(1),Compile)
  in
  let len = String.length filename in 
  if String.sub filename (len - 5) (len - 1)  = ".svip" then
    let lexbuf = Lexing.from_channel (open_in filename) in
    let program = Parser.program Scanner.token lexbuf   in
    match action with

    | Ast ->let (vdecl,fdecl,errors) = program in 
            let listing = Ast.string_of_program (List.rev (vdecl),List.rev (fdecl))
             in print_endline listing;
                print_endline ("\nProgram has been parsed with " ^ string_of_int !Ast.error_count ^ " errors.");
    | Sast -> let (vdecl,fdecl,errors) = program in 
              let s_program = Sast.check_program (List.rev (fdecl),List.rev (vdecl),errors) in 
                print_endline ("\nProgram has been semantically checked with " ^ string_of_int !Ast.error_count ^ " errors.");

    | Interpret -> (*ignore (Interpret.run (List.rev program))*)print_endline "Not Implemented Yet"
    | Bytecode -> print_endline "Program has been parsed"
    | Compile -> let (vdecl,fdecl,errors) = program in 
              let c_program = Codegen.check_program (List.rev (fdecl),List.rev (vdecl),errors,filename) in 
                print_endline "C code generated"
  else print_endline "Error reading the file. It is only possible to compile a '.svip' file"


