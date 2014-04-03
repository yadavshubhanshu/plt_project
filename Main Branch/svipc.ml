type action = Ast | Compile | Test
exception Illegal_character of string * int

let _ =
  let (filename,action) = 
  if Array.length Sys.argv > 2 then
    (Sys.argv.(2),List.assoc Sys.argv.(1) [ ("-a", Ast);
          ("-c", Compile);
          ("-t", Test)])
  else (Sys.argv.(1),Compile)
  in
  let len = String.length filename in 
  if String.sub filename (len - 5) 5  = ".svip" then
    let lexbuf = Lexing.from_channel (open_in filename) in
    let program = Parser.program Scanner.token lexbuf   in

    match action with

    | Ast -> let listing = Ast.string_of_program program
             in print_endline listing;
                print_endline ("\nProgram has been parsed with " ^ string_of_int !Ast.error_count ^ " errors.");
    | Test ->   let (_,_,_,_) = Sast.check_program program in
                if !Ast.error_count <> 0  then
                raise (Failure ("Program has "^string_of_int !Ast.error_count^"  errors"))
                else 
                print_endline ("Program has no errors.")

    | Compile -> let (a,b,c,d) = Sast.check_program program in
                   Codegen.make_program (a,b,c,d,filename) 
                
  else print_endline "Error reading the file. It is only possible to compile a '.svip' file"


