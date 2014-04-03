open Printf
open Sast
open Ast


let string_of_fdecl fdefn =
  fdefn.rtype ^ " " ^ fdefn.fname ^ "(" ^ String.concat ", " (List.map string_of_formal fdefn.formals) ^ ")"



let make_program (varmap,funmap,program,main,filename) =
let convert_program program main global_vars funcs =
("
#include \". /c_source/image.h\"
#include \"./c_source/bmp.h\"
#include \"CImg.h\"
#include <assert.h>
#include <iostream> 
#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
#include <math.h>
#include \"./c_source/extra.h\"
  
using namespace std;

"^

 String.concat ";\n" (List.rev (NameMap.fold (
 	fun id ty a -> ( ty ^" "^ id )::a
 		) global_vars []))   ^";\n\n"^

 String.concat ";\n" (List.rev (NameMap.fold (
 	fun fname fdecl a -> (string_of_fdecl fdecl)::a
 		) funcs []))   ^";\n\n"^
 
 "int main(void){\n"^

 main
(* String.concat "" (List.map string_of_stmt program) ^ *)

 ^"\nreturn 0;\n}\n\n"^

 program
 

)


in 
if !Ast.error_count <> 0  then
	raise (Failure ("Program could not be compile because there are "^string_of_int !Ast.error_count^"  errors"))		
else 
let file = String.sub filename 0 (String.length filename - 5) ^".cpp"  in
let listing = convert_program program main varmap funmap  in
 	let outChannel = open_out file in
		(fprintf outChannel "%s" (listing);
		 close_out outChannel;
		 ignore (Sys.command ("g++  -o " ^  (String.sub filename 0 (String.length filename -5)) ^ " " ^ file ^" -O2 -L/usr/X11R6/lib -lm -lpthread -lX11 -DUSE_UNIX -g -ansi -Wall ./c_source/bmp.cpp ./c_source/pixel.cpp ./c_source/image.cpp ./c_source/vector.cpp " ));
		 print_endline "Program has compiled successfully"				
		)

		 
