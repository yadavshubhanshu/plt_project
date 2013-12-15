open Printf

let check_program (program,filename) =

let file = String.sub filename 0 (String.length filename - 5) ^".cpp"  in
let listing = Ast.string_of_program program in
 	let outChannel = open_out file in
		fprintf outChannel "%s" ("#include<stdio.h>\n\n"^listing);
		close_out outChannel;
		if !Ast.error_count = 0  then
		print_int (Sys.command ("g++ -o " ^ (String.sub filename 0 (String.length filename -5)) ^ " " ^ file))
		else
		print_string ("Program could not be compile due to errors");		