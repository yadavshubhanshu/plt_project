open Printf

let check_program (fdecl_list,vdecl_list,errors,filename) =

let file = String.sub filename 0 (String.length filename - 5) ^".cpp"  in
let message = "Hello World, what is up with you today \n" in
let listing = Ast.string_of_program (vdecl_list,fdecl_list) in
 	let outChannel = open_out file in
		fprintf outChannel "%s" listing;
		close_out outChannel