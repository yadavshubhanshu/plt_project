open Ast


module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

type env = {
    functions : func_decl NameMap.t; (* Index for each function *)
    globals   : string NameMap.t; (* "Address" for global variables *)
    locals    : string NameMap.t; (* FP offset for args, locals *)
  }


(*return result type of expr*)
let check_expr env ex = match ex with
 Assign(id) -> (let ty = 
 				if NameMap.mem id env.globals then 
						NameMap.find fst(id) env.globals
				else if NameMap.mem fst(id) env.locals then
						NameMap.find fst(id) env.locals
				else (print_endline ("Undeclared variable "^ fst(id) ^".");"")
				in
					if ty = check_expr env snd(id) then 
						ty 
					else (print_endline ("Types dont match in assignment to variable "^ fst(id) ^". Variable is of type "^ty^" and the rhs has type "^check_expr env snd(id)^" .");ty)
				)
| Id(id) -> (if NameMap.mem id env.globals then 
						NameMap.find fst(id) env.globals
				else if NameMap.mem fst(id) env.locals then
						NameMap.find fst(id) env.locals
				else (print_endline ("Undeclared variable "^ fst(id) ^".");"")
			)
| Integers() -> "int"
| Strings() -> "string"
| Floats() -> "float"
| Boolean() -> "bool"
| Binop(e1,op,e2) -> (let t1 = check_expr env e1 and t2 = check_expr env e2 in(*return type of expr after checking if op is done properly*)
						if t1 = t2 then t1
						else (print_endline ("Operation types dont match.");t1)
					)
| Objcall() -> "void"(*later*)
| Objid() -> "void" (*later*)
| _ -> "void"


let check_stmt stmt env= env

let exec_call fdecl env = 
		let stmt_list = fdecl.body in
		let env = List.fold_left (fun env stmt -> exec_stmt stmt env) stmt_list in
		let last = List.tl stmt_list in match last with
			| Return(ret_param) -> 	if check_expr env ret_param = fdecl.r_type then	fdecl.r_type
					else (incr Ast.error_count; print_endline ("Error in return statement ... check if the expression has type \""^fdecl.r_type^"\" ."); "" )
			| _ -> if fdecl.r_type = "void" then "void"
				else (incr Ast.error_count; print_endline ("Missing return statement in function \""^fdecl.fname ^ "\" ."); "" )
			


  (* Invoke a function and return an updated global symbol table *)
let check_call fdecl actuals env =
	let return =     (* Enter the function: bind actual values to formal arguments *)
    try ({env with locals =	List.fold_left2 (fun locals (ty,id) value -> 
							if ty = (check_expr env value) then NameMap.add id ty env.locals
							else (incr Ast.print_endline ("Error in assignment of variable (argument "^ id ^" of function \"" ^ fdecl.fname ^"\") ");env.locals)
						  ) env.locals fdecl.formals actuals};
    					exec_call fdecl env)
				        with Invalid_argument(_) ->
							(print_endline ("Error in call to function \"" ^ fdecl.fname ^ "\". Wrong number of arguments.");"void")



let check_program (fdecl_list,vdecl_list,errors) =
	let varMap = List.fold_left (fun vmap vdecl -> (match vdecl with
				|Vdefn(ty,id_list)  -> if ty <> "" then List.fold_left (fun map id -> NameMap.add id ty map) vmap id_list
							else vmap
				|Vassign(ty,id_list,init_expr) -> if ty = check_expr ({ locals = NameMap.empty ; globals = varMap ; functions = NameMap.empty }) init_expr then List.fold_left (fun map id -> NameMap.add id ty map) vmap id_list
							else (print_endline ("Error in assignment of variable(s) " ^ String.concat "," id_list ^ " ."); vmap)
						)
	) NameMap.empty vdecl_list
	and funcMap = List.fold_left (fun fmap fdecl -> NameMap.add fdecl.fname fdecl fmap)
	 NameMap.empty fdecl_list
	in 
	let env = { locals = NameMap.empty ; globals = varMap ; functions = funcMap }  in
	if (NameMap.mem "main" funcMap) then ( (check_call (NameMap.find "main" funcMap) [] env);print_endline "Program has been parsed with "^string_of_int !Ast.error_count^" errors. Now checking the program for semantic errors ..." )
	else (print_endline ("did not find the function \"main()\""));
