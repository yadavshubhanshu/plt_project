open Ast

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)


let rec check_expr varmap fmap = (function
| Strings(_) -> "string"
| Integers(_) -> "int"
| Floats(_) -> "float"
| Boolean(_) -> "bool"
| Id(id) -> if (NameMap.mem id varmap) then (NameMap.find id varmap) 
			else ((print_string ("Undeclared variable "^id^".\n"));"void")
| Assign(id,exp) ->  	if (NameMap.mem id varmap) then (let lhs = (NameMap.find id varmap) 
							and rhs = (check_expr varmap fmap exp)
							in (if not(lhs = rhs) then (print_string ("Error : Variable "^id^" has type "^lhs^
							" and RHS has type "^rhs^".\n")));lhs) 
						else ((print_string ("Undeclared variable "^id^".\n"));"void")
| Call("print",actuals) -> 
(*Handle inbuilt print function here*) "void"
| Call(id,actuals) -> 
(if NameMap.mem id fmap then (
	let fdecl = (NameMap.find id fmap) in 
	ignore (
		try (List.fold_left2 (fun () actual formal -> 
			let tactual = check_expr varmap fmap actual and (tformal,formalid) = formal in 
			(if tactual = tformal then () 
				else (print_string ("Illegal argument to function '"^fdecl.fname^"'. The function expected an argument of type "^tformal^" but was provided an argument of type "^tactual^" .\n"))
		)) () actuals fdecl.formals)
    	with Invalid_argument(s) -> print_string ("Number of arguments provided in the call to function '"^fdecl.fname^"'' dont match the function definition.\n") 
    );fdecl.rtype
)
else (print_string ("Undeclared function '"^id^"'.\n");""))
| Binop(e1,o,e2) -> let t1 = check_expr varmap fmap e1 and t2 = check_expr varmap fmap e2 in
					(match o with
					|Add -> (if not(t1 = t2) then print_string ("Error in Binary operator '+' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));t1
					|Sub -> (if not(t1 = t2) then print_string ("Error in Binary operator '-' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));t1
					|Mult -> (if not(t1 = t2) then print_string ("Error in Binary operator '*' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));t1
					|Div -> (if not(t1 = t2) then print_string ("Error in Binary operator '/' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));t1
					|Equal -> (if not(t1 = t2) then print_string ("Error in Binary operator '==' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));"bool"
					|Neq -> (if not(t1 = t2) then print_string ("Error in Binary operator '!=' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));"bool"
					|Less -> (if not(t1 = t2) then print_string ("Error in Binary operator '<' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));"bool"
					|Leq -> (if not(t1 = t2) then print_string ("Error in Binary operator '<=' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));"bool"
					|Greater -> (if not(t1 = t2) then print_string ("Error in Binary operator '>' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));"bool"
					|Geq -> (if not(t1 = t2) then print_string ("Error in Binary operator '>=' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));"bool"
				)
| Objid(_) -> "void"
| Objcall(_) -> "void"
| Noexpr -> "void"
)

let check_vdecl functions varmap vdecl = (match vdecl with
		| Vassign(ty,id_list,expr) -> (if ty <> "void" then (List.fold_left (fun vmap id -> (if (NameMap.mem id vmap) 
			then print_string ("ERROR : variable "^id^" has already been declared in this scope.\n") );
			let rhs = (check_expr vmap functions expr) 
			in if ty = rhs then  (NameMap.add id ty vmap) 
				else (print_string ("Error : Variable "^id^" has type "^ty^" and RHS has type "^rhs^".\n"); vmap))
				varmap id_list)
			else (print_string ("Error : Variable(s) "^(String.concat "," id_list)^" cannot have type void.\n"); varmap))
		| Vdefn("",_) -> varmap
		| Vdefn(ty,id_list) -> (if ty <> "void" then (List.fold_left (fun vmap id -> (if (NameMap.mem id vmap) 
					then print_string ("ERROR : variable "^id^" has already been declared in this scope.\n") );
					NameMap.add id ty vmap) varmap id_list)
			else (print_string ("Error : Variable(s) "^(String.concat "," id_list)^" cannot have type void.\n"); varmap))
		)

let check_vexpr vmap fmap = (function
| Expr(e) -> check_expr vmap fmap e;"expr\n"
| Vdecl(v) -> check_vdecl fmap vmap v ;"vdecl\n")

(*add check stmts to ...write chks for the FOR stmt and vexpr *)
let rec check_stmt vmap fmap current_func= (function
| Block(stmt_list)  -> List.fold_left (fun (v,f) stmt -> check_stmt v f current_func stmt) (vmap,fmap) stmt_list
| Vexpr(vexpr) -> print_string (check_vexpr vmap fmap vexpr); (vmap,fmap)
| Return(expr) -> let ty = (check_expr vmap fmap expr) in if current_func.rtype = ty then (vmap,fmap) else (print_string ("Expression is of type "^ty^" but expected return type is "^current_func.rtype^".\n"); (vmap,fmap) )
| Break -> (vmap,fmap)
| Continue -> (vmap,fmap)
| If(predicate,then_blk,Block([])) -> let tpredicate = (check_expr vmap fmap predicate) in if tpredicate = "bool" then 
					(check_stmt vmap fmap current_func then_blk) else (print_string ("The predicate of an If statement should be a boolean expression not "^tpredicate^".\n") ;(vmap,fmap))
| If(predicate,then_blk,else_blk) -> let tpredicate = (check_expr vmap fmap predicate) in if tpredicate = "bool" then 
					(ignore (check_stmt vmap fmap current_func then_blk);(check_stmt vmap fmap current_func else_blk)) else (print_string ("The predicate of an If statement should be a boolean expression not "^tpredicate^".\n") ;(vmap,fmap))
| For(v_initialize,condition,step,stmt_blk) -> (vmap,fmap)
| While(predicate,stmt_blk) -> let tpredicate = (check_expr vmap fmap predicate) in if tpredicate = "bool" then (check_stmt vmap fmap current_func stmt_blk) else (print_string ("The predicate of an While statement should be a boolean expression not "^tpredicate^".\n") ;(vmap,fmap))
)

let check_function fdecl vmap fmap = 
	List.map (fun stmt -> check_stmt vmap fmap fdecl stmt) fdecl.body



let check_program (fdecl_list,vdecl_list,errors) =
let inbuilt_functions = 
	NameMap.add "print" {rtype = "void" ; fname = "print"; formals = []; body = []} NameMap.empty
	in
	let functions = List.fold_left (fun fmap fdecl -> (if (NameMap.mem fdecl.fname fmap) 
			then print_string ("ERROR : Either the function '"^fdecl.fname^"' has already been declared or you have tried to declare an inbuilt function.\n") );
				(check_function fdecl NameMap.empty fmap); NameMap.add fdecl.fname fdecl fmap) inbuilt_functions fdecl_list
	in

	let globals = List.fold_left (check_vdecl functions) NameMap.empty vdecl_list
	
	in 
	(if not(NameMap.mem "main" functions) then print_string "Error : There is no function 'main'!!! Where do you expect us to start the execution of the program??");(globals,functions)