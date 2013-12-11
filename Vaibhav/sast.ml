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

let check_program (fdecl_list,vdecl_list,errors) =
	let functions = List.fold_left (fun fmap fdecl -> NameMap.add fdecl.fname fdecl fmap) NameMap.empty fdecl_list
	in

	let globals = List.fold_left (fun varmap vdecl -> (match vdecl with
		| Vassign(ty,id_list,expr) -> (List.fold_left (fun vmap id -> (if (NameMap.mem id vmap) 
			then print_string ("ERROR : variable "^id^" has already been declared in this scope.\n") );
			let rhs = (check_expr vmap functions expr) 
			in if ty = rhs then  (NameMap.add id ty vmap) 
				else ((print_string ("Error : Variable "^id^" has type "^ty^" and RHS has type "^rhs^".\n")); vmap))
				varmap id_list)
		| Vdefn("",_) -> varmap
		| Vdefn(ty,id_list) -> (List.fold_left (fun vmap id -> (if (NameMap.mem id vmap) 
					then print_string ("ERROR : variable "^id^" has already been declared in this scope.\n") );
					NameMap.add id ty vmap) varmap id_list)
			)
		) NameMap.empty vdecl_list
	
	in (globals,functions)