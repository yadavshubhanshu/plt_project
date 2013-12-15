open Ast

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

type globals = 
{
	funMap : func_decl NameMap.t ;	(*all functions declared*)
	varMap : string NameMap.t ;	(*all global variables*)	
}

type locals =
{
	outMap : string NameMap.t ;	(*variables outside of the current scope*)
	currMap : string NameMap.t ;	(*variables in the current scope*)
}


(*
type env = globals * locals * program

let rec check_expr varmap fmap = (function
| Strings(_) -> "string"
| Integers(_) -> "int"
| Floats(_) -> "float"
| Boolean(_) -> "bool"
| Id(id) -> if (NameMap.mem id varmap) then (NameMap.find id varmap) 
			else ((print_string ("Undeclared variable "^id^".\n"));incr Ast.error_count;"void")
| Assign(id,exp) ->  	if (NameMap.mem id varmap) then (let lhs = (NameMap.find id varmap) 
							and rhs = (check_expr varmap fmap exp)
							in (if not(lhs = rhs) then (print_string ("Error : Variable "^id^" has type "^lhs^
							" and RHS has type "^rhs^".\n")));incr Ast.error_count;lhs) 
						else ((print_string ("Undeclared variable "^id^".\n"));incr Ast.error_count;"void")
| Call("print",actuals) -> 
(*Handle inbuilt print function here*) "void"
| Call(id,actuals) -> 
(if NameMap.mem id fmap then (
	let fdecl = (NameMap.find id fmap) in 
	ignore (
		try (List.fold_left2 (fun () actual formal -> 
			let tactual = check_expr varmap fmap actual and (tformal,formalid) = formal in 
			(if tactual = tformal then () 
				else (print_string ("Illegal argument to function '"^fdecl.fname^"'. The function expected an argument of type "^tformal^" but was provided an argument of type "^tactual^" .\n");incr Ast.error_count)
		)) () actuals fdecl.formals)
    	with Invalid_argument(s) -> print_string ("Number of arguments provided in the call to function '"^fdecl.fname^"'' dont match the function definition.\n") 
    );incr Ast.error_count;fdecl.rtype)
else (print_string ("Undeclared function '"^id^"'.\n");incr Ast.error_count;""))
| Binop(e1,o,e2) -> let t1 = check_expr varmap fmap e1 and t2 = check_expr varmap fmap e2 in
					(match o with
					|Add -> (if t1 <> t2 then print_string ("Error in Binary operator '+' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;t1
					|Sub -> (if t1 <> t2 then print_string ("Error in Binary operator '-' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;t1
					|Mult -> (if t1 <> t2 then print_string ("Error in Binary operator '*' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;t1
					|Div -> (if t1 <> t2 then print_string ("Error in Binary operator '/' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;t1
					|Equal -> (if t1 <> t2 then print_string ("Error in Binary operator '==' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;"bool"
					|Neq -> (if t1 <> t2 then print_string ("Error in Binary operator '!=' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;"bool"
					|Less -> (if t1 <> t2 then print_string ("Error in Binary operator '<' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;"bool"
					|Leq -> (if t1 <> t2 then print_string ("Error in Binary operator '<=' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;"bool"
					|Greater -> (if t1 <> t2 then print_string ("Error in Binary operator '>' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;"bool"
					|Geq -> (if t1 <> t2 then print_string ("Error in Binary operator '>=' expression on the left has type "^t1^" and expression on the right has type "^t2^".\n"));incr Ast.error_count;"bool"
				)
| Objid(_) -> "void"
| Objcall(_) -> "void"
| Noexpr -> "void"
)

let check_function fdecl vmap fmap = 
	List.map (fun stmt -> check_stmt vmap fmap fdecl stmt) fdecl.body
*)
(*
	check_function
	check_expr
*)

let rec check_expr expr = ""


let addMap map1 map2 = 
NameMap.merge (fun k xo yo -> match xo,yo with
    | Some x, Some y -> Some y
    | None, yo -> yo
    | xo, None -> xo
  ) map1 map2


let check_vdecl locals = function
	| Vdefn(ty,id_list) -> 
		if ty <> "void" then ({locals with currMap =
			List.fold_left (  
				fun vmap id_array -> 
					(match id_array with
						|Single(id) -> if NameMap.mem id vmap then (
										print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
										incr Ast.error_count;
										vmap
										) 
									   else NameMap.add id ty vmap
						|Array(id,i,j) -> if NameMap.mem id vmap then (
											print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
											incr Ast.error_count;
											vmap
											) 
									  	  else NameMap.add id ty vmap
					)
			) locals.currMap id_list
	 	})
		else (print_endline ("ERROR : Variable(s) "^(Ast.string_of_id_list id_list)^" cannot have type void.");
			  incr Ast.error_count;
			  locals)
		
	| Vassign(ty,id_list,expr) ->
		if ty <> "void" then (
			let rhs = check_expr (***********) expr in
			if ty = rhs then ({locals with currMap = 
				List.fold_left (  
					fun vmap id_array -> 
						(match id_array with
							|Single(id) -> if NameMap.mem id vmap then (
											print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
											incr Ast.error_count;
											vmap
											) 
										   else NameMap.add id ty vmap
							|Array(id,i,j) -> if NameMap.mem id vmap then (
												print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
												incr Ast.error_count;
													vmap
												) 
											  else NameMap.add id ty vmap
						)
				) locals.currMap id_list
			})
			else (print_endline ("ERROR : Variable(s) "^(Ast.string_of_id_list id_list)^" has(have) type "^ty^" and RHS has type "^rhs^".");
				  incr Ast.error_count;
				  locals)
	 	)
		else (print_endline ("ERROR : Variable(s) "^(Ast.string_of_id_list id_list)^" cannot have type void.");
			  incr Ast.error_count;
			  locals)
		


(*check_stmt function takes global and local environments and current return type and
 returns a modified local environment environment*)
(*
for loops
*)

let rec check_stmt (globals,locals) current_func = (function
| For(v_initialize,condition,step,stmt_blk) -> (*modify locals and then check statements then return modified local before stmt checks*)
												locals
| For_list(v_initialize,array_id,stmt_blk) -> (*Same as other for loop*)locals
| Vexpr(Vdecl(vdecl)) ->  check_vdecl locals vdecl
| Vexpr(Expr(expr)) -> ignore (check_expr (**************) expr);locals
| Block(stmt_list)  -> let locals = {outMap = addMap locals.outMap locals.currMap; currMap = NameMap.empty} in 
	List.fold_left (fun local stmt -> check_stmt (globals,local) current_func stmt) locals stmt_list
| Return(expr) -> 
	let ty = (check_expr (**************) expr) in 
		if current_func = ty then locals 
		else (print_string ("ERROR: Expression is of type "^ty^" but expected return type is "^current_func^".\n");
	    	 incr Ast.error_count;
			 locals )
| Break -> locals
| Continue -> locals
| If(predicate,then_blk,Block([])) -> 
	let tpredicate = (check_expr (*********) predicate) in 
		if tpredicate = "bool" then (check_stmt (globals,locals) current_func then_blk) 
		else (print_string ("Warning: The predicate of an If statement should be a boolean expression not "^tpredicate^".\n") ;
			 incr Ast.error_count;
			 locals)
| If(predicate,then_blk,else_blk) -> 
	let tpredicate = (check_expr (**********) predicate) in
		if tpredicate = "bool" then (
			ignore (check_stmt (globals,locals) current_func then_blk);
			(check_stmt (globals,locals) current_func else_blk)
		) 
		else (print_string ("Warning: The predicate of an If statement should be a boolean expression not "^tpredicate^".\n");
			  locals)
| While(predicate,stmt_blk) -> 
	let tpredicate = (check_expr (*******) predicate) in
		if tpredicate = "bool" then (check_stmt (globals,locals) current_func stmt_blk) 
		else (print_string ("Warning: The predicate of an While statement should be a boolean expression not "^tpredicate^".\n"); 
			 locals)
| Do_while(predicate,stmt_blk) -> 
	let tpredicate = (check_expr (*********) predicate) in 
		if tpredicate = "bool" then (check_stmt (globals,locals) current_func stmt_blk) 
		else (print_string ("Warning: The predicate of an While statement should be a boolean expression not "^tpredicate^".\n");
			  locals)
)



(*We will take take the Ast and return a global varmap, a funmap, the original Ast*)
let check_program program =
let globals = {varMap = NameMap.empty;funMap = NameMap.empty} and locals = {outMap = NameMap.empty;currMap = NameMap.empty} in
let inbuilt_functions = ["display";"main";"open";"save"] in
let (globals,_) =	
	List.fold_left (
		fun (globals,locals) prog -> 
			(match prog with
				| Fdefn(f) -> ({globals with funMap = 
					if List.mem f.fname inbuilt_functions then (
						print_endline ("ERROR :'"^f.fname^"' is an inbuilt function.");
						incr Ast.error_count;
						globals.funMap
					)
					else if NameMap.mem f.fname globals.funMap then (
						print_endline ("ERROR : The function '"^f.fname^"' has already been declared.");
						incr Ast.error_count;
						globals.funMap)
					else NameMap.add f.fname f globals.funMap},locals);
				(*check the statements in the function block here after saving globals. *)
				| Stmt(Vexpr(Vdecl(Vdefn(ty,id_list)))) -> ({globals with varMap =
					if ty <> "void" then (
						List.fold_left (  
							fun vmap id_array -> 
								(match id_array with
									|Single(id) -> if NameMap.mem id vmap then (
													print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
													incr Ast.error_count;
													vmap
													) 
												   else NameMap.add id ty vmap
									|Array(id,i,j) -> if NameMap.mem id vmap then (
														print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
														incr Ast.error_count;
														vmap
														) 
												  	  else NameMap.add id ty vmap
								)
						) globals.varMap id_list
				 	)
					else (print_endline ("ERROR : Variable(s) "^(Ast.string_of_id_list id_list)^" cannot have type void.");
						  incr Ast.error_count;
						  globals.varMap)
					},locals)
				| Stmt(Vexpr(Vdecl(Vassign(ty,id_list,expr)))) -> ({globals with varMap =
					if ty <> "void" then (
						let rhs = check_expr (***********) expr in
						if ty = rhs then (
							List.fold_left (  
								fun vmap id_array -> 
									(match id_array with
										|Single(id) -> if NameMap.mem id vmap then (
														print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
														incr Ast.error_count;
														vmap
														) 
													   else NameMap.add id ty vmap
										|Array(id,i,j) -> if NameMap.mem id vmap then (
															print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
															incr Ast.error_count;
															vmap
															) 
														  else NameMap.add id ty vmap
									)
							) globals.varMap id_list
						)
						else (print_endline ("ERROR : Variable(s) "^(Ast.string_of_id_list id_list)^" has(have) type "^ty^" and RHS has type "^rhs^".");
							  incr Ast.error_count;
							  globals.varMap)
				 	)
					else (print_endline ("ERROR : Variable(s) "^(Ast.string_of_id_list id_list)^" cannot have type void.");
						  incr Ast.error_count;
						  globals.varMap)
					},locals)
				| Stmt(Return(expr)) -> print_endline "ERROR: What are you trying to accomplish? 'return' only makes sense when used inside functions";
										incr Ast.error_count;
										(globals,locals)
				| Stmt(Break) ->  print_endline "ERROR: What are you trying to accomplish? 'break' only makes sense when used inside loops";
								  incr Ast.error_count;
								  (globals,locals)
				| Stmt(Continue) -> print_endline "ERROR: What are you trying to accomplish? 'continue' only makes sense when used inside loops";
									incr Ast.error_count;
									(globals,locals)
				| Stmt(s) -> (let locals = {outMap = globals.varMap ; currMap = NameMap.empty}  in ignore (check_stmt (globals,locals) "main" s);(globals,locals)
			)
		)) (globals,locals) program
in
(globals.varMap,globals.funMap,program)