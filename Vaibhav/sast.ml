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
*)


let get_id_access = function
| ASingle(id) -> id
| AArray(id,_,_) -> id

(*takes globals, locals and an expr to return a string which is the return datatype of the expr*)

let rec check_expr (globals,locals) = function
| Strings(_) -> "string"
| Integers(_) -> "int"
| Floats(_) -> "float"
| Boolean(_) -> "bool"
| Id(ida) -> (*return type of id*)
	let id = get_id_access ida in
		(try	NameMap.find id locals.currMap
		 with
			| Not_found -> 
				(
				 try	NameMap.find id locals.outMap
				 with
					| Not_found -> (print_endline ("ERROR: Variable "^Ast.string_of_id_access ida^" is undeclared.");
						incr Ast.error_count;
						"")
				)
		)
| Assign(ida,exp) -> (*check if lhs and rhs have the same type and return type of variable*)
let lhs = 
		let id = get_id_access ida in
		(try	NameMap.find id locals.currMap
		 with
			| Not_found -> 
				(
				 try	NameMap.find id locals.outMap
				 with
					| Not_found -> (print_endline ("ERROR: Variable "^Ast.string_of_id_access ida^" is undeclared.");
						incr Ast.error_count;
						"")
				)
		)
	and rhs = check_expr (globals,locals) exp in
	(if lhs <> rhs then 
		(print_endline ("ERROR: In assignment to variable "^Ast.string_of_id_access ida^". Variable has type "^lhs^" and RHS has type "^rhs^".");
			incr Ast.error_count
		)
	);lhs

(*Handle all specific inbuilt functions before the generic call*)
| Call("display",actuals) -> 
(*Handle inbuilt display function here*) 
ignore
(if List.length actuals <> 1 then 
	(print_endline("Wrong number of arguments in call to the 'display' function.");incr Ast.error_count)
 else
 	(let param = List.hd actuals in 
 		if check_expr (globals,locals) param <> "string" then 
 				(print_endline("Wrong number of arguments in call to the 'display' function.");
 				incr Ast.error_count)
 	) 
);
"void"
| Call("open",actuals) -> 
(*Handle inbuilt open function here*)
ignore
(let len = List.length actuals in
	(if len = 1 then 
		(let param = List.hd actuals in 
			let ty = check_expr (globals,locals) param in
				(if ty <> "string" then 
 				(print_endline("ERROR: Illegal argument to function 'open'. The function expected an argument of type string but was provided an argument of type "^ty^" .");
 				incr Ast.error_count)))
	else if len = 2 then 
		(let param1::param2::[] = actuals in
			let ty1 = check_expr (globals,locals) param1 and ty2 = check_expr (globals,locals) param2 in
					if ty1 <> "string" || ty2 <> "bool" then 
		 				(print_endline("ERROR: Illegal argument to function 'open'. The function expected arguments of type string and bool but was provided arguments of type "^ty1^", "^ty2^" .");
		 				incr Ast.error_count)
		)
	else 
		(print_endline("ERROR: Wrong number of arguments in call to the 'open' function.");
		incr Ast.error_count)
 	) 
);
"image"
| Call("save",actuals) -> 
(*Handle inbuilt save function here*)
ignore
(if List.length actuals <> 1 then 
	(print_endline("ERROR: Wrong number of arguments in call to the 'save' function.");
	incr Ast.error_count)
 else
 	(let param = List.hd actuals in
 		let ty =  check_expr (globals,locals) param in
	 		if ty <> "string" then 
 				(print_endline("ERROR: Illegal argument to function 'save'. The function expected an argument of type string but was provided an argument of type "^ty^" .");
 					incr Ast.error_count)
 	) 
);
"void"
| Call(id,actuals) -> 
(*Handle all other generic calls here*)
if NameMap.mem id globals.funMap then (
	let fdecl = (NameMap.find id globals.funMap) in 
	ignore (
		try (List.fold_left2 (fun () actual formal -> 
			let tactual = check_expr (globals,locals) actual and (tformal,formalid) = formal in 
				(if tactual <> tformal then 
					(print_endline ("ERROR: Illegal argument to function '"^fdecl.fname^"'. The function expected an argument of type "^tformal^" but was provided an argument of type "^tactual^" .");
					 incr Ast.error_count)
				)
			) () actuals fdecl.formals
		)
    with Invalid_argument(s) -> 
    	(print_endline ("ERROR: Number of arguments provided in the call to function '"^fdecl.fname^"'' dont match the function definition.");
    		incr Ast.error_count
   		)
		);
			fdecl.rtype
)
else (print_endline ("ERROR: Undeclared function '"^id^"'.");
			incr Ast.error_count;
			"")


| Binop(e1,o,e2) -> 
	let t1 = check_expr (globals,locals) e1 and t2 = check_expr (globals,locals) e2 in
		(match o with
			|Add -> 
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else if t2 = "float" then "float"
										  	 else if t2 = "string" then "string"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | "float" ->  if t2 = "float" then "float"
												else if t2 = "int" then "float"
										  		else if t2 = "string" then "string"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"float")
								  | "string" -> if t2 = "string" then "string"
								  				else if t2 = "int" then "string"
										  		else if t2 = "float" then "string"
								  				else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  				incr Ast.error_count;
								  				"string")
								  | "bool" -> if t2 = "bool" then "bool"
									  			else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
									  			incr Ast.error_count;
									  			"bool")
								  | "pixel" ->  if t2 = "pixel" then "image"
										  		else if t2 = "image" then "image"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"pixel")
								  | "image" -> if t2 = "pixel" then "image"
										  		else if t2 = "image" then "video"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"image")
								  | "video" -> if t2 = "video" then "video"
										  		else if t2 = "image" then "video"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"video")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Sub -> 
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | "float" -> if t2 = "float" then "float"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"float")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Mult -> 
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else if t2 = "float" then "float"
										  	 else if t2 = "string" then "string"
											 else if t2 = "pixel" then "image"
											 else if t2 = "image" then "video"
											 else if t2 = "video" then "video"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | "float" ->  if t2 = "float" then "float"
												else if t2 = "int" then "float"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"float")
								  | "string" -> if t2 = "int" then "string"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"string")
								  | "bool" -> if t2 = "bool" then "bool"
									  			else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
									  			incr Ast.error_count;
									  			"bool")
								  | "pixel" ->  if t2 = "int" then "image"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"pixel")
								  | "image" -> if t2 = "int" then "video"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"image")
								  | "video" -> if t2 = "int" then "video"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"video")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Div -> 
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | "float" -> if t2 = "float" then "float"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"float")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Mod -> 
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Equal -> 
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | "float" ->  if t2 = "float" then "float"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"float")
								  | "string" -> if t2 = "string" then "string"
								  				else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  				incr Ast.error_count;
								  				"string")
								  | "bool" -> if t2 = "bool" then "bool"
									  			else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
									  			incr Ast.error_count;
									  			"bool")
								  | "pixel" ->  if t2 = "pixel" then "pixel"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"pixel")
								  | "image" -> if t2 = "image" then "image"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"image")
								  | "video" -> if t2 = "video" then "video"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"video")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Neq -> 
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | "float" ->  if t2 = "float" then "float"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"float")
								  | "string" -> if t2 = "string" then "string"
								  				else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  				incr Ast.error_count;
								  				"string")
								  | "bool" -> if t2 = "bool" then "bool"
									  			else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
									  			incr Ast.error_count;
									  			"bool")
								  | "pixel" ->  if t2 = "pixel" then "pixel"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"pixel")
								  | "image" -> if t2 = "image" then "image"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"image")
								  | "video" -> if t2 = "video" then "video"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"video")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Less -> 
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | "float" -> if t2 = "float" then "float"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"float")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Leq ->  
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | "float" -> if t2 = "float" then "float"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"float")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Greater -> 
				(match t1 with 
								  | "int" -> if t2 = "int" then "int"
											 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
											 incr Ast.error_count;
											 "int")
								  | "float" -> if t2 = "float" then "float"
												else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
												incr Ast.error_count;
												"float")
								  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  ""))
			|Geq -> 
				(match t1 with 
				  | "int" -> if t2 = "int" then "int"
							 else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
							 incr Ast.error_count;
							 "int")
				  | "float" -> if t2 = "float" then "float"
								else (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								incr Ast.error_count;
								"float")
				  | s -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
				  incr Ast.error_count;
				  ""))
			| _ -> "")

| Uop(o,e) -> let t = check_expr (globals,locals) e in
	(match o with
				| Sub ->
					(match t with 
					  | "int" -> "int"
					  | "float" -> "float"
					  | "bool" -> "bool"
					  | s -> (print_endline ("ERROR in op. Conflict in type "^t^" .");
					  incr Ast.error_count;
					  ""))
				| Not ->
					(match t with
						| "bool" -> "bool"
						| s -> (print_endline ("ERROR in op. Conflict in type "^t^" .");
						incr Ast.error_count;
						""))
					| _ -> "")

| Paren(e) -> check_expr (globals,locals) e

| Objid(_) -> "void"

| Objcall(_) -> "void"

| Noexpr -> "void"

let addMap map1 map2 = 
NameMap.merge (fun k xo yo -> match xo,yo with
    | Some x, Some y -> Some y
    | None, yo -> yo
    | xo, None -> xo
  ) map1 map2


let check_vdecl (globals,locals) = function
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
			let rhs = check_expr (globals,locals) expr in
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
		


(*check_stmt function takes global and local environments and current return type and returns a modified local environment environment*)

let rec check_stmt (globals,locals) current_func = (function
| For(v_initialize,condition,step,stmt_blk) -> (*modify locals and then check statements then return modified local before stmt checks*)
(*v_initialize; while (condition) stmt_blk*)
 ignore
 (let locals = check_stmt (globals,locals) current_func (Vexpr(v_initialize)) in
		(let cond =  check_expr (globals,locals) condition in
				if cond <> "bool" then (
					print_endline ("ERROR : Condition in for statement should be a boolean expression, not "^cond^".");
					incr Ast.error_count
				)
		);
		check_stmt (globals,locals) current_func stmt_blk
	);locals
| For_list(v_initialize,array_id,stmt_blk) -> (*Same as other for loop*)
	ignore
 (let locals = check_stmt (globals,locals) current_func (Vexpr(v_initialize)) in
		check_stmt (globals,locals) current_func stmt_blk
	);locals
| Vexpr(Vdecl(vdecl)) ->  check_vdecl (globals,locals) vdecl
| Vexpr(Expr(expr)) -> ignore (check_expr (globals,locals) expr);locals
| Block(stmt_list)  -> 
		ignore (let locals = {outMap = addMap locals.outMap locals.currMap; currMap = NameMap.empty} in 
	List.fold_left (fun local stmt -> check_stmt (globals,local) current_func stmt) locals stmt_list
		) ; locals
| Return(expr) -> 
	let ty = (check_expr (globals,locals) expr) and current_func = NameMap.find current_func globals.funMap in 
		if current_func.rtype = ty then locals 
		else (print_endline ("ERROR: In function '"^current_func.fname^"' return expression is of type "^ty^" but expected return type is "^current_func.rtype^".\n");
	    	 incr Ast.error_count;
			 locals )
| Break -> locals
| Continue -> locals
| If(predicate,then_blk,Block([])) -> 
 	ignore 
 	(let cond =  check_expr (globals,locals) predicate in
			(if cond <> "bool" then (
				print_endline ("ERROR : The predicate of an If statement should be a boolean expression, not "^cond^".");
				incr Ast.error_count)
			);
		check_stmt (globals,locals) current_func then_blk
	);locals
| If(predicate,then_blk,else_blk) -> 
	ignore
 	(let cond =  check_expr (globals,locals) predicate in
			(if cond <> "bool" then (
				print_endline ("ERROR : The predicate of an If statement should be a boolean expression, not "^cond^".");
				incr Ast.error_count)
			);
		ignore (check_stmt (globals,locals) current_func then_blk);
		check_stmt (globals,locals) current_func else_blk
	);locals
| While(predicate,stmt_blk) -> 
	ignore
 	(let cond =  check_expr (globals,locals) predicate in
			(if cond <> "bool" then (
				print_endline ("ERROR : The predicate of a While statement should be a boolean expression, not "^cond^".");
				incr Ast.error_count)
			);
		check_stmt (globals,locals) current_func stmt_blk
	);locals
| Do_while(predicate,stmt_blk) -> 
	ignore
	(let cond =  check_expr (globals,locals) predicate in
		(if cond <> "bool" then (
			print_endline ("ERROR : The predicate of a Do-While statement should be a boolean expression, not "^cond^".");
			incr Ast.error_count)
		);
		check_stmt (globals,locals) current_func stmt_blk
	);locals
)

let check_function (globals,locals) fdecl = 
	ignore
	(let locals = {outMap = globals.varMap; 
				currMap = 
					List.fold_left (fun vmap (ty,id_array) -> 
						NameMap.add (
								(match id_array with
									|Single(id) -> id
									|Array(id,_,_) -> id
								)
							) ty vmap
						) NameMap.empty fdecl.formals
		} in
		List.fold_left (fun local stmt -> check_stmt (globals,local) fdecl.fname stmt) locals fdecl.body
	);
	(globals,locals)
(*We will take take the Ast and return a global varmap, a funmap, the original Ast*)
let check_program program =
let globals = {varMap = NameMap.empty;funMap = NameMap.empty} and locals = {outMap = NameMap.empty;currMap = NameMap.empty} in
let inbuilt_functions = ["display";"main";"open";"save"] in
let (globals,_) =	
	List.fold_left (
		fun (globals,locals) prog -> 
			(match prog with
				| Fdefn(f) -> 
					let (globals,locals) =
						({globals with funMap = 
							if List.mem f.fname inbuilt_functions then (
							print_endline ("ERROR :'"^f.fname^"' is an inbuilt function.");
							incr Ast.error_count;
							globals.funMap
						)
						else if NameMap.mem f.fname globals.funMap then (
							print_endline ("ERROR : The function '"^f.fname^"' has already been declared.");
							incr Ast.error_count;
							globals.funMap)
						else NameMap.add f.fname f globals.funMap},locals)
					in
					check_function (globals,locals) f
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
						let rhs = check_expr (globals,{outMap = NameMap.empty; currMap = globals.varMap}) expr in
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
						else (print_endline ("ERROR : Variable(s) "^(Ast.string_of_id_list id_list)^" has(have) type "^ty^" and RHS has type "^rhs^". Variables could not be declared.");
							  incr Ast.error_count;
							  globals.varMap)
				 	)
					else (print_endline ("ERROR : Variable(s) "^(Ast.string_of_id_list id_list)^" cannot have type void.");
						  incr Ast.error_count;
						  globals.varMap)
					},locals)
				| Stmt(Return(_)) -> print_endline "ERROR: What are you trying to accomplish? 'return' only makes sense when used inside functions";
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