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
type env = globals * locals or globals * locals * (stmt list)
*)

let string_of_id = function
| Single(id) -> id
| Array(id,e1,e2,e3) -> 
(
if e3 <> Integers(0) then
id ^ "[" ^string_of_expr e1 ^"]" ^ "[" ^ string_of_expr e2 ^ "]" ^ "[" ^ string_of_expr e2 ^ "]"
else if e2 <> Integers(0) then 
id ^ "[" ^string_of_expr e1 ^"]" ^ "[" ^ string_of_expr e2 ^ "]"
else
id ^ "[" ^string_of_expr e1 ^"]"	)
(*takes globals, locals and an expr to return a string which is the return datatype of the expr, string to print to file*)

let rec check_expr (globals,locals) = function
    Integers(l) -> ("int",string_of_int l)
  | Floats(f) -> ("float",string_of_float f)
  | Strings(s) -> ("string",("\""^s^"\""))
  | Boolean(b) -> ("bool",(if(b) then "true" else "false"))
  | Id(s) -> 
  	(try (
  		NameMap.find s locals.currMap,s
  		)
  	with 
  	 	| Not_found -> (
  	 			try (
  				NameMap.find s locals.outMap,s
		  		)
				 	with 
		  	 	| Not_found -> (
		  	 		print_endline ("ERROR: Undelared Variable '"^s^"'.");
		  	 		incr Ast.error_count;
		  	 		("",s)
		  	 		)
		  	)
  	)

  | Var_access(i,e,(b1,e11,e12),(b2,e21,e22),(b3,e31,e32)) -> 
			let	r,str   = check_expr (globals,locals) e   and
					r11,str11 = check_expr (globals,locals) e11 and 
					r12,str12 = check_expr (globals,locals) e12 and 
					r21,str21 = check_expr (globals,locals) e21 and 
					r22,str22 = check_expr (globals,locals) e22 and 
					r31,str31 = check_expr (globals,locals) e31 and 
					r32,str32 = check_expr (globals,locals) e32 in
					(match (i,r,(b1,r11,r12),(b2,r21,r22),(b3,r31,r32)) with
					| (1,r,(true,"int","int"),_,_) when r = "Video" -> "Video", str
					| (1,r,(false,"int",_),_,_) when r = "int" -> ("int", str^"["^str11^"]" )
					| (1,r,(false,"int",_),_,_)	when r = "Pixel" -> ("int","("^str^")."^(match str11 with | "0" -> "r" | "1" -> "g" | "2" -> "b" | _ ->(print_endline "Erroneous array access.";incr Ast.error_count ;"")))
					| (1,r,(false,"int",_),_,_)	when r = "Video" -> "Image*",str
					| (2,r,(true,"int","int"),(true,"int","int"),_) when r = "Image*" -> "Image*","getCropped("^str^","^str11^","^str12^","^str21^","^str22^")"
					| (2,r,(true,"int","int"),(false,"int",_),_) when r = "Image*" -> "Image*","getCropped("^str^","^str11^","^str12^","^str21^","^str21^")"
					| (2,r,(false,"int",_),(true,"int","int"),_) when r = "Image*" -> "Image*","getCropped("^str^","^str11^","^str11^","^str21^","^str22^")"
					| (2,r,(false,"int",_),(false,"int",_),_) when r = "Image*" -> "Pixel","(*("^str^")).pixels["^str11^" * (*("^str^")).width + "^str21^"]"
					| (2,r,(false,"int",_),(false,"int",_),_) when r = "int" -> "int",str^"["^str11^"]["^str21^"]"
					| (3,r,(true,"int","int"),(true,"int","int"),(true,"int","int")) when r = "Video" -> "",str
					| (3,r,(true,"int","int"),(true,"int","int"),(false,"int",_)) when r = "Video" -> "",str
					| (3,r,(true,"int","int"),(false,"int",_),(true,"int","int")) when r = "Video" -> "",str
					| (3,r,(true,"int","int"),(false,"int",_),(false,"int",_)) when r = "Video" -> "",str
					| (3,r,(false,"int",_),(true,"int","int"),(true,"int","int")) when r = "Video" -> "",str
					| (3,r,(false,"int",_),(true,"int","int"),(false,"int",_)) when r = "Video" -> "",str
					| (3,r,(false,"int",_),(false,"int",_),(true,"int","int")) when r = "Video" -> "",str
					| (3,r,(false,"int",_),(false,"int",_),(false,"int",_)) when r = "int" -> "int",str^"["^str11^"]["^str21^"]["^str31^"]"
					| _ -> print_endline "Erroneous array access.";incr Ast.error_count ;"",str
					)
  
(*
add op overloading ??

*)

| Binop(e1,o,e2) -> 
	let t1,str1 = check_expr (globals,locals) e1 and t2,str2 = check_expr (globals,locals) e2 in
		(match o,t1,t2 with
			| (Add,"int","int") -> "int", (str1^" + "^ str2)
			| (Add,"int","float") -> "float", (str1^" + " ^ str2)
			| (Add,"float","int") -> "float", (str1^" + " ^ str2)
			| (Add,"float","float") -> "float", (str1^" + " ^ str2)
			| (Add,"string","string") -> "string",str1^" + " ^ str2
			| (Add,"bool","bool") -> "bool",str1^" || " ^ str2
			| (Add,"Pixel","Pixel") -> "Pixel",str1^" + " ^ str2
			| (Add,"Image*","Image*") -> "Video",str1^" + " ^ str2
			| (Add,"Image*","Video") -> "Video",str1^" + " ^ str2
			| (Add,"Video","Image*") -> "Video",str1^" + " ^ str2
			| (Add,"Video","Video") -> "Video",str1^" + " ^ str2
			| (Sub,"int","int") -> "int",str1^" - " ^ str2
			| (Sub,"int","float") -> "float",str1^" - " ^ str2
			| (Sub,"float","int") -> "float",str1^" - " ^ str2
			| (Sub,"float","float") -> "float",str1^" - " ^ str2
			| (Mult,"int","int") -> "int",str1^" * " ^ str2
			| (Mult,"int","float") -> "float",str1^" * " ^ str2
			| (Mult,"int","string") -> "string","String_rep("^str1^","^str2^")"
			| (Mult,"int","Pixel") -> "Image*",str1^" * " ^ str2
			| (Mult,"int","Image*") -> "Video",str1^" * " ^ str2
			| (Mult,"int","Video") -> "Video",str1^" * " ^ str2
			| (Mult,"float","int") -> "float",str1^" * " ^ str2
			| (Mult,"float","float") -> "float",str1^" * " ^ str2
			| (Mult,"string","int") -> "string","String_rep("^str2^","^str1^")"
			| (Mult,"bool","bool") -> "bool",str1^" && " ^ str2
			| (Mult,"Pixel","int") -> "Image*",str1^" * " ^ str2
			| (Mult,"Image*","int") -> "Video",str1^" * " ^ str2
			| (Mult,"Video","int") -> "Video",str1^" * " ^ str2
			| (Div,"int","int") -> "int",str1^" / " ^ str2
			| (Div,"int","float") -> "float",str1^" / " ^ str2
			| (Div,"float","int") -> "float",str1^" / " ^ str2
			| (Div,"float","float") -> "float",str1^" / " ^ str2
			| (Mod,"int","int") -> "int",str1^" % " ^ str2
			| (Equal,t1,t2) when t1=t2 && t1 <> "void" -> "bool",str1^" == " ^ str2
			| (Neq,t1,t2) when t1=t2 && t1 <> "void" -> "bool",str1^" != " ^ str2
			| (Greater,"int","int") -> "bool",str1^" > " ^ str2
			| (Greater,"float","float") -> "bool",str1^" > " ^ str2
			| (Geq,"int","int") -> "bool",str1^" >= " ^ str2
			| (Geq,"float","float") -> "bool",str1^" >= " ^ str2
			| (Less,"int","int") -> "bool",str1^" < " ^ str2
			| (Less,"float","float") -> "bool",str1^" < " ^ str2
			| (Leq,"int","int") -> "bool",str1^" <= " ^ str2
			| (Leq,"float","float") -> "bool",str1^" <= " ^ str2
			| (_,t1,t2) -> (print_endline ("ERROR in op. Conflict between types "^t1^" and "^t2^" .");
								  incr Ast.error_count;
								  "","")
		)

| Uop(o,e) -> let t,str = check_expr (globals,locals) e in
	(match o with
				| Sub ->
					(match t with 
					  | "int" -> "int","-("^str^")"
					  | "float" -> "float","-("^str^")"
					  | "bool" -> "bool","!("^str^")"
					  | s -> (print_endline ("ERROR in op. Conflict in type "^t^" .");
					  incr Ast.error_count;
					  "","!("^str^")"))
				| Not ->
					(match t with
						| "bool" -> "bool","!("^str^")"
						| s -> (print_endline ("ERROR in op. Conflict in type "^t^" .");
						incr Ast.error_count;
						"","!("^str^")"))
					| _ -> "","!("^str^")")


| Paren(e) -> let a = check_expr (globals,locals) e in fst(a),"("^snd(a)^")"

| Assign(e1, e2) -> let t1,str1 = check_expr (globals,locals) e1 and t2,str2 =  check_expr (globals,locals) e2 in
											if t1 = t2 then t1,(str1^" = "^str2)
											else (print_endline ("ERROR in Assignment. Conflict between types "^t1^" and "^t2^" .");
														incr Ast.error_count;
														t1,str1^" = "^str2)


(*Handle all specific inbuilt functions before the generic call*)

(*
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
"Image*",""


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
"void",""


*)

| Call(id,actuals) -> 
(*Handle all other generic calls here*)
if NameMap.mem id globals.funMap then (
	let fdecl = (NameMap.find id globals.funMap) in 
	ignore (
		try (List.fold_left2 (fun () actual formal -> 
			let tactual,str = check_expr (globals,locals) actual and (tformal,formalid) = formal in 
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
			fdecl.rtype,fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_expr actuals) ^ ")"
)
else (print_endline ("ERROR: Undeclared function '"^id^"'.");
			incr Ast.error_count;
			"",""
)
(*
| Call("open",act)-> "open(" ^ String.concat ", " (List.map string_of_expr act) ^ ")"
| Call("save",act)-> "save(" ^ String.concat ", " (List.map string_of_expr act) ^ ")"
*)
| Int_list(lst) -> "int","{"^ String.concat "," (List.map (fun expr -> snd(check_expr (globals,locals) expr)) lst) ^"}"
| Objid(obj,var) -> 
let ty,objid = check_expr (globals,locals) obj in
	(match var with
		| "height" when ty = "Image*" -> "int","(*("^objid^")).height "
		| "width" when ty = "Image*" -> "int","(*("^objid^")).width "
		| "mode" when ty = "Image*" -> "int","((*"^objid^")).GetMode() "
		| "C1" when ty = "Pixel" ->  "int",objid^".r "
		| "C2" when ty = "Pixel" ->  "int",objid^".g "
		| "C3" when ty = "Pixel" ->  "int",objid^".b "
		| _ -> ty,objid^"."^var
	)
	 
| Objcall(Objid(obj,met),act) ->
	let ty,objid = check_expr (globals,locals) obj in
		let actuals = (List.map (fun expr -> (let typ,str = check_expr (globals,locals) expr in
			str) ) act)
		in

		(match met with
				| "getPixel" when ty = "Image*" ->  "Pixel",objid^".GetPixel("^String.concat "," actuals^") "
				| "getWidth" when ty = "Image*" ->  "int",objid^".GetWidth() "
				| "getHeight" when ty = "Image*" -> "int",objid^".GetHeight() "
				| "getMode" when ty = "Image*" -> "bool",objid^".GetMode() "
				| "setPixel" when ty = "Image*" -> "void",objid^".SetPixel("^String.concat "," actuals^") "
				| "getRows" when ty = "Image*" -> "Image*",objid^".GetRows("^String.concat "," actuals^") "
				| "getCols" when ty = "Image*" -> "Image*",objid^".GetCols("^String.concat "," actuals^") "
				| "BMP2RGB" when ty = "Image*" -> "Image*",objid^".BMP2RGB("^String.concat "," actuals^") "
				| "RGB2BMP" when ty = "Image*" -> "Image*",objid^".RGB2BMP() "
				| "changeMode" when ty = "Image*" ->  "Image*",objid^".ChangeMode() "
				| "RGB2YUV" when ty = "Image*" || ty = "Pixel" -> ty,objid^".RGB2YUV() "
				| "YUV2RGB" when ty = "Image*" || ty = "Pixel" -> ty,objid^".YUV2RGB() "
				| "getC1" when ty = "Pixel" -> "int",objid^".getC1() "
				| "getC2" when ty = "Pixel" -> "int",objid^".getC2() "
				| "getC3" when ty = "Pixel" -> "int",objid^".getC3() "
				| "setY" when ty = "Image*" -> "int",objid^".alterBufY("^String.concat "," actuals^") "
				| "setR" when ty = "Image*" -> "int",objid^".alterBufY("^String.concat "," actuals^") "
				| "setU" when ty = "Image*" -> "int",objid^".alterBufU("^String.concat "," actuals^") "
				| "setG" when ty = "Image*" -> "int",objid^".alterBufU("^String.concat "," actuals^") "
				| "setV" when ty = "Image*" -> "int",objid^".alterBufV("^String.concat "," actuals^") "
				| "setB" when ty = "Image*" -> "int",objid^".alterBufV("^String.concat "," actuals^") "
				| "addFrame" when ty = "Video" -> "int",objid^".AddFrame("^String.concat "," actuals^") "
				| "getFrame" when ty = "Video" -> "Image*","extract(readfile,writefile,start frame,no of frames)"
				| _ -> ty,objid^"."^met^"("^	String.concat "," actuals	^") "
	)

| Noexpr -> "void",""
| _ -> "",""





let addMap map1 map2 = 
NameMap.merge (fun k xo yo -> match xo,yo with
    | Some x, Some y -> Some y
    | None, yo -> yo
    | xo, None -> xo
  ) map1 map2


let check_vdecl (globals,locals) = function


|Vassign(ty,Single(id),Call("open",act))->
	let locals = {locals with currMap = 
		if NameMap.mem id locals.currMap then (
			print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
			incr Ast.error_count;
	 		locals.currMap
			) 
    	else NameMap.add id ty locals.currMap}
    in
	if ty <> "Image*" && ty <> "Video" then (print_endline "open is used for images and videos only."; incr Ast.error_count);
	if List.length act <> 1 then (print_endline "Invalid arguments to open" ; incr Ast.error_count);
  	let typ,strng = check_expr (globals,locals) (List.hd act) in
  	  	if typ <> "string" then (print_endline "Invalid arguments to open" ; incr Ast.error_count);
  	  	locals,("\n{\nstring S = "^strng^";\nFILE *F = fopen(S.c_str(),\"r\");\n "^id^"= BMPReadImage(F);\nfclose(F);\n}\n")

|Vassign(ty,Array(_),Call("open",_))->
	print_endline "Wrong use of variable declaration and open"; incr Ast.error_count;
	locals,";\n"
	
| Vassign(ty,ida,Call("input",act))-> 
	let id = string_of_id ida in
  	let locals = {locals with currMap = 
		if NameMap.mem id locals.currMap then (
			print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
			incr Ast.error_count;
	 		locals.currMap
			) 
    	else NameMap.add id ty locals.currMap}
    in
    if ty = "Image*" || ty = "Video" || ty = "Pixel" then (print_endline "open is used for images and videos not input."; incr Ast.error_count);
	locals,(
  		if List.length act > 1 then (print_endline "Invalid arguments to open" ; incr Ast.error_count);
		if List.length act <> 0 then (
		let typ,strng = check_expr (globals,locals) (List.hd act) in
   			if typ <> "string" then (print_endline "Invalid arguments to open" ; incr Ast.error_count);
  			"cout << ("^strng^")<< endl;\n cin.clear();\ncin >> "^ id ^";\n"
  			)
	  	else ("\n cin.clear();\ncin >> "^ id ^";\n"))

|Vassign("Pixel",Single(id),Int_list([Integers(i1);Integers(i2);Integers(i3)])) ->
{locals with currMap = 
		if NameMap.mem id locals.currMap then (
			print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
			incr Ast.error_count;
	 		locals.currMap
			) 
    	else NameMap.add id "Pixel" locals.currMap},(
    			if i1>255 || i1<0 || i2>255 || i2<0 || i3>255 || i3<0 then
					(print_endline "This is a pixel. Assign an array of 3 integers whose value is less than 255 to it."; incr Ast.error_count);
			    		"Pixel "^id^"("^string_of_int i1^","^string_of_int i2^","^string_of_int i3^");\n")




|Vassign("Pixel",Single(id),Int_list([e1;e2;e3])) ->
{locals with currMap = 
		if NameMap.mem id locals.currMap then (
			print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
			incr Ast.error_count;
	 		locals.currMap
			) 
    	else NameMap.add id "Pixel" locals.currMap},(
    		let t1,s1 = check_expr (globals,locals) e1 and t2,s2 = check_expr (globals,locals) e1 and t3,s3 = check_expr (globals,locals) e1 in
    			if t1 <> "int" || t2 <> "int" || t2 <> "int" then
					(print_endline "This is a pixel. Assign an array of 3 integers to it."; incr Ast.error_count);
			    		"Pixel "^id^"("^s1^","^s2^","^s3^");\n")





|Vdefn(ty,id_list) -> {locals with currMap = 
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
						|Array(id,e1,e2,e3) -> if NameMap.mem id vmap then (
											print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
											incr Ast.error_count;
											vmap
											) 
								  	  else if fst(check_expr (globals,locals) e1) <> "int" || fst(check_expr (globals,locals) e2) <> "int" ||	fst(check_expr (globals,locals) e3) <> "int" then  (
													print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
													incr Ast.error_count;
													vmap
												)
										else 
								  	  NameMap.add id ty vmap
					)
			) locals.currMap id_list
	 	)
		else (print_endline ("ERROR : Variable(s) "^Ast.string_of_id_list id_list^" cannot have type void.");
			  incr Ast.error_count;
			  locals.currMap)
		}, (String.concat ";\n" (List.map (fun id ->(match id with
				| Array(ident,a,b,Integers(0)) when ty = "Image*"->ty^" "^ ident^" = new Image("^string_of_expr a^","^string_of_expr b^ ");\n" 
				| _ ->	(ty^" "^string_of_id id ))) id_list)^";\n")



|Vassign(ty,id_array,expr) -> 
	let rhs,str = check_expr (globals,locals) expr in
	({locals with currMap =
		if ty <> "void" then (
			if ty = rhs then (
				(match id_array with
					|Single(id) -> if NameMap.mem id locals.currMap then (
									print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
									incr Ast.error_count;
									locals.currMap
									) 
								   else NameMap.add id ty locals.currMap
					|Array(id,e1,e2,e3) -> if NameMap.mem id locals.currMap then (
									print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
									incr Ast.error_count;
									locals.currMap
									) 
									else if fst(check_expr (globals,locals) e1) <> "int" || fst(check_expr (globals,locals) e2) <> "int" ||	fst(check_expr (globals,locals) e3) <> "int" then  (
										print_endline ("ERROR : variable "^id^" has already been declared in this scope.");
										incr Ast.error_count;
										locals.currMap
									)
								 
								else NameMap.add id ty locals.currMap
				)
			)
			else (print_endline ("ERROR : Variable(s) has(have) type "^ty^" and RHS has type "^rhs^". Variables could not be declared.");
				  incr Ast.error_count;
				  locals.currMap)
	 	)
		else (print_endline ("ERROR : Variable(s) cannot have type void.");
			  incr Ast.error_count;
			  locals.currMap)
		},
			(match id_array with
				| Array(ident,a,b,Integers(0)) when ty = "Image*"->ty^" "^ ident^"("^string_of_expr a^","^string_of_expr b^ ");\n" 
				| _ ->	(ty^" "^string_of_id id_array ^ " = "^ str^";\n" )
										) 	)




(*check_stmt function takes global and local environments and current return type and returns a modified local environment environment*)

let rec check_stmt (globals,locals) current_func = (function


| Vexpr(Expr(Assign(e,Call("input",act))))-> 
	let ty,str = check_expr (globals,locals) e in
  	if ty = "Image*" || ty = "Video" || ty = "Pixel" then (print_endline "input is used for images and videos not input."; incr Ast.error_count);
	locals,(
  		if List.length act > 1 then (print_endline "Invalid arguments to input" ; incr Ast.error_count);
		if List.length act <> 0 then (
		let typ,strng = check_expr (globals,locals) (List.hd act) in
   			if typ <> "string" then (print_endline "Invalid arguments to input" ; incr Ast.error_count);
  			"cout << ("^strng^")<< endl;\n cin.clear();\ncin >> "^ str ^";\n"
  			)
	  	else ("\n cin >> "^ str ^";\n"))


| Vexpr(Expr(Assign(e,Call("open",act))))-> 
	let ty,str = check_expr (globals,locals) e in
	if ty <> "Image*" && ty <> "Video" then (print_endline "open is used for images and videos only."; incr Ast.error_count);
	if List.length act <> 1  then (print_endline "Invalid arguments to open" ; incr Ast.error_count);
  	let typ,strng = check_expr (globals,locals) (List.hd act) in
  	  	if typ <> "string" then (print_endline "Invalid arguments to open" ; incr Ast.error_count);
  	  	locals,("\n{\nstring S = "^strng^";\nFILE *F = fopen(S.c_str(),\"r\");\n "^str^" = BMPReadImage(F);\nfclose(F);\n}\n")


| Vexpr(Expr(Call("save",act)))-> 
   	   	if List.length act <> 2 then (print_endline "Invalid arguments to save" ; incr Ast.error_count);
   	    let frst::scnd::rem = act in
   	    let ty1,str1 = check_expr (globals,locals) frst and ty2,str2 = check_expr (globals,locals) scnd in
   	   		if ty2 <> "string" || ty1 <> "Image*" then (print_endline "Invalid arguments to save" ; incr Ast.error_count);
   			locals,("\n{\nstring S = "^str2^";\nFILE *F = fopen(S.c_str(),\"w\");\nBMPWriteImage(("^str1^"),F);\nfclose(F);\n}\n")			
   	  	


| Vexpr(Expr(Call("input",act)))-> 
   	locals,(
   	   	if List.length act > 1 then (print_endline "Invalid arguments to input" ; incr Ast.error_count);
   	    	if List.length act <> 0 then 
   	    	let ty,str = check_expr (globals,locals) (List.hd act) in
   	   		(
   				if ty <> "string" then (print_endline "Invalid arguments to input" ; incr Ast.error_count);
   				"cout << ("^str^")<< endl;\n \n cin.clear();\ncin.ignore(2);\n"
   			 )
   		  	else "\ncin.clear();\ncin.ignore(2);\n"
   	  	)

| Vexpr(Expr(Call("display",actuals))) -> 
locals,(	
	if List.length actuals = 1 then 
	(let ty,str = check_expr (globals,locals) (List.hd actuals) in
		(
			if ty = "Image*" || ty = "Video" then "display("^str^");\n"
						else if ty = "Pixel" then (print_endline "cant display a pixel!!";incr Ast.error_count;"cout<<endl;")
						else ("cout<<"^str^"<<endl;\n")
					)
	)
	else
	( "cout<<" ^ (String.concat "<<" (List.map (fun expr -> 
		let ty,str = check_expr (globals,locals) expr in
		(if ty = "Image*" || ty = "Video" || ty = "Pixel" then (print_endline "cant display a pixel!!";incr Ast.error_count));
		str
		) actuals))  ^ "<< endl;\n"
	)

)




| For(v_initialize,condition,step,stmt_blk) -> (*modify locals and then check statements then return modified local before stmt checks*)
(*v_initialize; while (condition) stmt_blk*)
 let str = 
 (let locals,strv = check_stmt (globals,locals) current_func (Vexpr(v_initialize)) in
		let cond,stre =  check_expr (globals,locals) condition in
				if cond <> "bool" then (
					print_endline ("Warning : Condition in for statement should be a boolean expression, not "^cond^".")
				);
		let _,strs = check_stmt (globals,locals) current_func stmt_blk in
			("for ("^String.sub strv 0 (String.length strv - 2)^";"^stre^";"^snd(check_expr (globals,locals) step)^")\n"^strs^"\n")
	)
 in
 locals,str
| For_list(v_initialize,array_id,stmt_blk) -> (*Same as other for loop*)
	locals,"\n\n\n\n\n\n\n\n"
| Vexpr(Vdecl(vdecl)) ->  let locals,str = check_vdecl (globals,locals) vdecl in locals,str
| Vexpr(Expr(expr)) ->  locals,(snd(check_expr (globals,locals) expr)^";\n")
| Block(stmt_list)  ->
	let str_stmt_blk = ("{\n"^
			(let locals = {outMap = addMap locals.outMap locals.currMap; currMap = NameMap.empty} in 
				let _,str_blk =
		List.fold_left (fun (local,str) stmt -> 
							(let lcl,strs = check_stmt (globals,local) current_func stmt in
														lcl,(str^strs)													
							)
						) (locals,"") stmt_list
			in str_blk
			)^ "}\n") in
		 locals,str_stmt_blk
| Return(expr) -> 
	let ty,str = check_expr (globals,locals) expr and current_func = NameMap.find current_func globals.funMap in 
		(if current_func.rtype <> ty then 
				(print_endline ("ERROR: In function '"^current_func.fname^"' return expression is of type "^ty^" but expected return type is "^current_func.rtype^".\n");
			    	 incr Ast.error_count));
			 locals,("return "^str^";\n")
| Break -> locals,"break;\n"
| Continue -> locals,"continue;\n"
| If(predicate,then_blk,Block([])) -> 
 	
 	let cond,strp =  check_expr (globals,locals) predicate in
			(if cond <> "bool" then (
				print_endline ("Warning : The predicate of an If statement should be a boolean expression, not "^cond^"."))
			);
	let _,strthen	= check_stmt (globals,locals) current_func then_blk in
	locals,("if ("^strp^")"^strthen^"\n")
| If(predicate,then_blk,else_blk) -> 
	let cond,strp =  check_expr (globals,locals) predicate in
			if cond <> "bool" then (
				print_endline ("Warning : The predicate of an If statement should be a boolean expression, not "^cond^"."));
	let _,strthen	= check_stmt (globals,locals) current_func then_blk and _,strelse = check_stmt (globals,locals) current_func else_blk in
	locals,("if ("^strp^") "^strthen^" else "^strelse^"\n")
| While(predicate,stmt_blk) -> 
	let cond,strp =  check_expr (globals,locals) predicate in
			(if cond <> "bool" then (
				print_endline ("Warning : The predicate of a While statement should be a boolean expression, not "^cond^"."))
			);
		let  _,strs = check_stmt (globals,locals) current_func stmt_blk in
	locals,("while (" ^ strp ^ ") "^ strs ^ "\n")
| Do_while(predicate,stmt_blk) -> 
	let cond,strp =  check_expr (globals,locals) predicate in
		(if cond <> "bool" then (
			print_endline ("Warning : The predicate of a Do-While statement should be a boolean expression, not "^cond^"."))
		);
	let _,strs = check_stmt (globals,locals) current_func stmt_blk in
	
	locals,("do " ^ strs ^ " while (" ^ strp ^ ");\n")
)




let check_function (globals,locals) fdecl = 
	let locals = {outMap = globals.varMap; 
				currMap = 
					List.fold_left (fun vmap (ty,id_array) -> 
						NameMap.add (
								(match id_array with
									|Single(id) -> id
									|Array(id,_,_,_) -> id
								)
							) ty vmap
						) NameMap.empty fdecl.formals
		} 
	in
		let _,str =	List.fold_left (fun (local,str) stmt -> 
			let lcl,strs = check_stmt (globals,local) fdecl.fname stmt in
					(lcl,(str^strs))
				) (locals,"") fdecl.body
				
	in
	(globals,locals,str)






(*We will take take the Ast and return a global varmap, a funmap, the modified Ast*)
let check_program program =
let globals = {varMap = NameMap.empty;funMap = NameMap.empty} and locals = {outMap = NameMap.empty;currMap = NameMap.empty} in
let inbuilt_functions = ["display";"main";"open";"save";"input"] in
let (globals,_,program,main) =	
	List.fold_left (
		fun (globals,locals,program,main) prog -> 
			(match prog with
				|Fdefn(f) ->
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
					let (_,_,str)=
						(check_function (globals,locals) f)
					in
					(globals,locals,(program^"\n"^f.rtype^" "^f.fname^"("^ (String.concat ", " (List.map Ast.string_of_formal f.formals)) ^")\n{ "^str^" \n}"),main)
					(*check the statements in the function block here after saving globals. *)
	

				| Stmt(Vexpr(Vdecl(Vassign(ty,ida,expr)))) -> 

	        		let local,str = check_vdecl (globals,{outMap=NameMap.empty;currMap=globals.varMap}) (Vassign(ty,ida,expr)) in
						({globals with varMap = local.currMap},locals,program,main^str)

				| Stmt(Vexpr(Vdecl(vdecl))) -> 

	        		let local,str = check_vdecl (globals,{outMap=NameMap.empty;currMap=globals.varMap}) vdecl in
						({globals with varMap = local.currMap},locals,program,main)
				
				
				| Stmt(Return(_)) -> 
					print_endline "ERROR: What are you trying to accomplish? 'return' only makes sense when used inside functions";
					incr Ast.error_count;
					(globals,locals,program,main)
				| Stmt(Break) ->  
					print_endline "ERROR: What are you trying to accomplish? 'break' only makes sense when used inside loops";
					incr Ast.error_count;
					(globals,locals,program,main)
				| Stmt(Continue) -> 
					print_endline "ERROR: What are you trying to accomplish? 'continue' only makes sense when used inside loops";
					incr Ast.error_count;
					(globals,locals,program,main)
				| Stmt(s) -> 
					let locals = {outMap = globals.varMap ; currMap = NameMap.empty}  in 
						 let _,str = check_stmt (globals,locals) "main" s in 
						 	(globals,locals,program,main^str)
						 
			)
		) (globals,locals,"","") program
in
(globals.varMap,globals.funMap,program,main)
