open Ast

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

let check_program (fdecl_list,vdecl_list,errors) =
	let globals = List.fold_left (fun varmap vdecl -> (match vdecl with
		| Vassign(ty,id_list,expr) -> (List.fold_left (fun vmap id -> (print_string (ty^" "^id^";")) ;NameMap.add id ty vmap) varmap id_list)
		| Vdefn("",_) -> varmap
		| Vdefn(ty,id_list) -> (List.fold_left (fun vmap id -> (print_string (ty^" "^id^";")) ;NameMap.add id ty vmap) varmap id_list)
			)
		) NameMap.empty vdecl_list
	and functions = NameMap.empty 
	in (globals,functions)