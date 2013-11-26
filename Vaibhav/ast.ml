type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    Integers of int
  | Floats of float
  | Strings of string
  | Id of string
  | Boolean of bool
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Objcall of string * string * expr list
  | Objid of string * string
  | Noexpr

type vdecl =
    Vdefn of string * (string list)
  | Vassign of string * (string list) * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Vdecl of vdecl
  | Break
  | Continue
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    rtype : string;
    fname : string;
    formals : (string*string) list;
    body : stmt list;
  }

type program = (vdecl list) * (func_decl list)


let rec string_of_expr = function
    Integers(l) -> string_of_int l
  | Floats(f) -> string_of_float f
  | Strings(s) -> "\"" ^ s ^ "\""
  | Boolean(b) -> if b then "true" else "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
  Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Objid(obj,var) -> obj ^ "." ^ var
  | Objcall(obj,met,act) -> obj ^ "." ^ met ^ "(" ^ String.concat ", " (List.map string_of_expr act) ^ ")"
  | Noexpr -> ""

let string_of_id_list id = String.concat "," id

let string_of_vdecl = function
    Vdefn(ty,id) -> ty ^ " " ^ string_of_id_list id ^ ";\n"
  | Vassign(ty,id,ex) -> ty ^ " " ^ string_of_id_list id ^ " = " ^ (string_of_expr ex) ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Vdecl(vdecl) -> string_of_vdecl vdecl
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ (string_of_expr e) ^ ") " ^ string_of_stmt s
  | Return(ex) -> "return "^ (string_of_expr ex) ^ ";\n"
  | Break -> "break ;\n"
  | Continue -> "continue ;\n"

let string_of_formal (ty,id) = ty ^ " " ^ id 

let string_of_vdecl_opt = function
    Vdefn("",_) -> ""
  | Vdefn(ty,id) -> ty ^ " " ^ string_of_id_list id ^ ";\n"
  | Vassign(ty,id,ex) -> ty ^ " " ^ string_of_id_list id ^ " = " ^ (string_of_expr ex) ^ ";\n"

let string_of_fdefn fdefn =
  fdefn.rtype ^ " " ^ fdefn.fname ^ "(" ^ String.concat ", " (List.map string_of_formal fdefn.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdefn.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl_opt vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdefn funcs)
