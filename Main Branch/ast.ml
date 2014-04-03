let error_count = ref 0

type op = 
    Add | Sub | Mult | Div | Mod | Pow
  | Equal | Neq | Less | Leq | Greater | Geq | Not


type expr =
    Integers of int
  | Floats of float
  | Strings of string
  | Id of string
  | Var_access of int * expr * (bool * expr * expr) * (bool * expr * expr) * (bool * expr * expr)
  | Boolean of bool
  | Uop of op * expr
  | Binop of expr * op * expr
  | Paren of expr
  | Assign of expr * expr
  | Call of string * expr list
  | Objcall of expr * expr list
  | Objid of expr * string
  | Int_list of expr list
  | Noexpr

  
type id =
|Single of string
|Array of string * expr * expr * expr


type vdecl =
    Vdefn of string * (id list)
  | Vassign of string * id * expr

type vexpr = 
    Vdecl of vdecl
  | Expr of expr    

type stmt =
  | Block of stmt list
  | Vexpr of vexpr
  | Return of expr
  | Break
  | Continue
  | If of expr * stmt * stmt
  | For of vexpr * expr * expr * stmt
  | For_list of vexpr * string * stmt
  | While of expr * stmt
  | Do_while of expr * stmt

type func_decl = {
    rtype : string;
    fname : string;
    formals : (string * id) list;
    body : stmt list;
  }

type possibilities = 
| Stmt of stmt
| Fdefn of func_decl

type program = possibilities list


let rec string_of_expr = function
    Integers(l) -> string_of_int l
  | Floats(f) -> string_of_float f
  | Strings(s) -> "\"" ^ s ^ "\""
  | Boolean(b) -> if b then "true" else "false"
  | Id(s) -> s
  | Int_list(l) -> "{"^String.concat "" (List.map string_of_expr l)^"}"
  | Var_access(i,e,(b1,e11,e12),(b2,e21,e22),(b3,e31,e32)) -> 
    string_of_expr e ^ (if b1 then "["^string_of_expr e11^":"^string_of_expr e11^"]" else "["^string_of_expr e11^"]") ^
    if i=1 then ""
    else if i=2 then (if b1 then "["^string_of_expr e11^":"^string_of_expr e12^"]" else "["^string_of_expr e11^"]")
    else if i=3 then (if b1 then "["^string_of_expr e21^":"^string_of_expr e22^"]" else "["^string_of_expr e21^"]") ^ (if b1 then "["^string_of_expr e31^":"^string_of_expr e32^"]" else "["^string_of_expr e31^"]") 
    else "Never Occurrs"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
  Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" | _ -> "") ^ " " ^
      string_of_expr e2
  | Uop(o,e) ->  (match o with
        Sub -> "-" | Not -> "!" | _ -> "") ^ " " ^
      string_of_expr e
  | Paren(e) -> "(" ^ string_of_expr e ^")"
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Objid(obj,var) -> string_of_expr obj ^ "." ^ var
  | Objcall(obj,act) -> string_of_expr obj ^ "(" ^ String.concat ", " (List.map string_of_expr act) ^ ")"
  | Noexpr -> ""



let string_of_id = function
| Single(id) -> id
| Array(id,e1,e2,e3) -> id ^ "[" ^string_of_expr e1 ^"]" ^ "[" ^ string_of_expr e2 ^ "]" ^ "[" ^ string_of_expr e2 ^ "]"

let string_of_id_list id = String.concat "," (List.map string_of_id id)

let string_of_vdecl = function
    Vdefn(ty,id) -> ty ^ " " ^ string_of_id_list id ^ " "
  | Vassign(ty,id,ex) -> ty ^ " " ^ string_of_id id ^ " = " ^ (string_of_expr ex) ^ " "


let string_of_vexpr = function
  Vdecl(vdecl) -> string_of_vdecl vdecl
  |Expr(expr) -> string_of_expr expr 


let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Vexpr(vexpr) -> string_of_vexpr vexpr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ (string_of_expr e) ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(v1, e2, e3, s) ->
      "for (" ^ string_of_vexpr v1  ^ " ; " ^ (string_of_expr e2) ^ " ; " ^
      (string_of_expr e3)  ^ ") " ^ string_of_stmt s
(*  | For_list(v1,id,s) -> 
      "for (" ^ string_of_vexpr v1  ^ " in " ^ string_of_id_access id ^ ") " ^ string_of_stmt s*)
  | While(e, s) -> "while (" ^ (string_of_expr e) ^ ") " ^ string_of_stmt s
  | Do_while(e,s) -> "do " ^ string_of_stmt s ^ " while (" ^ (string_of_expr e) ^ ");\n"
  | Return(ex) -> "return "^ (string_of_expr ex) ^ ";\n"
  | Break -> "break ;\n"
  | Continue -> "continue ;\n"


let string_of_formal (ty,id) = ty ^ " " ^ string_of_id id 


let string_of_fdefn fdefn =
  if fdefn.fname <> "" then
  fdefn.rtype ^ " " ^ fdefn.fname ^ "(" ^ String.concat ", " (List.map string_of_formal fdefn.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdefn.body) ^
  "}\n"
  else ""


let string_of_prog = function
    Stmt(s) -> string_of_stmt s
  | Fdefn(f) -> string_of_fdefn f

let string_of_program program =
  String.concat "" (List.map string_of_prog program ) ^ "\n" 
