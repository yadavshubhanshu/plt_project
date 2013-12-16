let error_count = ref 0

type op = 
    Add | Sub | Mult | Div | Mod 
  | Equal | Neq | Less | Leq | Greater | Geq | Not


type range = 
|Index of int
|Range of int * int

   
type ida = 
|ASingle of string
|AArray of string * range * range

type id =
|Single of string
|Array of string * int * int


type expr =
    Integers of int
  | Floats of float
  | Strings of string
  | Id of ida
  | Boolean of bool
  | Uop of op * expr
  | Binop of expr * op * expr
  | Paren of expr
  | Assign of ida * expr
  | Call of string * expr list
  | Objcall of string * string * expr list
  | Objid of string * string
  | Noexpr



type vdecl =
    Vdefn of string * (id list)
  | Vassign of string * (id list) * expr

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
  | For_list of vexpr * ida * stmt
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



let string_of_range = function
|Index(i) -> string_of_int i
|Range(i1,i2) -> if i2>0 then string_of_int i1 ^ ":" ^ string_of_int i2 else ":"


let string_of_id_access = function
| ASingle(id) -> id
| AArray(id,r1,r2) -> id ^ "[" ^ string_of_range r1 ^ "]" ^ (if r2 = Range(0,0) then " " else  "[" ^ string_of_range r2 ^ "]")


let rec string_of_expr = function
    Integers(l) -> string_of_int l
  | Floats(f) -> string_of_float f
  | Strings(s) -> "\"" ^ s ^ "\""
  | Boolean(b) -> if b then "true" else "false"
  | Id(s) -> string_of_id_access s
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
  | Assign(v, e) -> string_of_id_access v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Objid(obj,var) -> obj ^ "." ^ var
  | Objcall(obj,met,act) -> obj ^ "." ^ met ^ "(" ^ String.concat ", " (List.map string_of_expr act) ^ ")"
  | Noexpr -> ""



let string_of_id = function
| Single(id) -> id
| Array(id,i,j) -> id ^ "[" ^string_of_int i ^"]" ^ ( if j>0 then  "[" ^ string_of_int j ^ "]" else "")

let string_of_id_list id = String.concat "," (List.map string_of_id id)

let string_of_vdecl = function
    Vdefn(ty,id) -> ty ^ " " ^ string_of_id_list id ^ " "
  | Vassign(ty,id,ex) -> ty ^ " " ^ string_of_id_list id ^ " = " ^ (string_of_expr ex) ^ " "


let string_of_vexpr = function
  Vdecl(vdecl) -> string_of_vdecl vdecl
  |Expr(expr) -> string_of_expr expr 


let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Vexpr(vexpr) -> string_of_vexpr vexpr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(v1, e2, e3, s) ->
      "for (" ^ string_of_vexpr v1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | For_list(v1,id,s) -> 
      "for (" ^ string_of_vexpr v1  ^ " in " ^ string_of_id_access id ^ ") " ^ string_of_stmt s
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
