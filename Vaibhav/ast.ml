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