
(* Abstract Syntax Tree
    - created by parser
    - consumed by semantic analyzer
*)

type op = Concat | Mod | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    Num_Lit of int
  | Bool_Lit of bool
  | String_Lit of string list
  | Id of string
  | Func_Create of func_create
  | Func_Call_expr of expr * expr list
  | Access of expr * expr 
  | List_Create of expr list
  | Sublist of expr * expr option * expr option (*In the case of slicing*)
  | Object_Create of (string * expr) list  
  | Binop of expr * op * expr
  | Not of expr 
 

type func_create = {
    formals : string list;
    body : stmt list;
}

type assign = expr * expr    



type stmt =
    Return of expr
  | If of conditional list  
  | For of assign option * expr * assign option * stmt list
  | While of expr * stmt list 
  | Assign of assign
  | Func_Call_Stmt of expr * expr list


type conditional = {
  condition : expr;
  body : stmt list;
}

type program = stmt list
