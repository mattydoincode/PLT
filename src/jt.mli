
(* Java Tree
	- produced by java converter
	- consumed by java code generator
*)

type num_op = Mod | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type string_op = Concat


type expr = 
    num_expr
  | bool_expr
  | string_expr
  | func_expr
  | Func_Create of func_create
  | Access of expr * string_expr 
  | List_Create of expr list
  | Sublist of expr * num_expr * num_expr (*In the case of slicing*)
  | Object_Create of (string * expr) list  
  | Not of expr 


type num_expr = 
    Num_Lit of int
  | Num_Id of string
  | Binop of expr * num_op * expr
  | Num_Func_Call of func_expr * expr list

type bool_expr = 
    Bool_lit of bool
  | Bool_Id of string
  | Bool_Func_Call of func_expr * expr list 
  (*Need to add some form of AND, OR, etc.*)


type string_expr
    String_Lit of string
  | String_Id of string
  | Joined of string_expr * string_op * string_expr
  | String_Func_Call of func_expr * expr list

type func_expr = 
    Func_Id of string
  | Func_Func_Call of func_expr * expr list;

type func_create = {
    name     : string;
    formals  : string list;
    body     : stmt list;
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
  condition : bool_expr;
  body : stmt list;
}

type program = stmt list
