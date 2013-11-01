
(* Abstract Syntax Tree
    - created by parser
    - consumed by semantic analyzer
*)

type op = Concat | Mod | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    Num_Lit of int
  | Bool_Lit of bool
  | String_Lit of string 
  | Id of string
  | Func_Create of func_create
  | Func_Call of expr * expr list
  | Access of Access
  | List_Create of expr list
  | Sublist of expr * expr option * expr option (*In the case of slicing*)
  | Object_Create of (string * expr) list  
  | Binop of expr * op * expr
  | Not of expr  
 

type func_create = {
    formals : string list;
    body : stmt list;
}


 
 type assign = {

 }   

type stmt =
  | Return of expr
  | If of expr * stmt list * else list  
  | For of assign option * expr * assign option * stmt list
  | While of expr * stmt list 

  type else = {
      condition : expr;
      body : stmt list;
  }

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

type program = string list * func_decl list
