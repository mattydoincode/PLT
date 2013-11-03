type op = 
    Add | Sub | Mult | Div | Mod
  | Equal | Neq | Less | Leq | Greater | Geq
  | Concat | And | Or

type pctype = 
    PCNUM | PCBOOL | PCSTRING (*weird*)
  | PCLIST | PCOBJECT | PCFUNC

type stTypedExpr = stExpr * pctype list

and stExpr =
    NumLit of float
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | FuncCreate of string list * stStmt list
  | FuncCallExpr of stTypedExpr * stTypedExpr list
  | Access of stTypedExpr * stTypedExpr 
  | ListCreate of stTypedExpr list
  | Sublist of stTypedExpr * stTypedExpr option * stTypedExpr option
  | ObjectCreate of (string * stTypedExpr) list  
  | Binop of stTypedExpr * op * stTypedExpr
  | Not of stTypedExpr 

and stStmt =
    Return of stTypedExpr
  | If of stConditional list * stStmt list option
  | For of (stTypedExpr * stTypedExpr) option * stTypedExpr option * (stTypedExpr * stTypedExpr) option * stStmt list
  | While of stTypedExpr * stStmt list 
  | Assign of (stTypedExpr * stTypedExpr)
  | FuncCallStmt of stTypedExpr * stTypedExpr list

and stConditional = {
  condition : stTypedExpr;
  body : stStmt list;
}

type stProgram = stStmt list


(*
NOTES ON SYMBOL MAPPING TABLE
1) Mapping table can't be passed between stages because it's dynamic... 
in other words it gets created on the fly as you walk through the tree
2) rules for the mapping table, however, can be decided on
3) RULES:
  a) Each time you enter function, all variables get an underscore appended
    if you're two functions deep, two underscores appended, this is our scoping
  b) each time you assign, you simply overwrite any existing variable with
      the same name. This should work if we walk the tree in the correct direction
  c) each entry will have a type and a value associated with it, that's determined on 
      assignment and doesn't change until another assignment
*)