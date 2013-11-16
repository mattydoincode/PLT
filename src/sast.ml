include Ast

(* types for inference *)
type t = 
    TVar of string
  | TFunc of t list * t
  | TList of t
  | TObjCreate of (string * t) list
  | TObjAccess of string * t
  | TNum
  | TChar
  | TBool

(* annotated expression *)
type aExpr =
    ANumLit of float * t
  | ABoolLit of bool * t
  | ACharLit of char * t
  | AId of string * bool * t
  | AFuncCreate of string list * aStmt list * t
  | AFuncCallExpr of aExpr * aExpr list * t
  | AObjAccess of aExpr * string * t
  | AListAccess of aExpr * aExpr * t
  | AListCreate of aExpr list * t
  | ASublist of aExpr * aExpr option * aExpr option * t
  | AObjectCreate of (string * aExpr) list * t
  | ABinop of aExpr * Ast.op * aExpr * t
  | ANot of aExpr * t

and aStmt =
    AReturn of aExpr
  | AIf of aConditional list * aStmt list option
  | AFor of (aExpr * aExpr) option * aExpr option * (aExpr * aExpr) option * aStmt list
  | AWhile of aExpr * aStmt list 
  | AAssign of (aExpr * aExpr)
  | AFuncCallStmt of aExpr * aExpr list

and aConditional = {
  aCondition : aExpr;
  aBody : aStmt list;
}

type aProgram = aStmt list

let tree_of_opt tree_of = function 
    Some(x) -> Some(tree_of x)
  | None -> None

let rec tree_of_typed_expr e = (tree_of_expr e, [])

and tree_of_expr = function
    NumLit(n) -> ANumLit(n, TNum)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | CharLit(c) -> ACharLit(c, TChar)
  | Id(s) -> AId(s, false, TVar(s))
  | Not(e) -> ANot(tree_of_typed_expr e, TBool)
  | Binop(e1, op, e2) -> ABinop(tree_of_typed_expr e1, op, tree_of_typed_expr e2)
  | FuncCallExpr(e, el) -> 
      AFuncCallExpr(tree_of_typed_expr e, List.map tree_of_typed_expr el)
  | FuncCreate(formals, body) ->
      AFuncCreate(formals, List.map tree_of_stmt body)
  | ListCreate(exprs) ->
      AListCreate(List.map tree_of_typed_expr exprs)
  | Sublist(e, eleft, eright) -> 
      ASublist(
        tree_of_typed_expr e,
        tree_of_opt tree_of_typed_expr eleft,
        tree_of_opt tree_of_typed_expr eright)
  | ObjCreate(props) ->
      AObjCreate(List.map (fun(prop) -> (fst prop, tree_of_typed_expr (snd prop))) props)
  | ObjAccess(e1, s) -> 
      AObjAccess(tree_of_typed_expr e1, s)
  | ListAccess(e1, e2) ->
      AListAccess(tree_of_typed_expr e1, tree_of_typed_expr e2)

and tree_of_stmt = function
    Return(expr) ->  AReturn(tree_of_typed_expr expr)
  | If(conds, elsebody) -> AIf(List.map tree_of_cond conds, tree_of_opt tree_of_stmts elsebody)
  | For(a1, e, a2, body) ->
      AFor(tree_of_opt tree_of_assign a1,
        tree_of_opt tree_of_typed_expr e,
        tree_of_opt tree_of_assign a2,
        tree_of_stmts body)
  | While(e, s) -> 
      AWhile(tree_of_typed_expr e, tree_of_stmts s)
  | Assign(a) -> AAssign(tree_of_assign a)
  | FuncCallStmt(e, el) -> 
      AFuncCallStmt(tree_of_typed_expr e, List.map tree_of_typed_expr el)

and tree_of_stmts stmts = 
  List.map tree_of_stmt stmts

and tree_of_cond cond = 
  {aCondition=(tree_of_typed_expr cond.condition);aBody=(tree_of_stmts cond.body)}

and tree_of_assign ((e1, e2)) = 
  (tree_of_typed_expr e1, tree_of_typed_expr e2)

let tree_of_program prog = 
  tree_of_stmts prog

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