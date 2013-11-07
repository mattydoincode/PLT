include Ast
type pctype = 
    PCNUM | PCBOOL | PCSTRING (*weird*)
  | PCLIST | PCOBJECT | PCFUNC

type stTypedExpr = stExpr * pctype list

and stExpr =
    StNumLit of float
  | StBoolLit of bool
  | StStringLit of string
  | StId of string
  | StFuncCreate of string list * stStmt list
  | StFuncCallExpr of stTypedExpr * stTypedExpr list
  | StAccess of stTypedExpr * stTypedExpr 
  | StListCreate of stTypedExpr list
  | StSublist of stTypedExpr * stTypedExpr option * stTypedExpr option
  | StObjectCreate of (string * stTypedExpr) list  
  | StBinop of stTypedExpr * Ast.op * stTypedExpr
  | StNot of stTypedExpr 

and stStmt =
    StReturn of stTypedExpr
  | StIf of stConditional list * stStmt list option
  | StFor of (stTypedExpr * stTypedExpr) option * stTypedExpr option * (stTypedExpr * stTypedExpr) option * stStmt list
  | StWhile of stTypedExpr * stStmt list 
  | StAssign of (stTypedExpr * stTypedExpr)
  | StFuncCallStmt of stTypedExpr * stTypedExpr list

and stConditional = {
  stCondition : stTypedExpr;
  stBody : stStmt list;
}

type stProgram = stStmt list

let tree_of_opt tree_of = function 
    Some(x) -> Some(tree_of x)
  | None -> None

let rec tree_of_typed_expr e = (tree_of_expr e, []) 

and tree_of_expr = function
    NumLit(n) -> StNumLit(n)
  | BoolLit(b) -> StBoolLit(b)
  | StringLit(chars) -> StStringLit(String.concat "" (List.map Char.escaped chars) ^ "'")
  | Id(s) -> StId(s)
  | Not(e) -> StNot(tree_of_typed_expr e)
  | Binop(e1, op, e2) -> StBinop(tree_of_typed_expr e1, op, tree_of_typed_expr e2)
  | FuncCallExpr(e, el) -> 
      StFuncCallExpr(tree_of_typed_expr e, List.map tree_of_typed_expr el)
  | FuncCreate(formals, body) ->
      StFuncCreate(formals, List.map tree_of_stmt body)
  | ListCreate(exprs) ->
      StListCreate(List.map tree_of_typed_expr exprs)
  | Sublist(e, eleft, eright) -> 
      StSublist(
        tree_of_typed_expr e,
        tree_of_opt tree_of_typed_expr eleft,
        tree_of_opt tree_of_typed_expr eright)
  | ObjectCreate(props) ->
      StObjectCreate(List.map (fun(prop) -> (fst prop, tree_of_typed_expr (snd prop))) props)
  | Access(e1, e2) -> 
      StAccess(tree_of_typed_expr e1, tree_of_typed_expr e2)

and tree_of_stmt = function
    Return(expr) ->  StReturn(tree_of_typed_expr expr)
  | If(conds, elsebody) -> StIf(List.map tree_of_cond conds, tree_of_opt tree_of_stmts elsebody)
  | For(a1, e, a2, body) ->
      StFor(tree_of_opt tree_of_assign a1,
        tree_of_opt tree_of_typed_expr e,
        tree_of_opt tree_of_assign a2,
        tree_of_stmts body)
  | While(e, s) -> 
      StWhile(tree_of_typed_expr e, tree_of_stmts s)
  | Assign(a) -> StAssign(tree_of_assign a)
  | FuncCallStmt(e, el) -> 
      StFuncCallStmt(tree_of_typed_expr e, List.map tree_of_typed_expr el)


and tree_of_stmts stmts = 
  List.map tree_of_stmt stmts

and tree_of_cond cond = 
  stConditional({condition=tree_of_typed_expr cond.condition;body=tree_of_stmts cond.body})

and tree_of_assign ((e1, e2)) = 
  (tree_of_typed_expr e1, tree_of_typed_expr e2)

let tree_of_program prog = 
  stProgram(tree_of_stmts prog)



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