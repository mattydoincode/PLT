(* Semantic analysis
	- input: abstract syntax tree
	- build symbol table
	- run type inference algorithm
	- output: semantic tree
*)

open Ast
open Sast

type symbol_table = {
	parent : symbol_table option;
	variables : (string * Sast.t) list;
}

type environment = {
	parent : environment option; (* entire env can be nested (funcs) *)
	return_type : Sast.t option; (* Function’s return type *)
	scope : symbol_table;        (* symbol table for vars *)
}

(* inside a for loop or if block, nest scope *)
let nest_scope (env : environment) : environment =
	let s = { variables = []; parent = Some(env.scope) } in
	{ env with scope = s; }	

(* inside a function, nest entire env *)
let nest_env (env : environment) : environment = 
	let s = { variables = []; parent = Some(env.scope) } in
	{ scope = s; return_type = None; parent = Some(env); }

let rec find_variable (scope : symbol_table) name =
	try
		List.find (fun (s, _, _, _) -> s = name) scope.variables
	with Not_found ->
		match scope.parent with
			Some(parent) -> find_variable parent name
			| _ -> raise Not_found

(* UNIFICATION *)
(* invariant for substitutions: no id on a lhs occurs in any term earlier  *)
(* in the list                                                             *)
type substitution = (id * typ) list

(* check if a variable occurs in a term *)
let rec occurs (x : id) (t : typ) : bool =
  match t with
  | TVar y -> x = y
  | Arrow (u, v) -> occurs x u || occurs x v

(* substitute term s for all occurrences of var x in term t *)
let rec subst (s : typ) (x : id) (t : typ) : typ =
  match t with
  | TVar y -> if x = y then s else t
  | Arrow (u, v) -> Arrow (subst s x u, subst s x v)

(* apply a substitution to t right to left *)
let apply (s : substitution) (t : typ) : typ =
  List.fold_right (fun (x, e) -> subst e x) s t

(* unify one pair *)
let rec unify_one (s : typ) (t : typ) : substitution =
  match (s, t) with
  | (TVar x, TVar y) -> if x = y then [] else [(x, t)]
  | (Arrow (x, y), Arrow (u, v)) -> unify [(x, u); (y, v)]
  | ((TVar x, (Arrow (u, v) as z)) | ((Arrow (u, v) as z), TVar x)) ->
      if occurs x z
      then failwith "not unifiable: circularity"
      else [(x, z)]

(* unify a list of pairs *)
and unify (s : (typ * typ) list) : substitution =
  match s with
  | [] -> []
  | (x, y) :: t ->
      let t2 = unify t in
      let t1 = unify_one (apply t2 x) (apply t2 y) in
      t1 @ t2

(* INFERENCE *)
let code1 = ref (Char.code 'a')
let code2 = ref (Char.code 'a')

let reset_type_vars() = 
  (code1 := Char.code 'a'; code2 := Char.code 'a')

let next_type_var() : Sast.t =
  let c1 = !code1 in
  let c2 = !code2 in
  (
    if c2 > Char.code 'Z' then (incr code1; code2 := Char.code 'a')
    else incr code2;
    TVar((Char.escaped (Char.chr c1)) ^ (Char.escaped (Char.chr c2)))
  )

let type_of (ae : aexpr) : typ =
  match ae with
    AVar (_, a) -> a
  | AFun (_, _, a) -> a
  | AFunCall (_, _, a) -> a
  
let annotate_program (p : Ast.program) : Sast.aProgram =
	let env = { scope = { variables = []; parent = None }; return_type = None } in
	annotate_stmts p env

let rec annotate_stmts (stmts : Ast.stmt list) (env : environment) : Sast.aStmt list =
	List.map (fun x -> annotate_stmt x env) stmts

let rec annotate_stmt (s : Ast.stmt) (env : environment) : Sast.aStmt =
	match s with
    Return(expr) -> AReturn(annotate_expr expr env)
  | If(conds, elsebody) -> 
  	let aIfFunc = fun cond -> 
  		let ae = annotate_expr cond.condition env in
  		let new_env = nest_scope env in
  		let aBody = annotate_stmts cond.body new_env in
  		(ae, aBody)
	in
	let aElseFunc = fun else -> 
  		let new_env = nest_scope env in
  		annotate_stmts cond.body new_env in
	in
	match elsebody with
		Some(x) -> AIf(List.map aIfFunc conds, Some(aElseFunc x))
		None -> AIf(List.map aIfFunc conds, None)
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

let annote

let annotate_expr (e : Ast.expr) (env : environment) : Sast.aExpr =

(* annotate all subexpressions with types *)
(* bv = stack of bound variables for which current expression is in scope *)
(* fv = hashtable of known free variables *)
let annotate (e : expr) : aexpr =
  let (h : (id, typ) Hashtbl.t) = Hashtbl.create 16 in
  let rec annotate' (e : expr) (bv : (id * typ) list) : aexpr =
    match e with
      Var x ->
        (* bound variable? *)
        (try let a = List.assoc x bv in AVar (x, a)
        (* known free variable? *)
        with Not_found -> try let a = Hashtbl.find h x in AVar (x, a)
        (* unknown free variable *)
        with Not_found -> let a = next_type_var() in Hashtbl.add h x a; AVar (x, a))
    | Fun (x, e) ->
        (* assign a new type to x *)
        let a = next_type_var() in
        let ae = annotate' e ((x, a) :: bv) in
        AFun (x, ae, Arrow (a, type_of ae))
    | FunCall (e1, e2) ->
        AFunCall (annotate' e1 bv, annotate' e2 bv, next_type_var())
  in annotate' e []

(* collect constraints for unification *)
let rec collect (aexprs : aexpr list) (u : (typ * typ) list) : (typ * typ) list =
  match aexprs with
    [] -> u
  | AVar (_, _) :: r -> collect r u
  | AFun (_, ae, _) :: r -> collect (ae :: r) u
  | AFunCall (ae1, ae2, a) :: r ->
      let (f, b) = (type_of ae1, type_of ae2) in
      collect (ae1 :: ae2 :: r) ((f, Arrow (b, a)) :: u)

(* collect the constraints and perform unification *)
let infer (p : Ast.program) : Sast.aProgram =
  reset_type_vars();
  let annotatedP = annotate p in
  let collectedP = collect [annotatedP] [] in
  let subs = unify collectedP in
  apply subs (type_of ae)


  (* 1. get the char thing working*)
