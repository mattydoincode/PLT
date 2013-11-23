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
  mutable variables : (string * Sast.t) list;
}

type environment = {
	mutable cur_func_return_type : Sast.t option; (* Function’s return type *)
	scope : symbol_table;        (* symbol table for vars *)
}

(* inside a for loop or if block, nest scope *)
let nest_scope (env : environment) : environment =
	let s = { variables = []; parent = Some(env.scope) } in
	{ env with scope = s; }	

(* inside a function, nest entire env *)
let new_env() : environment = 
	let s = { variables = []; parent = None } in
	{ scope = s; cur_func_return_type = None; }

let rec find_variable (scope : symbol_table) (name : string) : Sast.t option =
  try
    let (_, typ) = List.find (fun (s, _) -> s = name) scope.variables in
    Some(typ)
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_variable parent name
      | _ -> None





(* UNIFICATION *)
(* invariant for substitutions: no id on a lhs occurs in any term earlier  *)
(* in the list                                                             *)
type substitution = (string * Sast.t) list

(* check if a variable occurs in a term *)
let rec occurs (x : string) (typ : Sast.t) : bool =
  match typ with
  | TVar(name) -> x = name
  | TFunc(params, y) -> List.exists (fun param -> occurs x param) params || occurs x y
  | TList(y) -> occurs x y
  | TObjCreate(props) -> List.exists (fun prop -> occurs x (snd prop)) props
  | TObjAccess(prop) -> occurs x (snd prop)
  | TNum -> false
  | TChar -> false
  | TBool -> false

(* substitute term s for all occurrences of var x in term t *)
let rec subst (s : Sast.t) (x : string) (typ : Sast.t) : Sast.t =
  match typ with
  | TVar(name) -> if x = name then s else typ
  | TFunc(params, y) -> TFunc(List.map (fun param -> subst s x param) params, subst s x y)
  | TList(y) -> TList(subst s x y)
  | TObjCreate(props) -> TObjCreate(List.map (fun prop -> (fst prop, subs s x (snd prop))) props)
  | TObjAccess(prop) -> TObjAccess(fst prop, subst s x (snd prop))
  | TNum -> typ
  | TChar -> typ
  | TBool -> typ

(* apply a substitution to t right to left *)
let apply (s : substitution) (typ : Sast.t) : Sast.t =
  List.fold_right (fun (x, e) -> subst e x) s typ

(* unify one pair *)
let rec unify_one (a : Sast.t) (b : Sast.t) : substitution =
  match (a, b) with
  | (TVar(x), TVar(y)) -> 
      if x = y then [] else [(x, b)]
  | (TVar(x), TFunc(params, y) as z) | (TFunc(params, y) as z, TVar(x))
  | (TVar(x), TList(y) as z) | (TList(y) as z, TVar(x))
  | (TVar(x), TObjCreate(props) as z) | (TObjCreate(props) as z,TVar(x))
  | (TVar(x), TObjAccess(prop) as z) | (TObjAccess(prop) as z, TVar(x))
  | (TVar(x), TNum as z) | (TNum as z, TVar(x))
  | (TVar(x), TChar as z) | (TChar as z, TVar(x))
  | (TVar(x), TBool as z) | (TBool as z, TVar(x)) ->
      [(x, z)]
  | (TFunc(params1, x), TFunc(params2, y)) ->
      if 
        List.length params1 = List.length params2
      then
        unify (x,y)::(List.map2 (fun u v -> (u,v)) params1 params2)
      else
        failwith "# of parameters not the same"
  | (TFunc(params1, x), TList(y)) | (TList(y), TFunc(params1, x))
  | (TFunc(params, x), TObjCreate(props)) | (TObjCreate(props), TFunc(params, x)) 
  | (TFunc(params, x), TObjAccess(prop)) | (TObjAccess(prop), TFunc(params, x))
  | (TFunc(params, x), TNum) | (TNum, TFunc(params, x))
  | (TFunc(params, x), TChar) | (TChar, TFunc(params, x))
  | (TFunc(params, x), TBool) | (TBool, TFunc(params, x)) ->
      failwith "type mismatch function with non-function"
  | (TList(x), TList(y)) ->
  | (TList(x), TObjCreate(props)) ->
  | (TList(x), TObjAccess(prop)) ->
  | (TList(x), TNum) ->
  | (TList(x), TChar) ->
  | (TList(x), TBool) ->
  | (TObjCreate(props1), TObjCreate(props2)) ->
      let mapper = fun prop1 -> 
        let found = List.find (fun prop2 -> (fst prop1) = (fst prop2)) props2 in
        ((snd prop1), (snd found))
        with Not_found ->
          failwith "Type mistmatch: Property from one Object not found on other Object."
      in
      unify (List.map mapper props1)
  | (TObjCreate(props), TObjAccess(prop)) | (TObjAccess(prop), TObjCreate(props)) ->
      let found = List.find (fun cProp -> (fst cProp) = (fst prop)) props in
        unify_one (snd found) (snd prop)
      with Not_found -> 
        failwith "Type mistmatch: Property does not exist on Object."
  | (TObjCreate(props), TNum)  | (TNum, TObjCreate(props))
  | (TObjCreate(props), TChar) | (TChar, TObjCreate(props))
  | (TObjCreate(props), TBool) | (TBool, TObjCreate(props)) ->
      failwith "Type mistmatch: Object and Primitive."
  | (TObjAccess(prop1), TObjAccess(prop2)) ->
      unify_one (snd prop1) (snd prop2)
  | (TObjAccess(prop), TNum as z)  | (TNum as z, TObjAccess(prop))
  | (TObjAccess(prop), TChar as z) | (TChar as z, TObjAccess(prop))
  | (TObjAccess(prop), TBool as z) | (TBool as z, TObjAccess(prop)) ->
      unify_one (snd prop) z
  | (TNum, TChar) | (TNum, TBool) | (TChar, TBool)
  | (TChar, TNum) | (TBool, TNum) | (TBool, TChar) ->
      failwith "Type mismatch: Primitive and Primitive."
  | (TNum, TNum) | (TChar, TChar) | (TBool, TBool) -> 
      []


(*
  | (TVar x, TVar y) -> if x = y then [] else [(x, typ)]
  | (Arrow (x, y), Arrow (u, v)) -> unify [(x, u); (y, v)]
  | ((TVar x, (Arrow (u, v) as z)) | ((Arrow (u, v) as z), TVar x)) ->
      if occurs x z
      then failwith "not unifiable: circularity"
      else [(x, z)]
*)



(* unify a list of pairs *)
and unify (s : (Sast.t * Sast.t) list) : substitution =
  match s with
  | [] -> []
  | (x, y) :: tl ->
      let t2 = unify tl in
      let t1 = unify_one (apply t2 x) (apply t2 y) in
      t1 @ t2





(* INFERENCE *)
let code1 = ref (Char.code 'A')
let code2 = ref (Char.code 'A')

let reset_type_vars() = 
  (code1 := Char.code 'A'; code2 := Char.code 'A')

let next_type_var() : Sast.t =
  let c1 = !code1 in
  let c2 = !code2 in
    (
      if c2 == Char.code 'Z' 
      then (incr code1; code2 := Char.code 'a')
      else incr code2;
      if c1 >= Char.code 'Z'
      then code1 := Char.code 'a'
      else ();
      TVar((Char.escaped (Char.chr c1)) ^ (Char.escaped (Char.chr c2)))
    )

let type_of (ae : Sast.aExpr) : Sast.t =
  match ae with
  | ANumLit(_, t) -> t
  | ABoolLit(_, t) -> t
  | ACharLit(_, t) -> t
  | AId(_, _, t) -> t
  | AFuncCreate(_, _, t) -> t
  | AFuncCallExpr(_, _, t) -> t
  | AObjAccess(_, _, t) -> t
  | AListAccess(_, _, t) -> t
  | AListCreate(_, t) -> t
  | ASublist(_, _, _, t) -> t
  | AObjCreate(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | ANot(_, t) -> t

let rec annotate_expr (e : Ast.expr) (env : environment) : Sast.aExpr =
  match e with
  | NumLit(n) -> ANumLit(n, TNum)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | CharLit(c) -> ACharLit(c, TChar)
  | Id(s) ->
      let typ = find_variable env.scope s in
      (match typ with
      | Some(x) -> AId(s, false, x)
      | None -> failwith ("Unrecognized identifier " ^ s ^ "."))
  | FuncCreate(formals, body) ->
      let new_type = next_type_var() in
      let new_env = new_env() in
      new_env.cur_func_return_type <- Some(new_type); (* side effect *)
      let formals_with_type = List.map (fun x -> (x, next_type_var())) formals in
      let formal_types = List.map (fun (_, t) -> t) formals_with_type in
      new_env.scope.variables <- formals_with_type @ new_env.scope.variables; (* side effect *)
      let aBody = annotate_stmts body new_env in
      AFuncCreate(formals_with_type, aBody, TFunc(formal_types, new_type))
  | FuncCallExpr(e, elist) -> 
      let ae = annotate_expr e env in
      let aelist = List.map (fun x -> annotate_expr x env) elist in
      let new_type = next_type_var() in
      AFuncCallExpr(ae, aelist, new_type)
  | ObjAccess(e, s) ->
      let ae = annotate_expr e env in
      let new_type = next_type_var() in
      AObjAccess(ae, s, new_type)
  | ListAccess(e1, e2) -> 
      let ae1 = annotate_expr e1 env in
      let ae2 = annotate_expr e2 env in
      let new_type = next_type_var() in
      AListAccess(ae1, ae2, new_type)
  | ListCreate(exprs) ->
      let aExprs = List.map (fun x -> annotate_expr x env) exprs in
      let new_type = next_type_var() in
      AListCreate(aExprs, TList(new_type))
  | Sublist(e, eleft, eright) -> 
      let ae = annotate_expr e env in
      let aeleft = match eleft with
      | Some(x) -> Some(annotate_expr x env)
      | None -> None in
      let aeright = match eright with
      | Some(x) -> Some(annotate_expr x env)
      | None -> None in
      let new_type = next_type_var() in
      ASublist(ae, aeleft, aeright, TList(new_type))
  | ObjCreate(props) -> 
      let rec check_dups = function
          [] -> false
        | (h::t) -> if List.mem h t then true else check_dups t
      in
      if check_dups (List.map (fun (name, _) -> name) props)
      then failwith "Duplicate property names on object."
      else
      let aProps = List.map (fun (name, e) -> (name, annotate_expr e env)) props in
      let types = List.map (fun (name, ae) -> (name, type_of ae)) aProps in
      AObjCreate(aProps, TObjCreate(types))
  | Binop(e1, op, e2) -> 
      let ae1 = annotate_expr e1 env in
      let ae2 = annotate_expr e2 env in
      let new_type = next_type_var() in
      ABinop(ae1, op, ae2, new_type)
  | Not(e) ->
      let ae = annotate_expr e env in
      ANot(ae, TBool)

and annotate_assign (e1 : Ast.expr) (e2 : Ast.expr) (env : environment) (must_exist : bool) : Sast.aExpr * Sast.aExpr = 
  let ae2 = annotate_expr e2 env in
  match e1 with
  | Id(x) -> 
      let typ = find_variable env.scope x in
      (match typ with
      | Some(t) -> 
          (AId(x, false, t), ae2)
      | None -> 
        if must_exist
        then failwith "Invalid assignment."
        else 
          let new_type = next_type_var() in
          env.scope.variables <- (x, new_type) :: env.scope.variables; (* side effect *)
          (AId(x, true, new_type), ae2))
  | ObjAccess(_, _) ->
      let ae1 = annotate_expr e1 env in
      (ae1, ae2)
  | ListAccess(_, _) ->
      let ae1 = annotate_expr e1 env in
      (ae1, ae2)
  | _ -> failwith "Invalid assignment."

and annotate_stmt (s : Ast.stmt) (env : environment) : Sast.aStmt =
	match s with
  | Return(expr) -> 
    (match env.cur_func_return_type with
    | None -> failwith "Invalid return statement."
    | Some(x) -> 
        let ae = annotate_expr expr env in
        AReturn(ae, x))
  | Assign(e1, e2) -> 
      let (ae1, ae2) = annotate_assign e1 e2 env false in
      AAssign(ae1, ae2)
  | If(conds, elsebody) -> 
	  	let aIfFunc = fun cond -> 
        let ae = annotate_expr cond.condition env in
        let scoped_env = nest_scope env in
        let aBody = annotate_stmts cond.body scoped_env in
        (ae, aBody)
      in
      (match elsebody with
      | Some(x) -> 
        let scoped_env = nest_scope env in
        let aElse = annotate_stmts x scoped_env in
        AIf(List.map aIfFunc conds, Some(aElse))
      | None -> AIf(List.map aIfFunc conds, None))
  | For(a1, e, a2, body) ->
    let scoped_env = nest_scope env in
    let aa1 = 
      (match a1 with
      | Some(e1, e2) -> Some(annotate_assign e1 e2 scoped_env false)
      | None -> None)
    in
    let ae = 
      (match e with
      | Some(x) -> Some(annotate_expr x scoped_env)
      | None -> None)
    in
    let aa2 = 
      (match a2 with
      | Some(e1, e2) -> Some(annotate_assign e1 e2 scoped_env true)
      | None -> None)
    in
    let aBody = annotate_stmts body scoped_env in
    AFor(aa1, ae, aa2, aBody)
  | While(e, body) -> 
      let ae = annotate_expr e env in
      let scoped_env = nest_scope env in
      let aBody = annotate_stmts body scoped_env in
      AWhile(ae, aBody)
  | FuncCallStmt(e, elist) -> 
      let ae = annotate_expr e env in
      let aelist = List.map (fun x -> annotate_expr x env) elist in
      AFuncCallStmt(ae, aelist)

and annotate_stmts (stmts : Ast.stmt list) (env : environment) : Sast.aStmt list =
  List.map (fun x -> annotate_stmt x env) stmts
  
let annotate_program (p : Ast.program) : Sast.aProgram =
  reset_type_vars();
  let dist_type = next_type_var() in
  let dist_return_type = next_type_var() in
  let env = new_env() in
  env.scope.variables <- [
    ("print", TFunc([TList(TChar)], 
                    TList(TChar)));
    ("read", TFunc([],
                   TList(TList(TChar))));
    ("printFile", TFunc([TList(TChar); TList(TChar)],
                        TList(TList(TChar))));
    ("readFile", TFunc([TList(TChar)],
                       TList(TList(TChar))));
    ("download", TFunc([TList(TChar)],
                       TList(TChar)));
    ("distribute", TFunc([TList(dist_type); TFunc([dist_type],dist_return_type)], 
                         TList(dist_return_type)))
  ];
  annotate_stmts p env

(* TODO BOUND VS FREE VARIABLES *)
(* bv = stack of bound variables for which current expression is in scope *)
(* fv = hashtable of known free variables *)
(* 
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
*)



(* collect constraints for unification *)



let rec collect_expr (e : Sast.aExpr) : (Sast.t * Sast.t) list = 
  match e with 
    | ANumLit(num, ty) -> []
    | ABoolLit(boo, ty) -> []
    | ACharLit(c, ty) ->   []

    | AId(name, seenBefore, ty) -> []
    | AFuncCreate(params, body, ty) -> collect_stmts body

    | AFuncCallExpr(fExpr, params, ty) -> 
      let ftype = type_of fExpr in
      let myCreatedType = TFunc(List.map (fun p-> type_of p) params, ty) in
      [(myCreatedType, ftype)] @ (List.fold_left (fun l p -> l@ collect_expr p) [] params) @ collect_expr fExpr
    | AObjAccess(oExpr, name, ty) -> 
      let oType = type_of oExpr in
      [(oType, TObjAccess(name, ty))]
    | AListAccess(lExpr, idx, return_type) ->
      let idx_type = type_of idx in
      let list_type = type_of lExpr in
      [(list_type, TList(return_type));(idx_type,TNum)]
    | AListCreate(members, ty) -> 
      (*all must be same*)
      (match ty with 
      | TList(x) -> List.map (fun m-> (type_of m, x)) members
      | _ -> failwith "not a list!?")
    
    | ASublist(mylist, e1, e2, ty) ->
      let e1_constraints = 
        match e1 with
          | Some(x) -> [(type_of x, TNum)]
          | None -> []
      in
      let e2_constraints = 
        match e2 with 
          | Some(x) -> [(type_of x, TNum)]
          | None -> []
      in
      let list_type = type_of mylist in
      [(list_type, ty)] @ e1_constraints @ e2_constraints
      
    | AObjCreate(props, ty) -> [] (*added in annotate*)
    | ABinop(e1, op, e2, ty) ->
      let e1t = type_of e1 in
      let e2t = type_of e2 in
      (match op with
      | Add | Sub | Mult | Div | Mod -> [(e1t, TNum); (e2t, TNum); (ty, TNum)]
      | Equal | Neq -> [(e1t,e2t); (ty, TBool)]
      | Less | Leq | Greater | Geq -> [(e1t, TNum); (e2t, TNum); (ty, TBool)]
      | Concat -> let new_type = next_type_var() in
        [(e1t, e2t); (ty, e1t); (ty, TList(new_type))]
      | And | Or -> [(e1t, TBool); (e2t, TBool); (ty, TBool)])
    | ANot(e, ty) -> [(type_of e, TBool); (ty, TBool)]

and collect_stmt (s : aStmt) : (Sast.t * Sast.t) list =
  match s with
    | AReturn(expr, func_return_t) -> 
        let constraints_from_expr = collect_expr expr in
        (type_of expr,func_return_t) :: (constraints_from_expr)
    | AIf(conds, the_else) -> 
        let cond_func = fun list_so_far (ex, stlist) ->
          let expr_list = (type_of ex, TBool) :: collect_expr ex in
          let my_list = expr_list @ (collect_stmts stlist) in
          list_so_far @ my_list
        in
        let list_after_ifs = List.fold_left cond_func [] conds in
        let list_after_cond = (match the_else with
          | Some(x) -> list_after_ifs @ collect_stmts x
          | None -> [])
        in
        list_after_cond @ list_after_ifs
    | AFor(assign1, e, assign2, stmts) ->
      let list_from_a1 = 
        (match assign1 with
          | None -> []
          | Some(e1, e2) -> 
            let aa = AAssign(e1, e2) in
            collect_stmt aa)
        in
      let list_from_a2 = 
        (match assign2 with
          | None -> []
          | Some(e1, e2) -> 
            let aa = AAssign(e1, e2) in
            collect_stmt aa)
        in
      let list_from_e = 
        match e with
            None -> []
          | Some(ex) -> (type_of ex, TBool) :: collect_expr ex
        in
      let list_from_topbit = list_from_a1 @ list_from_a2 @ list_from_e in
      list_from_topbit @ (collect_stmts stmts)
    | AWhile(expr, stmts) -> 
      let list_from_expr = (type_of expr, TBool) :: collect_expr expr in
      list_from_expr @ collect_stmts stmts
    | AAssign(lhs, rhs) -> (type_of lhs, type_of rhs) :: (collect_expr lhs @ collect_expr rhs)
    | AFuncCallStmt (fExpr, params) -> 
      let ftype = type_of fExpr in
      let myCreatedType = TFunc(List.map (fun p-> type_of p) params, next_type_var()) in
      [(myCreatedType, ftype)] @ (List.fold_left (fun l p -> l @ collect_expr p) [] params) @ collect_expr fExpr

and collect_stmts (stmts : Sast.aStmt list) : (Sast.t * Sast.t) list = 
 List.fold_left (fun l s -> l @ (collect_stmt s)) [] stmts

let collect (cprog : Sast.aProgram) : (Sast.t * Sast.t) list =
  collect_stmts cprog


(*and annotate_stmts (stmts : Ast.stmt list) (env : environment) : Sast.aStmt list =*)


(*original collect
    [] -> u
  | AVar (_, _) :: r -> collect r u
  | AFun (_, ae, _) :: r -> collect (ae :: r) u
  | AFunCall (ae1, ae2, a) :: r ->
      let (f, b) = (type_of ae1, type_of ae2) in
      collect (ae1 :: ae2 :: r) ((f, Arrow (b, a)) :: u)*)

(* collect the constraints and perform unification *)
(*let infer (p : Ast.program) : Sast.aProgram =
  reset_type_vars();
  let annotatedP = annotate p in
  let collectedP = collect [annotatedP] [] in
  let subs = unify collectedP in
  apply subs (type_of ae)*)

