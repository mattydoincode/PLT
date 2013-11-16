(******************************************
 * Type inference for simple lambda terms *
 ******************************************)

open Ast

(* INFERENCE *)
let code1 = ref (Char.code 'a')
let code2 = ref (Char.code 'a')

let reset_type_vars() = 
  (code1 := Char.code 'a'; code2 := Char.code 'a')

let next_type_var() : typ =
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
let infer (e : expr) : typ =
  reset_type_vars();
  let ae = annotate e in
  let cl = collect [ae] [] in
  let s = Unify.unify cl in
  Unify.apply s (type_of ae)
