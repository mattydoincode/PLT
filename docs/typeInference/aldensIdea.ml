
(* SYNTAX TREE *)

  type op = 
      Add | Sub | Mult | Div | Mod
    | Equal | Neq | Less | Leq | Greater | Geq
    | Concat | And | Or

  type expr =
      NumLit of float
    | BoolLit of bool
    | Id of string
    | FuncCreate of string list * stmt list
    | FuncCallExpr of expr * expr list
    | ListCreate of expr list
    | ObjectCreate of (string * expr) list  
    | Binop of expr * op * expr

  and stmt =
      Return of expr
    | If of conditional list * stmt list option
    | While of expr * stmt list 
    | Assign of (expr * expr)

  and conditional = {
    condition : expr;
    body : stmt list;
  }

(* SEMANTIC TREE *)

  type typ =
    | TVar of string
    | Arrow of typ * typ

  type stTypedExpr = stExpr * typ

  and stExpr =
      StNumLit of float
    | StBoolLit of bool
    | StId of string
    | StFuncCreate of string list * stStmt list
    | StFuncCallExpr of stTypedExpr * stTypedExpr list
    | StListCreate of stTypedExpr list
    | StObjectCreate of (string * stTypedExpr) list  
    | StBinop of stTypedExpr * op * stTypedExpr

  and stStmt =
      StReturn of stTypedExpr
    | StIf of stConditional list * stStmt list option
    | StWhile of stTypedExpr * stStmt list 
    | StAssign of (stTypedExpr * stTypedExpr)

  and stConditional = {
    stCondition : stTypedExpr;
    stBody : stStmt list;
  }

(* INFERENCE *)

  (* type variable generator *)
  let next_type_var (last : string) : string = 
    let len = String.length last in
    let lastChar = last.[len - 1] in
    let lastCode = Char.code lastChar in
      if lastCode < Char.code 'z' 
      then (String.sub last 0 (len - 1)) ^ Char.escaped (Char.chr (lastCode + 1))
      else last ^ "a"

  (* get type from typed expression *)
  let type_of (e : stTypedExpr) : typ = function
      (StNumLit(_), t) -> t
    | (StBoolLit(_), t) -> t
    | (StId(_), t) -> t
    | (StFuncCreate(_, _), t) -> t
    | (StFuncCallExpr(_, _), t) -> t
    | (StListCreate(_), t) -> t
    | (StObjectCreate(_), t) -> t
    | (StBinop(_, _, _), t) -> t

  (* annotate all subexpressions with types *)
  (* bound = stack of bound variables for which current expression is in scope *)
  (* free = hashtable of known free variables *)
  let annotate (e : expr) : stTypedExpr =
    let (free : (string, typ)) = StringMap.make in 
    let rec helper (e : expr) (bound : (string * typ) list) : stTypedExpr = function
        NumLit(f) -> 
      | BoolLit(b) -> 
      | Id(s) -> 
          (* bound variable? *)
          (try let a = List.assoc s bound in AVar (s, a)
          (* known free variable? *)
          with Not_found -> try let a = StringMap.find free s in AVar (s, a)
          (* unknown free variable *)
          with Not_found -> let a = next_type_var() in StringMap.add free s a; AVar (s, a))
      | FuncCreate(formals, body) ->
          (* assign a new type *)
          let a = next_type_var() in
          (* assign a new function name *)
          let name = next_type_var() in
          (* call type on entire body *)
          let ae = helper e ((name, a) :: bound) in
          let newBody = 1 in
          (* create typed function *)
          (StFuncCreate(formals, newBody) * type_of ae)
      | FuncCallExpr(e, actuals) -> 
      | ListCreate(el) ->
      | ObjectCreate(props) ->  
      | Binop(e1, o, e2) ->
    in helper e []

  (* collect constraints for unification *)
  let rec collect (texprs : stTypedExpr list) (u : (typ * typ) list) : (typ * typ) list = function
      [] -> u
    | (StId(_), _) :: r -> collect r u
    | (StFuncCreate(_, body), _) :: r -> collect (body :: r) u

  (* collect the constraints and perform unification *)
  let infer (e : expr) : typ =
    let ae = annotate e in
    let cl = collect [ae] [] in
    let s = Unify.unify cl in
    Unify.apply s (type_of ae)

(* UNIFICATION *)

  (* invariant for substitutions: no id on a lhs occurs in any term earlier  *)
  (* in the list                                                             *)
  type substitution = (string * typ) list

  (* check if a variable occurs in a term *)
  let rec occurs (x : string) (t : typ) : bool =
    match t with
    | TVar y -> x = y
    | Arrow (u, v) -> occurs x u || occurs x v

  (* substitute term s for all occurrences of var x in term t *)
  let rec subst (s : typ) (x : string) (t : typ) : typ =
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
