(* Semantic analysis
	- input: abstract syntax tree
	- build symbol table
	- run type inference algorithm
	- output: semantic tree
*)

open Ast
open Sast

(*************************
****** UTILITIES *********
*************************)

type symbol_table = {
	parent : symbol_table option;
  mutable variables : (string * Sast.t) list;
}

type environment = {
	mutable cur_func_return_type : Sast.t option; (* Function’s return type *)
	scope : symbol_table;        (* symbol table for vars *)
}

let rec find_variable (scope : symbol_table) (name : string) : Sast.t option =
  try
    let (_, typ) = List.find (fun (s, _) -> s = name) scope.variables in
    Some(typ)
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_variable parent name
      | _ -> None

let code1 = ref (Char.code 'A')
let code2 = ref (Char.code 'A')

let reset_type_vars() = 
  (code1 := Char.code 'A'; code2 := Char.code 'A')

let next_type_var() : Sast.t =
  let c1 = !code1 in
  let c2 = !code2 in
    if c2 = Char.code 'Z'
    then code2 := Char.code 'a'
    else incr code2;
    if c2 = Char.code 'z'
    then (incr code1; code2 := Char.code 'A')
    else ();
    if c1 = Char.code 'Z'
    then code1 := Char.code 'a'
    else ();
    let name = (Char.escaped (Char.chr c1)) ^ (Char.escaped (Char.chr c2)) in
    TVar(name)

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

(* inside a for loop or if block, nest scope *)
let nest_scope (env : environment) : environment =
  let s = { variables = []; parent = Some(env.scope) } in
  { env with scope = s; } 

(* inside a function, nest entire env *)
let new_env() : environment = 
  let dist_type = next_type_var() in
  let dist_return_type = next_type_var() in
  let where_type = next_type_var() in
  let mapping_type = next_type_var() in
  let mapped_type = next_type_var() in
  let find_type = next_type_var() in
  let split_type = next_type_var() in
  let utilities = [
    ("print", TFunc([TList(TChar)], 
                    TList(TChar)));
    ("read", TFunc([],
                   TList(TList(TChar))));
    ("printFile", TFunc([TList(TChar); TList(TChar)],
                        TList(TChar)));
    ("readFile", TFunc([TList(TChar)],
                       TList(TList(TChar))));
    ("download", TFunc([TList(TChar)],
                       TList(TChar)));
    ("distribute", TFunc([TList(dist_type); TFunc([dist_type],dist_return_type)], 
                         TList(dist_return_type)));
    ("where", TFunc([TList(where_type); TFunc([where_type], TBool)],
                    TList(where_type)));
    ("map", TFunc([TList(mapping_type); TFunc([mapping_type], mapped_type)],
                  TList(mapped_type)));
    ("find", TFunc([TList(find_type); find_type], 
                   TNum));
    ("split", TFunc([TList(split_type); split_type],
                    TList(TList(split_type))));
    ("range", TFunc([TNum; TNum], 
                    TList(TNum)))
  ] in
  let s = { variables = utilities; parent = None } in
  { scope = s; cur_func_return_type = None; }





(*************************
****** ANNOTATE **********
*************************)

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
  
let annotate_prog (p : Ast.program) : Sast.aProgram =
  reset_type_vars();
  let env = new_env() in
  annotate_stmts p env





(*************************
****** COLLECT ***********
*************************)

let rec collect_expr (e : Sast.aExpr) : (Sast.t * Sast.t) list = 
  match e with 
    | ANumLit(num, ty) -> []
    | ABoolLit(boo, ty) -> []
    | ACharLit(c, ty) ->   []
    | AId(name, seenBefore, ty) -> []
    | AFuncCreate(params, body, ty) -> 
        collect_stmts body
    | AFuncCallExpr(fExpr, params, ty) -> 
        let ftype = type_of fExpr in
        let myCreatedType = TFunc(List.map (fun p -> type_of p) params, ty) in
        (List.fold_left (fun l p -> l @ collect_expr p) [] params) @ collect_expr fExpr @ [(myCreatedType, ftype)]
    | AObjAccess(oExpr, name, ty) -> 
        let oType = type_of oExpr in
        (oType, TObjAccess(name, ty)) :: collect_expr oExpr
    | AListAccess(lExpr, iExpr, return_type) ->
        let idx_type = type_of iExpr in
        let list_type = type_of lExpr in
        [(list_type, TList(return_type));(idx_type,TNum)] @ collect_expr lExpr @ collect_expr iExpr
    | AListCreate(members, ty) -> 
        (*all must be same*)
        let constraints = 
          (match ty with 
          | TList(x) -> List.map (fun m -> (type_of m, x)) members
          | _ -> failwith "Internal error, should never happen: list not a list.")
        in
        constraints @ (List.fold_left (fun l m -> l @ collect_expr m) [] members)
    | ASublist(mylist, e1, e2, ty) ->
        let e_constraint = fun e_opt ->
          match e_opt with
            | Some(x) -> (type_of x, TNum) :: collect_expr x
            | None -> []
        in
        let list_type = type_of mylist in
        [(list_type, ty)] @ e_constraint e1 @ e_constraint e2
    | AObjCreate(props, ty) ->
        let prop_exprs = List.map snd props in
        (List.fold_left (fun l p -> l @ collect_expr p) [] prop_exprs)
    | ABinop(e1, op, e2, ty) ->
        let e1t = type_of e1 in
        let e2t = type_of e2 in
        let opc = 
          (match op with
          | Add | Sub | Mult | Div | Mod -> 
              [(e1t, TNum); (e2t, TNum); (ty, TNum)] 
          | Equal | Neq -> 
              [(e1t,e2t); (ty, TBool)]
          | Less | Leq | Greater | Geq -> 
              [(e1t, TNum); (e2t, TNum); (ty, TBool)]
          | Concat -> 
              let new_type = next_type_var() in
              [(e1t, e2t); (ty, e1t); (ty, TList(new_type))]
          | And | Or -> 
              [(e1t, TBool); (e2t, TBool); (ty, TBool)])
        in
        (collect_expr e1) @ (collect_expr e2) @ opc
    | ANot(e, ty) -> 
        let notc = [(type_of e, TBool); (ty, TBool)] in
        (collect_expr e) @ notc

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
    | AAssign(lhs, rhs) -> (collect_expr lhs @ collect_expr rhs) @ [(type_of lhs, type_of rhs)]
    | AFuncCallStmt (fExpr, params) -> 
      let ftype = type_of fExpr in
      let myCreatedType = TFunc(List.map (fun p-> type_of p) params, next_type_var()) in
      [(myCreatedType, ftype)] @ (List.fold_left (fun l p -> l @ collect_expr p) [] params) @ collect_expr fExpr

and collect_stmts (stmts : Sast.aStmt list) : (Sast.t * Sast.t) list = 
 List.fold_left (fun l s -> l @ (collect_stmt s)) [] stmts

let collect_prog (cprog : Sast.aProgram) : (Sast.t * Sast.t) list =
  collect_stmts cprog





(*************************
****** UNIFY *************
*************************)

type substitution = (string * Sast.t) list

(* check if a variable occurs in a term *)
let rec occurs (x : string) (typ : Sast.t) : bool =
  match typ with
  | TVar(name) -> x = name
  | TFunc(params, y) -> List.exists (fun param -> occurs x param) params || occurs x y
  | TList(y) -> occurs x y
  | TObjCreate(props) -> List.exists (fun prop -> occurs x (snd prop)) props
  | TObjAccess(_, y) -> occurs x y
  | TNum -> false
  | TChar -> false
  | TBool -> false

(* substitute term s for all occurrences of var x in term t *)
let rec subst (s : Sast.t) (x : string) (typ : Sast.t) : Sast.t =
  match typ with
  | TVar(name) -> if x = name then s else typ
  | TFunc(params, y) -> TFunc(List.map (fun param -> subst s x param) params, subst s x y)
  | TList(y) -> TList(subst s x y)
  | TObjCreate(props) -> TObjCreate(List.map (fun prop -> (fst prop, subst s x (snd prop))) props)
  | TObjAccess(name, y) -> TObjAccess(name, subst s x y)
  | TNum -> typ
  | TChar -> typ
  | TBool -> typ

(* apply a substitution to t right to left *)
let apply (s : substitution) (typ : Sast.t) : Sast.t =
  List.fold_right (fun (x, e) -> subst e x) s typ

let rec copy_type (typ : Sast.t) (table : symbol_table) : Sast.t =
  match typ with
  | TVar(name) -> 
      let typ = find_variable table name in
      (match typ with
      | Some(x) -> x
      | None ->
          let new_type = next_type_var() in
          table.variables <- (name, new_type) :: table.variables;
          new_type)
  | TFunc(params, y) -> TFunc(List.map (fun param -> copy_type param table) params, copy_type y table)
  | TList(y) -> TList(copy_type y table)
  | TObjCreate(props) -> TObjCreate(List.map (fun prop -> (fst prop, copy_type (snd prop) table)) props)
  | TObjAccess(name, y) -> TObjAccess(name, copy_type y table)
  | TNum -> typ
  | TChar -> typ
  | TBool -> typ

(* unify one pair *)
let rec unify_one (a : Sast.t) (b : Sast.t) (in_copy: bool) : substitution =
  match (a, b) with
  | (TVar(x), TVar(y)) -> 
      if x = y then [] else [(x, b)]
  | (TVar(x), (TList(_) as z))         | ((TList(_) as z), TVar(x))
  | (TVar(x), (TObjCreate(_) as z))    | ((TObjCreate(_) as z), TVar(x))
  | (TVar(x), (TObjAccess(_, _) as z)) | ((TObjAccess(_, _) as z), TVar(x))
  | (TVar(x), (TNum as z))             | ((TNum as z), TVar(x))
  | (TVar(x), (TChar as z))            | ((TChar as z), TVar(x))
  | (TVar(x), (TBool as z))            | ((TBool as z), TVar(x)) ->
      [(x, z)]
  | (TVar(x), (TFunc(_, _) as z))      | ((TFunc(_, _) as z), TVar(x))
      -> 
      print_string ("\nID " ^ x ^ " " ^ Sast.string_of_type z ^ "\n");
      [(x, z)]
  | (TFunc(params1, x), (TFunc(params2, y) as z)) ->
      print_string "\nIM HERE\n"; 
      print_string (String.concat ", " (List.map Sast.string_of_type params1) ^ " ---> " ^ Sast.string_of_type x);
      print_string "\n";
      print_string (String.concat ", " (List.map Sast.string_of_type params2) ^ " ---> " ^ Sast.string_of_type y);
      print_string "\n";
      if in_copy && false
      then
        (try
        let pairs = List.map2 (fun u v -> (v,u)) params1 params2 in
          unify ((x,y)::pairs) in_copy
        with Invalid_argument(_) ->
          failwith "Type mismatch: Calling function with wrong # of parameters.")
      else
        let copy = copy_type z { variables = []; parent = None } in
        let new_params1 = List.map (fun p -> copy_type p { variables = []; parent = None }) params1 in
        (match copy with
        | TFunc(new_params2, new_y) ->
            print_string (String.concat ", " (List.map Sast.string_of_type new_params1) ^ " ---> " ^ Sast.string_of_type x);
            print_string "\n";
            print_string (String.concat ", " (List.map Sast.string_of_type new_params2) ^ " ---> " ^ Sast.string_of_type new_y);
            print_string "\n";
            (try
            (* PUTTING PARAMS1 ON THE RIGHT SIDE OF THE PAIRS IS INTENTIONAL, DONT MESS WITH IT *)
            let pairs = List.map2 (fun u v -> (u,v)) new_params2 params1 in
            unify ((x,new_y)::pairs) true
            with Invalid_argument(_) ->
              failwith "Type mismatch: Calling function with wrong # of parameters.")
        | _ -> failwith "Internal error, should never happen: copying TFunc failed.")
  | (TFunc(_, _), TList(_))         | (TList(_), TFunc(_, _)) ->
      failwith "Type mismatch: function with list."
  | (TFunc(_, _), TObjCreate(_))    | (TObjCreate(_), TFunc(_, _)) ->
      failwith "Type mismatch: function with object."
  | ((TFunc(_, _) as z), TObjAccess(_, y)) | (TObjAccess(_, y), (TFunc(_, _) as z)) ->
      unify_one y z in_copy
  | (TFunc(_, _), TNum)             | (TNum, TFunc(_, _))
  | (TFunc(_, _), TChar)            | (TChar, TFunc(_, _))
  | (TFunc(_, _), TBool)            | (TBool, TFunc(_, _)) ->
      failwith "Type mismatch: function with primitive."
  | (TList(x), TList(y)) -> 
      unify_one x y in_copy
  | ((TList(_) as z), TObjAccess(_, y)) | (TObjAccess(_, y), (TList(_) as z))  ->
      unify_one y z in_copy
  | (TList(_), TObjCreate(_)) | (TObjCreate(_), TList(_)) ->
      failwith "Type mismatch: list with object."
  | (TList(_), TNum)          | (TNum, TList(_))
  | (TList(_), TChar)         | (TChar, TList(_))
  | (TList(_), TBool)         | (TBool, TList(_))->
      failwith "Type mismatch: list with primitive."
  | (TObjCreate(props1), TObjCreate(props2)) ->
      let mapper = fun prop1 -> 
        try
        let found = List.find (fun prop2 -> (fst prop1) = (fst prop2)) props2 in
          ((snd prop1), (snd found))
        with Not_found ->
          failwith "Type mistmatch: object with object."
      in
      unify (List.map mapper props1) in_copy
  | (TObjCreate(props), TObjAccess(name, y)) | (TObjAccess(name, y), TObjCreate(props)) ->
      (try
      let found = List.find (fun cProp -> (fst cProp) = name) props in
        unify_one (snd found) y in_copy
      with Not_found -> 
        failwith "Type mistmatch: property does not exist on object.")
  | (TObjCreate(_), TNum)  | (TNum, TObjCreate(_))
  | (TObjCreate(_), TChar) | (TChar, TObjCreate(_))
  | (TObjCreate(_), TBool) | (TBool, TObjCreate(_)) ->
      failwith "Type mistmatch: object with primitive."
  | (TObjAccess(_, x), TObjAccess(_, y)) ->
      unify_one x y in_copy
  | (TObjAccess(_, y), (TNum as z))  | ((TNum as z), TObjAccess(_, y))
  | (TObjAccess(_, y), (TChar as z)) | ((TChar as z), TObjAccess(_, y))
  | (TObjAccess(_, y), (TBool as z)) | ((TBool as z), TObjAccess(_, y)) ->
      unify_one y z in_copy
  | (TNum, TChar)  | (TChar, TNum)
  | (TNum, TBool)  | (TBool, TNum)
  | (TChar, TBool) | (TBool, TChar) ->
      failwith "Type mismatch: primitive with primitive."
  | (TNum, TNum) 
  | (TChar, TChar) 
  | (TBool, TBool) -> 
      []

(* unify a list of pairs *)
and unify (s : (Sast.t * Sast.t) list) (in_copy: bool) : substitution =
  match s with
  | [] -> []
  | (x, y) :: tl ->
      let t2 = unify tl in_copy in
      let t1 = unify_one (apply t2 x) (apply t2 y) in_copy in
      t1 @ t2





(*************************
****** INFER *************
*************************)

let rec apply_expr (ae : Sast.aExpr) (subs : substitution) : Sast.aExpr = 
  match ae with 
    | ANumLit(n, ty) -> 
        ANumLit(n, apply subs ty)
    | ABoolLit(b, ty) -> 
        ABoolLit(b, apply subs ty)
    | ACharLit(c, ty) ->
        ACharLit(c, apply subs ty)
    | AId(name, seenBefore, ty) ->
        AId(name, seenBefore, apply subs ty)
    | AFuncCreate(params, body, ty) ->
        let new_params = List.map (fun (name, typ) -> (name, apply subs typ)) params in
        AFuncCreate(new_params, apply_stmts body subs, apply subs ty)
    | AFuncCallExpr(fExpr, params, ty) -> 
        let new_params = List.map (fun e -> apply_expr e subs) params in
        AFuncCallExpr(apply_expr fExpr subs, new_params, apply subs ty)
    | AObjAccess(oExpr, name, ty) -> 
        AObjAccess(apply_expr oExpr subs, name, apply subs ty)
    | AListAccess(lExpr, iExpr, ty) ->
        AListAccess(apply_expr lExpr subs, apply_expr iExpr subs, apply subs ty)
    | AListCreate(members, ty) -> 
        let new_members = List.map (fun m -> apply_expr m subs) members in
        AListCreate(new_members, apply subs ty)
    | ASublist(lExpr, e1, e2, ty) ->
        let e_opt_func = fun e_opt ->
          match e_opt with
          | Some(e) -> Some(apply_expr e subs)
          | None -> None
        in
        ASublist(apply_expr lExpr subs, e_opt_func e1, e_opt_func e2, apply subs ty)
    | AObjCreate(props, ty) ->
        let new_props = List.map (fun (name, e) -> (name, apply_expr e subs)) props in
        AObjCreate(new_props, apply subs ty)
    | ABinop(e1, op, e2, ty) ->
        ABinop(apply_expr e1 subs, op, apply_expr e2 subs, apply subs ty)
    | ANot(e, ty) -> 
        ANot(apply_expr e subs, apply subs ty)

and apply_stmt (s : aStmt) (subs : substitution) : Sast.aStmt = 
  match s with
    | AReturn(expr, func_return_t) -> 
        AReturn(apply_expr expr subs, apply subs func_return_t)
    | AIf(conds, the_else) -> 
        let cond_func = fun (expr, body) ->
          (apply_expr expr subs, apply_stmts body subs)
        in
        let else_func = fun e ->
          (match e with
          | Some(body) -> Some(apply_stmts body subs)
          | None -> None)
        in
        AIf(List.map cond_func conds, else_func the_else)
    | AFor(assign1, expr, assign2, stmts) ->
        let assign_func = fun a ->
          (match a with
          | Some(e1, e2) -> Some(apply_expr e1 subs, apply_expr e2 subs)
          | None -> None)
        in
        let expr_func = fun e ->
          (match e with
          | Some(e1) -> Some(apply_expr e1 subs)
          | None -> None)
        in
        AFor(assign_func assign1, expr_func expr, assign_func assign2, apply_stmts stmts subs)
    | AWhile(expr, stmts) -> 
        AWhile(apply_expr expr subs, apply_stmts stmts subs)
    | AAssign(lhs, rhs) ->
        AAssign(apply_expr lhs subs, apply_expr rhs subs)
    | AFuncCallStmt(fExpr, params) -> 
        let new_params = List.map (fun e -> apply_expr e subs) params in
        AFuncCallStmt(apply_expr fExpr subs, new_params)

and apply_stmts (stmts : Sast.aStmt list) (subs : substitution) : Sast.aStmt list =
  List.map (fun s -> apply_stmt s subs) stmts

let infer_prog (p : Ast.program) : Sast.aProgram =
  let ap = annotate_prog p in
  let constraints = collect_prog ap in
  let subs = unify (List.rev constraints) false in
  apply_stmts ap subs




