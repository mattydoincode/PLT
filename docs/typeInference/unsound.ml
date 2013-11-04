
<!-- saved from url=(0049)http://okmij.org/ftp/ML/generalization/unsound.ml -->
<html><head><meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1"><style type="text/css"></style></head><body><pre style="word-wrap: break-word; white-space: pre-wrap;">(*
  Simple Hindley-Milner type checker for pure lambda-calculus with let
  Explanation of efficient generalization -- Remy algorithm

  This code is written to illustrate the need to check the type environment
  when generalizing.

  The generalization function below is unsound: it quantifies free type
  variables in a type with no regard for the type environment.
  The problem is fixed in sound_*.ml files.
*)

(* The language: lambda-calculus with let *)
type varname = string

type exp = 
  | Var of varname                      (* variable                *)
  | App of exp * exp                    (* application: e1 e2      *)
  | Lam of varname * exp                (* abstraction: fun x -&gt; e *)
  | Let of varname * exp * exp          (* let x = e in e2         *)
;;

(* The types to infer *)
(* Types without QVar (quantified variables) are simple types;
   those containing QVar are type schemas.
   Since quantifiers are always on the outside in the HM system, 
   they are implied and not explicitly represented.
*)
type qname = string
type typ = 
  | TVar of tv ref               (* type (schematic) variable *)
  | QVar of qname                (* quantified type variable *)
  | TArrow of typ * typ
and tv = Unbound of string | Link of typ
;;

let gensym_counter = ref 0
let reset_gensym : unit -&gt; unit =
  fun () -&gt; gensym_counter := 0
;;

let gensym : unit -&gt; string = fun () -&gt;
  let n = !gensym_counter in
  let () = incr gensym_counter in
  if n &lt; 26 then String.make 1 (Char.chr (Char.code 'a' + n))
            else "t" ^ string_of_int n
;;

(* Make a fresh type variable *)
let newvar : unit -&gt; typ =
 fun () -&gt; TVar (ref (Unbound (gensym ())))
;;

(* check to see if a TVar (the first argument) occurs in the type
   given as the second argument. Fail if it does.
*)
let rec occurs : tv ref -&gt; typ -&gt; unit = fun tvr -&gt; function
  | TVar tvr' -&gt; if tvr == tvr' then failwith "occurs check"
  | TVar {contents = Link ty} -&gt; occurs tvr ty
  | TArrow (t1,t2) -&gt; occurs tvr t1; occurs tvr t2
  | _ -&gt; ()
;;

(* Simplistic.  No path compression *)
(* Also, QVar are unexpected: they should've been instantiated *)
let rec unify : typ -&gt; typ -&gt; unit = fun t1 t2 -&gt;
  if t1 == t2 then ()                   (* t1 and t2 are physically the same *)
  else match (t1,t2) with
  | (TVar ({contents = Unbound _} as tv),t')
  | (t',TVar ({contents = Unbound _} as tv)) -&gt; occurs tv t'; tv := Link t'
  | (TVar {contents = Link t1},t2)
  | (t1,TVar {contents = Link t2}) -&gt; unify t1 t2
  | (TArrow (tyl1,tyl2), TArrow (tyr1,tyr2)) -&gt;
      unify tyl1 tyr1;
      unify tyl2 tyr2
  (* everything else is error *)
;;

(* The type environment *)
type env = (varname * typ) list
;;

(* Unsound generalization: ignores the environment  *)
(* and converts all free TVar in the type into QVar *)
let rec gen : typ -&gt; typ = function
  | TVar {contents = Unbound name} -&gt; QVar name
  | TVar {contents = Link ty}      -&gt; gen ty
  | TArrow (ty1,ty2) -&gt; TArrow (gen ty1, gen ty2)
  | ty -&gt; ty
;;

(* instantiation: replace schematic variables with fresh TVar
*)
let inst : typ -&gt; typ = 
  let rec loop subst = function
    | QVar name -&gt; 
        begin
          try (List.assoc name subst, subst)
          with Not_found -&gt;
            let tv = newvar () in
            (tv, (name,tv)::subst)
        end
    | TVar {contents = Link ty} -&gt; loop subst ty
    | TArrow (ty1,ty2) -&gt; 
        let (ty1,subst) = loop subst ty1 in
        let (ty2,subst) = loop subst ty2 in
        (TArrow (ty1,ty2), subst)
    | ty -&gt; (ty, subst)
  in fun ty -&gt; fst (loop [] ty)
;;


(* Trivial type checker. Type checking errors are delivered
   as exceptions
*)
let rec typeof : env -&gt; exp -&gt; typ = fun env -&gt; function
  | Var x     -&gt; inst (List.assoc x env)
  | Lam (x,e) -&gt; 
      let ty_x = newvar () in
      let ty_e = typeof ((x,ty_x)::env) e in
      TArrow(ty_x,ty_e)
  | App (e1,e2) -&gt;
      let ty_fun = typeof env e1 in
      let ty_arg = typeof env e2 in
      let ty_res = newvar () in
      unify ty_fun (TArrow (ty_arg,ty_res));
      ty_res
  | Let (x,e,e2) -&gt; 
      let ty_e = typeof env e in
      typeof ((x,gen ty_e)::env) e2
;;

let id = Lam("x",Var"x");;
let c1 = Lam("x",Lam("y",App (Var"x",Var"y")));;

let TArrow (TVar {contents = Unbound "a"}, TVar {contents = Unbound "a"})
   = reset_gensym (); 
     typeof [] id
;;

let 
 TArrow
 (TVar
   {contents =
     Link
      (TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "c"}))},
 TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "c"}))
 =
   reset_gensym (); 
   typeof [] c1
;;


let 
 TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "b"})
 =
 reset_gensym (); 
 typeof [] (Let ("y",Lam ("z",Var"z"), Var"y"));;

let
 TArrow (TVar {contents = Unbound "a"},
  TArrow (TVar {contents = Unbound "c"}, TVar {contents = Unbound "c"}))
 =
 reset_gensym (); 
 typeof [] (Lam ("x", Let ("y",Lam ("z",Var"z"), Var"y")));;

let
 TArrow (TVar {contents = Unbound "a"},
   TVar
    {contents = Link (TVar {contents = Link (TVar {contents = Unbound "a"})})})
 =
 reset_gensym (); 
 typeof [] (Lam ("x", Let ("y",Lam ("z",Var"z"), 
                                    App (Var"y",Var"x"))));;
try 
 reset_gensym (); 
 typeof [] (Lam ("x",App (Var"x",Var"x")));
 assert false;
 with Failure e -&gt; print_endline e
;;

try 
 reset_gensym (); 
 typeof [] (Let ("x",Var"x",Var"x"));
 assert false;
 with Not_found -&gt; print_endline "unbound var"
;;

(* id can be `self-applied', on the surface of it *)
let 
 TVar
 {contents =
   Link
    (TVar
      {contents =
        Link
         (TArrow (TVar {contents = Unbound "c"},
           TVar {contents = Unbound "c"}))})}
 =
 reset_gensym (); 
 typeof [] (Let ("id",id, App (Var"id",Var"id")));;

let 
 TArrow (TVar {contents = Unbound "i"}, TVar {contents = Unbound "i"})
 =
 reset_gensym (); 
 typeof [] (Let ("x",c1,
                    Let ("y",
                          Let ("z",App(Var"x",id), Var "z"),
                         Var"y")));;

(*
fun x -&gt; fun y -&gt; let x = x y in fun x -&gt; y x;;
- : (('a -&gt; 'b) -&gt; 'c) -&gt; ('a -&gt; 'b) -&gt; 'a -&gt; 'b = &lt;fun&gt;
*)
let 
 TArrow
 (TVar
   {contents =
     Link
      (TArrow
        (TVar
          {contents =
            Link
             (TArrow (TVar {contents = Unbound "d"},
               TVar {contents = Unbound "e"}))},
        TVar {contents = Unbound "c"}))},
 TArrow
  (TVar
    {contents =
      Link
       (TArrow (TVar {contents = Unbound "d"}, TVar {contents = Unbound "e"}))},
  TArrow (TVar {contents = Unbound "d"}, TVar {contents = Unbound "e"})))
 =
 reset_gensym (); 
 typeof [] (Lam ("x", Lam("y",Let ("x",App (Var"x",Var"y"),
                                  Lam ("x",App (Var"y",Var"x"))))));;

(* unsound generalization ! *)
let
 TArrow (TVar {contents = Unbound "a"}, TVar {contents = Unbound "b"})
 =
 reset_gensym (); 
 typeof [] (Lam ("x", Let ("y",Var"x", Var"y")));;

(* unsound generalization ! *)
let 
 TArrow (TVar {contents = Unbound "a"},
  TArrow (TVar {contents = Unbound "c"}, TVar {contents = Unbound "d"}))
 =
 reset_gensym (); 
 typeof [] (Lam ("x", Let ("y",Lam ("z",Var"x"), Var"y")));;

(* unsound generalization ! *)
let
 TArrow
 (TVar
   {contents =
     Link
      (TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "c"}))},
  TArrow (TVar {contents = Unbound "d"}, TVar {contents = Unbound "e"}))
 =
 reset_gensym (); 
 typeof [] (Lam ("x", Let ("y",Lam ("z",App (Var"x",Var"z")), Var"y")));;


(* unsound generalization ! *)
let 
 TArrow
 (TVar
   {contents =
     Link
      (TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "c"}))},
 TArrow (TVar {contents = Unbound "b"}, TVar {contents = Unbound "e"}))
 =
 reset_gensym (); 
 typeof [] (Lam ("x", Lam("y",Let ("x",App (Var"x",Var"y"),
                                    App (Var"x",Var"y")))));;


(* unsound generalization ! *)
let
 TArrow (TVar {contents = Unbound "a"}, TVar {contents = Unbound "d"})
 =
 reset_gensym (); 
 typeof [] (Lam ("x",Let("y",Var"x", App (Var"y",Var"y"))));;

(* unsound generalization ! *)
let
 TArrow
 (TVar
   {contents =
     Link
      (TArrow
        (TArrow (TVar {contents = Unbound "b"},
          TVar {contents = Unbound "b"}),
        TVar {contents = Unbound "c"}))},
  TVar {contents = Unbound "e"})
 =
 reset_gensym (); 
 typeof [] (Lam ("x",
                    Let ("y",
                          Let ("z",App(Var"x",id), Var "z"),
                         Var"y")));;


print_endline "\nAll Done\n";;
</pre></body></html>