include Ast

(*************************
**** SAST ****************
*************************)

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
  | AFuncCreate of (string * t) list * aStmt list * t
  | AFuncCallExpr of aExpr * aExpr list * t
  | AObjAccess of aExpr * string * t
  | AListAccess of aExpr * aExpr * t
  | AListCreate of aExpr list * t
  | ASublist of aExpr * aExpr option * aExpr option * t
  | AObjCreate of (string * aExpr) list * t
  | ABinop of aExpr * Ast.op * aExpr * t
  | ANot of aExpr * t

(* annotated statment *)
and aStmt =
    AReturn of aExpr * t
  | AIf of (aExpr * aStmt list) list * aStmt list option
  | AFor of (aExpr * aExpr) option * aExpr option * (aExpr * aExpr) option * aStmt list
  | AWhile of aExpr * aStmt list 
  | AAssign of (aExpr * aExpr)
  | AFuncCallStmt of aExpr * aExpr list

(* annotated program *)
type aProgram = aStmt list


(*************************
**** PRINT SAST **********
*************************)

let string_of_opt string_of = function 
    Some(x) -> string_of x 
  | None -> ""

let rec string_of_type = function
    TVar(s) -> "TVar('" ^ s ^ ")"
  | TFunc(tlist, t) -> "TFunc(" ^ String.concat "," (List.map string_of_type tlist) ^ " -> " ^ string_of_type t ^ ")"
  | TList(t) -> "TList(" ^ string_of_type t ^ ")"
  | TObjCreate(props) -> "TObjCreate(" ^ String.concat "," (List.map (fun (s, t) -> s ^ ":" ^ string_of_type t) props) ^ ")"
  | TObjAccess(s, t) -> "TObjAccess(" ^ s ^ ":" ^ string_of_type t ^ ")"
  | TNum -> "TNum"
  | TChar -> "TChar"
  | TBool -> "TBool"

let sot typ =
  " [" ^ string_of_type typ ^ "]\n"

let rec string_of_expr = function
    ANumLit(n, t) -> string_of_float n ^ sot t
  | ABoolLit(b, t) -> string_of_bool b ^ sot t
  | ACharLit(c, t) -> "'" ^ Char.escaped c ^ "'" ^ sot t
  | AId(s, b, t) -> 
    if b 
    then s ^ " [NEW " ^ string_of_type t ^ "]\n"
    else s ^ sot t
  | AFuncCreate(formals, body, t) -> 
      "(" ^ String.concat ", " (List.map (fun x -> fst x ^ sot (snd x)) formals) ^ ") -> {\n" ^
      String.concat "" (List.map string_of_stmt body) ^ "}" ^ sot t
  | AFuncCallExpr(ae, ael, t) -> 
      string_of_expr ae ^ "(" ^ 
      String.concat ", " (List.map string_of_expr ael) ^ ")" ^ sot t
  | AObjAccess(ae, s, t)  ->
      string_of_expr ae ^ "." ^ s ^ sot t
  | AListAccess(ae1, ae2, t) -> 
      string_of_expr ae1 ^ "[" ^ string_of_expr ae2 ^ "]" ^ sot t
  | AListCreate(ael, t) ->
      "[" ^ String.concat ", " (List.map string_of_expr ael) ^ "]" ^ sot t
  | ASublist(ae, aeleft, aeright, t) ->
      string_of_expr ae ^ "[" ^ 
      string_of_opt string_of_expr aeleft ^ ":" ^ 
      string_of_opt string_of_expr aeright ^ "]" ^ sot t
  | AObjCreate(props, t) ->
      "{\n" ^ String.concat ",\n" (List.map (fun prop -> fst prop ^ ": " ^ string_of_expr (snd prop)) props) ^
      "}" ^ sot t
  | ABinop(ae1, op, ae2, t) ->
      string_of_expr ae1 ^ (match op with
        Add -> " + "    | Sub -> " - "     | Mult -> " * " 
      | Div -> " / "    | Mod -> " % "
      | Equal -> " == " | Neq -> " != "    | Less -> " < "
      | Leq -> " <= "   | Greater -> " > " | Geq -> " >= "
      | Concat -> " ^ " | And -> " && "    | Or -> " || ") ^
      string_of_expr ae2 ^ sot t
  | ANot(ae, t) ->
      "!" ^ string_of_expr ae ^ sot t

and string_of_stmt = function
    AReturn(ae, t) -> "return " ^ string_of_expr ae ^ ";" ^ sot t
  | AIf(conds, elsebody) -> 
      "if" ^ string_of_cond (List.hd conds) ^ String.concat "" 
        (List.map (fun x -> "\nelif" ^ string_of_cond x) (List.tl conds)) ^
        string_of_opt (fun x -> "\nelse {\n" ^ string_of_stmts x ^ "}") elsebody
  | AFor(a1, ae, a2, asl) ->
      "for (" ^ string_of_opt string_of_assign a1 ^ "; " ^ 
                string_of_opt string_of_expr ae ^ "; " ^
                string_of_opt string_of_assign a2 ^ ") {\n" ^ 
      string_of_stmts asl ^ "}"
  | AWhile(ae, asl) -> 
      "while (" ^ string_of_expr ae ^ ") {\n" ^ 
      string_of_stmts asl ^ "}"
  | AAssign(a) -> string_of_assign a ^ ";"
  | AFuncCallStmt(ae, ael) ->
      string_of_expr ae ^ "(" ^ 
      String.concat ", " (List.map string_of_expr ael) ^ ");"

and string_of_stmts stmts = 
  String.concat "\n" (List.map string_of_stmt stmts)

and string_of_cond (ae, asl) =
  " (" ^ string_of_expr ae ^ ") {\n" ^
  string_of_stmts asl ^ "}"

and string_of_assign (e1, e2) = 
  string_of_expr e1 ^ " = " ^ string_of_expr e2

let string_of_prog prog = 
  string_of_stmts prog ^ "\n"


(*************************
**** PRINT CONSTRAINTS ***
*************************)

let string_of_constraints (l : (t * t) list) =  
  String.concat "\n" (List.map (fun (t1, t2) -> string_of_type t1 ^ "  " ^ string_of_type t2) l) ^ "\n"


(*************************
**** PRINT SUBSTITUTIONS *
*************************)

let string_of_subs (s : (string * t) list) =
  String.concat "\n" (List.map (fun (g, t) -> g ^ " " ^ string_of_type t) s) ^ "\n"


(*************************
**** PRINT INFERENCES ****
*************************)

let rec string_of_inferred_expr = function
    ANumLit(_, _) -> ""
  | ABoolLit(_, _) -> ""
  | ACharLit(_, _) -> ""
  | AId(s, b, t) -> 
    if b 
    then s ^ " ==> " ^ string_of_type t ^ "\n"
    else ""
  | AFuncCreate(_, body, _) -> 
      String.concat "" (List.map string_of_inferred_stmt body)
  | AFuncCallExpr(ae, ael, _) -> 
      string_of_inferred_expr ae ^
      String.concat "" (List.map string_of_inferred_expr ael)
  | AObjAccess(ae, _, _)  ->
      string_of_inferred_expr ae
  | AListAccess(ae1, ae2, _) -> 
      string_of_inferred_expr ae1 ^
      string_of_inferred_expr ae2
  | AListCreate(ael, _) ->
      String.concat "" (List.map string_of_inferred_expr ael)
  | ASublist(ae, aeleft, aeright, _) ->
      string_of_inferred_expr ae ^
      string_of_opt string_of_inferred_expr aeleft ^
      string_of_opt string_of_inferred_expr aeright
  | AObjCreate(props, _) ->
      String.concat "" (List.map (fun (_, ae) -> string_of_inferred_expr ae) props)
  | ABinop(ae1, _, ae2, _) ->
      string_of_inferred_expr ae1 ^
      string_of_inferred_expr ae2
  | ANot(ae, _) ->
      string_of_inferred_expr ae

and string_of_inferred_stmt = function
    AReturn(ae, _) -> string_of_inferred_expr ae
  | AIf(conds, elsebody) -> 
      String.concat "" (List.map (fun (ae, asl) -> string_of_inferred_expr ae ^ string_of_inferred_stmts asl) conds) ^
      string_of_opt string_of_inferred_stmts elsebody
  | AFor(a1, ae, a2, asl) ->
      string_of_opt string_of_inferred_assign a1 ^
      string_of_opt string_of_inferred_expr ae ^
      string_of_opt string_of_inferred_assign a2 ^
      string_of_inferred_stmts asl
  | AWhile(ae, asl) -> 
      string_of_inferred_expr ae ^
      string_of_inferred_stmts asl
  | AAssign(a) ->
      string_of_inferred_assign a
  | AFuncCallStmt(ae, ael) ->
      string_of_inferred_expr ae ^
      String.concat "" (List.map string_of_inferred_expr ael)

and string_of_inferred_stmts stmts = 
  String.concat "" (List.map string_of_inferred_stmt stmts)

and string_of_inferred_assign (e1, e2) = 
  string_of_inferred_expr e1 ^ string_of_inferred_expr e2

let string_of_inferred_prog prog = 
  string_of_inferred_stmts prog ^ "\n"

