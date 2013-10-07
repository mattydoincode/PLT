(*
 * Common components between the front end and the back end.
 * These include the parts of the AST.
 *)

type unop = IntNeg | BitNot | Not | Positive

type binop = Mult | Div | Mod | Add | Sub | LSh | RSh | LeT | GrT | LE |
	     GE | Eq | NEq | BAnd | BXor | BOr | And | Or

(* nameless header *)
type func_dec_header = { ret_type : vartype option ; params : var list }
(* variable types *)
and single_vartype =
  | FunkInt
  | FunkDouble
  | FunkChar
  | FunkBool
  (* function instance's value not calculated statically *)
  | FunkFunc of func_dec_header
and vartype =
  (*
   * type, size of each level. If integer list is empty, the type is just
   * single
   *)
  single_vartype * (expr list)
(* var is used only for the formal parameters of a function declaration
   notice how a var_dec has a list of id opposed to a single id
*)
and var = { id : string; bare_type : vartype option }
and single_funk_value =
  | IntVal of int
  (* this is what OCaml calls double, according to
   *http://blog.frama-c.com/index.php?post/2010/11/20/IEEE-754-single-precision-
   *numbers-in-Frama-C
   *)
  | DoubleVal of float
  | CharVal of char
  | BoolVal of bool
  (* function instance's value not calculated statically *)
  | FuncVal of func_dec_header * (statement list)
and func_call = expr * (rvalue list)

and expr =
  | SingleConst of single_funk_value
  | ArrayLit of vartype * rvalue list
  | Variable of var
  | FunkUnExpr of unop * expr
  | FunkBinExpr of expr * binop * expr
  | FunkCallExpr of func_call
  | FunkArrExpr of expr * expr
  | FunkAsyncExpr of (statement list) (* async block *)

and var_dec = { id_list: string list ; var_type: vartype ; actual_list: rvalue list}

(* notice this is the same as funcdec without a fid *)
and anon = func_dec_header * (statement list)

and rvalue =
  | ExprRVal of expr (* simple expression *)
  | FuncRVal of anon (* anonymous function declaration *)

and statement =
  | Assignment of (expr list) * (rvalue list)
  | Declaration of var_dec
  (* the state of the function may change *)
  | FunctionCall of func_call
  | Block of statement list
  | ForBlock of (statement option) * (expr option) * (statement option) *
    (statement list)
  | IfBlock of expr * (statement list)
  | IfElseBlock of expr * (statement list) * (statement list)
  | WhileBlock of expr * (statement list)
  | Break
  | Return of rvalue option



type funcdec = { fid: string; func_header: func_dec_header ; body: statement list}

type declaration =
  | Vardec of var_dec  
  | Funcdec of funcdec
  | Newline of unit

type program = declaration list 
