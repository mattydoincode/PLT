(* Abstract Syntax Tree
    - created by parser
    - consumed by semantic analyzer
*)

type op = 
    Add | Sub | Mult | Div | Mod
  | Equal | Neq | Less | Leq | Greater | Geq
  | Concat | And | Or

type expr =
    NumLit of float
  | BoolLit of bool
  | CharLit of char
  | Id of string
  | FuncCreate of string list * stmt list
  | FuncCallExpr of expr * expr list
  | ObjAccess of expr * string
  | ListAccess of expr * expr
  | ListCreate of expr list
  | Sublist of expr * expr option * expr option
  | ObjCreate of (string * expr) list  
  | Binop of expr * op * expr
  | Not of expr 

and stmt =
    Return of expr
  | If of conditional list * stmt list option
  | For of (expr * expr) option * expr option * (expr * expr) option * stmt list
  | While of expr * stmt list 
  | Assign of (expr * expr)
  | FuncCallStmt of expr * expr list

and conditional = {
  condition : expr;
  body : stmt list;
}

type program = stmt list



let string_of_opt string_of = function 
    Some(x) -> string_of x 
  | None -> ""

let rec string_of_expr = function
    NumLit(n) -> string_of_float n
  | BoolLit(b) -> string_of_bool b
  | CharLit(c) -> "'" ^ Char.escaped c ^ "'"
  | Id(s) -> s
  | Not(e) ->  "!" ^ string_of_expr e
  | Binop(e1, op, e2) ->
      string_of_expr e1 ^ (match op with
        Add -> " + "    | Sub -> " - "     | Mult -> " * " 
      | Div -> " / "    | Mod -> " % "
      | Equal -> " == " | Neq -> " != "    | Less -> " < "
      | Leq -> " <= "   | Greater -> " > " | Geq -> " >= "
      | Concat -> " ^ " | And -> " && "    | Or -> " || ") ^
      string_of_expr e2
  | FuncCallExpr(e, el) -> 
      string_of_expr e ^ "(" ^ 
      String.concat ", " (List.map string_of_expr el) ^ ")"
  | FuncCreate(formals, body) ->
      "(" ^ String.concat ", " formals ^ ") -> {\n" ^
      String.concat "" (List.map string_of_stmt body) ^ "\n}"
  | ListCreate(exprs) ->
      "[" ^ String.concat ", " (List.map string_of_expr exprs) ^ "]"
  | Sublist(e, eleft, eright) -> 
      string_of_expr e ^ "[" ^ 
      string_of_opt string_of_expr eleft ^ ":" ^ 
      string_of_opt string_of_expr eright ^ "]"
  | ListAccess(e1, e2) -> 
      string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]" 
  | ObjCreate(props) ->
      "{\n" ^ String.concat ",\n" (List.map 
          (fun(prop) -> fst prop ^ ": " ^ string_of_expr (snd prop)) 
      props) ^ "\n}"
  | ObjAccess(e, s) ->
      string_of_expr e ^ "." ^ s

and string_of_stmt = function
    Return(expr) -> "return " ^ string_of_expr expr ^ ";";
  | If(conds, elsebody) -> 
      "if" ^ string_of_cond (List.hd conds) ^ String.concat "" 
        (List.map (fun(x) -> "\nelif" ^ string_of_cond x) (List.tl conds)) ^
        string_of_opt (fun(x) -> "\nelse {\n" ^ string_of_stmts x ^ "}") elsebody
  | For(a1, e, a2, s) ->
      "for (" ^ string_of_opt string_of_assign a1 ^ "; " ^ 
                string_of_opt string_of_expr e ^ "; " ^
                string_of_opt string_of_assign a2 ^ ") {\n" ^ 
      string_of_stmts s ^ "}"
  | While(e, s) -> 
      "while (" ^ string_of_expr e ^ ") {\n" ^ 
      string_of_stmts s ^ "}"
  | Assign(a) -> string_of_assign a ^ ";"
  | FuncCallStmt(e, el) ->
      string_of_expr e ^ "(" ^ 
      String.concat ", " (List.map string_of_expr el) ^ ");"

and string_of_stmts stmts = 
  String.concat "\n" (List.map string_of_stmt stmts) ^ "\n"

and string_of_cond cond =
  " (" ^ string_of_expr cond.condition ^ ") {\n" ^
  string_of_stmts cond.body ^ "}"

and string_of_assign ((e1, e2)) = 
  string_of_expr e1 ^ " = " ^ string_of_expr e2

let string_of_program prog = 
  string_of_stmts prog
