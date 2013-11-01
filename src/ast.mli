
(* Abstract Syntax Tree
    - created by parser
    - consumed by semantic analyzer
*)

type op = Concat | Mod | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    Num_Lit of int
  | Bool_Lit of bool
  | String_Lit of string 
  | Id of string
  | Func_Create of func_create
  | Func_Call of expr * expr list
  | Access of Access
  | List_Create of expr list
  | Sublist of expr * expr option * expr option (*In the case of slicing*)
  | Object_Create of (string * expr) list  
  | Binop of expr * op * expr
  | Not of expr  
 

type func_create = {
    formals : string list;
    body : stmt list;
}


 
 type assign = {

 }   

type stmt =
  | Return of expr
  | If of expr * stmt list * else list  
  | For of assign option * expr * assign option * stmt list
  | While of expr * stmt list 

  type else = {
      condition : expr;
      body : stmt list;
  }

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

type program = string list * func_decl list






























let rec string_of_expr = function
    Num_Lit(n) -> string_of_float n
  | Bool_Lit(b) -> string_of_bool b
  | String_Lit(s) -> "'" ^ s ^ "'" 
  | Id(s) -> s
  | Not(e) ->  "!" ^ string_of_expr e
  | Binop(e1, op, e2) ->
      string_of_expr e1 ^
      (match op with
        Add -> " + "    | Sub -> " - "     | Mult -> " * " 
      | Div -> " / "    | Mod -> " % "
      | Equal -> " == " | Neq -> " != "    | Less -> " < "
      | Leq -> " <= "   | Greater -> " > " | Geq -> " >= "
      | Concat -> " ^ " | And -> " && "    | Or -> " || ") ^
      string_of_expr e2
  | Func_Call(e, el) -> 
      string_of_expr e ^ 
      "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Func_Create(f) ->
      "(" ^ String.concat ", " f.formals ^ ") -> {\n" ^
      String.concat "" (List.map string_of_stmt f.body) ^ "}\n"
  | List_Create(exprs) ->
      "[" ^ String.concat ", " ^ (List.map string_of_expr exprs) ^ "]"
  | Sublist(e, eleft, eright) -> 
      string_of_expr e ^ "[" ^ 
      (match eleft  with Some(x) -> string_of_expr x | None -> "") ^ ":" ^ 
      (match eright with Some(x) -> string_of_expr x | None -> "") ^ "]"
  | Object_Create(props) ->
      "{\n" ^ String.concat ",\n" (List.map 
          (fun(prop) -> fst prop ^ ": " ^ string_of_expr (snd prop)) props) ^ 
      "}\n"
  | Access(e1, e2) -> 
      string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]" 



let rec string_of_stmt = function
    Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program stmts =
  String.concat "\n" (List.map string_of_stmt stmts)
