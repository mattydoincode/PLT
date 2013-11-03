
(* Semantic Tree
	- created by semantic analyzer
	- consumed by java converter
*)

type stNumOp = 
    Add | Sub | Mult | Div | Mod

type stBoolMathOp = 
	Equal | Neq | Less | Leq | Greater | Geq

type stCharOp = 
	Equal | Neq
  
type stBoolBoolOp =
	And | Or

type stListOp = 
	Concat 

type stNum = 
	NumLit of float
  | NumOp of stNum * stNumOp * stNum
  | NumFuncCallExpr of expr * expr list
  | NumAccess of access
  | NumVariable of string

type stBool = 
	BoolList of bool,
  | BoolMathOp of stNum stBoolMathOp stNum
  | BoolBoolOp of stBool stBoolBoolOp stBool
  | BoolCharOp of char stCharOp char
  | BoolNot of stBool
  | BoolVariable of string


(*List create gets encompassed here*)
type stList = 
    NumList of stNum list
  | BoolList of stBool list
  | ListList of stList list
  | ObjectList of stObject list
  | StringList of char list  (*do we want to split into characters now?? *) 
  | FuncList of stFunc list
  | StringLit of string
  | Sublist of stList * stNum option * stNum option
  | ListVariable of string

type stObject = 
	ObjectCreate of (string * stExpr) list
  | ObjectVariable of string

type stFunc = 
	FuncCreate of string list * stStmt list
  | FuncVariable of string

type stAccess = 
	ListAccess of stList * stNum
  | ObjectAccess of stObject * stList (*TODO HOW TO WE BE MORE SPECIFIC!?*)
  | ObjectAccess of stObject * string (*Property access?*)

type stExpr =
  | NumExpr of stNum,
  | BoolExpr of stBool,
  | ListExpr of stList,
  | ObjectExpr of stObject,
  | FuncExpr of stFunc
  | FuncCallExpr of stFunc * stExpr list
  | Access of stAccess
  
 type stAssign = 
 	NumAssign of string * stNum
 	BoolAssign of string * stBool
 	ListAssign of string * stList
 	ObjectAssign of string * stObject
 	FuncAssign of string * stFunc

and stStmt =
    Return of stExpr
  | If of stConditional list * stStmt list option (*why is this here?.. i'm sure quimbs has a reason though*)
  | For of stAssign option * stBool option * stAssign option * stStmt list
  | While of stBool * stStmt list 
  | Assign of stAssign
  | FuncCallStmt of stFunc * stExpr list

and stConditional = {
  condition : stBool;
  body : stStmt list;
}

type program = stStmt list


(*
NOTES ON SYMBOL MAPPING TABLE
1) Mapping table can't be passed between stages because it's dynamic... 
in other words it gets created on the fly as you walk through the tree
2) rules for the mapping table, however, can be decided on
3) RULES:
	a) Each time you enter function, all variables get an underscore appended
		if you're two functions deep, two underscores appended, this is our scoping
	b) each time you assign, you simply overwrite any existing variable with
	    the same name. This should work if we walk the tree in the correct direction
	c) each entry will have a type and a value associated with it, that's determined on 
	    assignment and doesn't change until another assignment



*)