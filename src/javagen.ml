open Sast
 
open Printf 

let def_imports = ["core.PCObject";"core.PCList";"distribute.distributeServer"]


(*******************************************************************************
  Java boilerplate
 ********************************************************************************)
let write_imports fileName = 
  let print_each import = sprintf fileName "import %s;\n" import 
  in List.iter print_each def_imports

let write_main fileName = 
  sprintf fileName "public static void main(String[] args){"
  (*Note: must close this at the end *)



(*******************************************************************************  
    Literal expression handling - helper functions
********************************************************************************)
let writeNumLit file_desc numLit = 
  sprintf file_desc "new PCObject(%f).getbase()" numLit

let writeBoolLit file_desc boolLit = 
  sprintf file_desc "new PCObject(%b)" boolLit


let writeStringLit file_desc stringLit = 
  sprintf file_desc "new PCList(%s)" stringLit

(*******************************************************************************  
    Id handling - helper function
********************************************************************************)

let writeID fileName idName typed typ = match (typed, typ) with 
    (true, TVar) | (true, TBool)  -> sprintf fileName "new PCObject %s" idName
  | (true, TChar) | true,TFunc(_,_) -> sprintf fileName "new PCList %s" idName
  | (true, TFunc(_,_)) (*return to this *)


(*******************************************************************************  
    Function handling - helper functions
********************************************************************************)
(*"static" variables for function naming*)
let init = 100
let x = ref init

(*function name generator*)
let function_name_gen () = 
  incr x
  sprintf "function_%d" !x;;
  
let writeFunctionCall toCallExp paramsExp typ =
  let toCall = (gen_expr toCallExp) and params = (params_to_string paramsExp) in 
  sprintf fileName "%s.call(%s)" toCall params;;

let params_to_string paramsList= 
  let paramsStringList = List.map gen_expr paramsList in
    let rec paramsListtoString = function
        [] -> ""
      | [a] -> sprintf("%s") a 
      | hd::tl -> (sprintf("%s,") hd)^paramsListtoString tl
    in paramsListtoString paramsStringList ;;


(*******************************************************************************  
    List and Object handling - helper functions
********************************************************************************)


let writeObjectAccess objNameExpr fieldName =
  let objName = gen_expr objName in 
    sprintf fileName "%s.get(%s)" objName fieldName;;

let writeListAccess listNameExpr idxExpr =
  let listName = gen_expr listNameExpr and idx = gen_expr id_expr in
  sprintf fileName "%s.get(%s)";;

let writeListCreate exprList =
  let concatAdds = (fun a b -> a^(sprintf(".add(%s)") b)) 
  and list_of_strings = List.map gen_expr exprList in 
  List.fold_left concatAdds "New PCList()" list_of_strings;;

let writeSubList listNameExpr startIdxExpr endIdxExpr = 
  let listName = gen_expr listName in  
  let startIdx = 
    let det = function
        Some(x) -> gen_expr x
      | None -> "0"
    in det startIdxExpr and 
  let endIdx = 
    let det = function
        Some(x) -> gen_expr x
      | None -> sprintf "%s.size()" listName 
    in det endIdxExpr 
  in sprintf "%s.sublist(%s,%s)" listName startIdx endIdx;;

let writeObjCreate kvt_list = 
  let string_of_tuple (k , vExpr)  = 
    let v = gen_expr vExpr in
    sprintf ".set(\"%s\",%s)" k v
  in let stringList = List.map string_of_tuple kvt_list; in 
    List.fold_left (fun a b -> a^b) "New PCObject()" stringList;;

(*******************************************************************************  
    Binop and Not Handling - helper functions
********************************************************************************)

let writeUnaryNot boolExpr = 
  let boolObj = gen_expr boolExpr in
  sprintf "!%s" boolObj;;

let writeBinop expr1 op expr2 = 
  let e1 = gen_expr expr1 and e2 = gen_expr expr2 in
    let writeBinopHelper e1 op e2 = match op with
        Add  -> sprintf "New PCObject(%s.getBase() + %s.getBase())" e1 e2
      | Sub  -> sprintf "New PCObject(%s.getBase() - %s.getBase())" e1 e2  
      | Mult -> sprintf "New PCObject(%s.getBase() * %s.getBase())" e1 e2
      | Div  -> sprintf "New PCObject(%s.getBase() / %s.getBase())" e1 e2
      | Mod -> sprintf "New PCObject(%s.getBase() %% %s.getBase())" e1 e2
      | Less -> sprintf "New PCObject(%s.getBase() < %s.getBase())" e1 e2
      | Leq -> sprintf "New PCObject(%s.getBase() <= %s.getBase())" e1 e2   
      | Greater -> sprintf "New PCObject(%s.getBase() > %s.getBase())" e1 e2 
      | Geq -> sprintf "New PCObject(%s.getBase() >= %s.getBase())" e1 e2
      | And -> sprintf "New PCObject(%s.getBase() && %s.getBase())" e1 e2    
      | Or -> sprintf "New PCObject(%s.getBase() ||h %s.getBase())" e1 e2 
      | Equal ->
      | Neq -> 
      | Concat -> 

(*******************************************************************************
  Specific Statement evaluation
********************************************************************************)

let writeReturnStmt exp = 
  let expStr = gen_expr exp in
  sprintf "return %s;" expStr

let writeIfStmt 


(*******************************************************************************
  Expression and statement evaluation
********************************************************************************)

let gen_expr = function
    ANumLit(flt, typ)   -> writeNumLit flt
  | ABoolLit(boolLit, typ) -> writeBoolLit boolLit
  | ACharLit(charLit, typ) -> (*Check with Alden about this*)
  | AId(name, typed, typ) -> writeID  name typed typ
  | AFuncCreate(params, body) -> writeFunc
  | AFuncCall(exp, params, typ) -> writeFuncCall  exp params typ
  | AObjAccess(objName, fieldName, typ)-> writeObjectAccess ob
  | AListAccess(listName, idx, typ) -> writeListAccess listName idx
  | AListCreate(contentList,typ) -> writeListCreate contentList
  | ASublist(listName, Some(leftIdx), Some(rightIdx), typ) -> 
      writeSubList listName (some leftIdx) (Some rightIdx)
  | AObjCreate(nVTplList, typ) ->  writeObjCreate nVTplList
  | ABinop(ope1, op, ope2, typ) -> writeBinop ope1 op ope2
  | ANot(exp, typ) -> writeUnaryNot exp

let gen_stmt = function
    Return(exp) -> writeReturnStmt exp
  | If(condList, stmtList) -> writeIfStmt condList stmtList
  | For(Some(asnTuple), Some(cond), Some(incrTuple), stmtList) -> 
      writeForLoop (Some asnTuple) (Some cond) (Some incrTuple) stmtList
  | While(cond, stmtList) -> writeWhileLoop cond stmtList
  | Assign((expr1 , expr2)) -> writeAssign expr1 expr2
  | FuncCallStmt(funcNameExpr, paramsListExpr) -> 
      writeFuncCallStmt funcNameExpr paramsListExpr

let gen_program prog= 
