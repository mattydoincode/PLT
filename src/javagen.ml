open Sast
open Printf

(*import PCObject;
  import PCList;  killed these guys for now...*)


(************
HELPERS
************)

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


(*******************************************************************************
  Expression and statement evaluation
********************************************************************************)

let rec writeToFile fileName progString = 
  let file = open_out ("bin/" ^ fileName ^ ".java") in
    fprintf file "%s"  progString

and gen_program fileName prog = (*have a writetofile*)
  let stmtString = writeStmtList prog in
  let out = sprintf "

  public class %s{
      public static void main(String[] args){
      %s
      } 
  } " fileName stmtString in
  writeToFile fileName out;
  out


and writeStmtList stmtList = 
  let outStr = List.fold_left (fun a b-> a^(gen_stmt b)) "" stmtList in
  sprintf "%s" outStr


and gen_stmt = function
    AReturn(exp, _) -> writeReturnStmt exp
  | AIf(condTupleList, elseStmt) -> writeIfStmt condTupleList elseStmt
  | AFor(asnTuple, cond, incrTuple, stmtList) -> 
      writeForLoop asnTuple cond incrTuple stmtList
  | AWhile(cond, stmtList) -> writeWhileLoop cond stmtList
  | AAssign((expr1 , expr2)) -> writeAssign expr1 expr2
  | AFuncCallStmt(funcNameExpr, paramsListExpr) -> 
      writeFuncCallStmt funcNameExpr paramsListExpr



and gen_expr = function
    ANumLit(flt, _)   -> writeNumLit flt
  | ABoolLit(boolLit, _) -> writeBoolLit boolLit
  | ACharLit(charLit, _) -> writeCharLit charLit
  | AId(name, typed, typ) -> writeID  name typed 
  | AFuncCreate(params, body, _) -> writeFunc params body
  | AFuncCallExpr(exp, params, _) -> writeFuncCall  exp params
  | AObjAccess(objName, fieldName, _)-> writeObjectAccess objName fieldName
  | AListAccess(listName, idx, _) -> writeListAccess listName idx
  | AListCreate(contentList,_) -> writeListCreate contentList
  | ASublist(listName, leftIdx, rightIdx, _) -> 
      writeSubList listName leftIdx rightIdx
  | AObjCreate(nVTplList, _) ->  writeObjCreate nVTplList
  | ABinop(ope1, op, ope2, _) -> writeBinop ope1 op ope2
  | ANot(exp, _) -> writeUnaryNot exp



(*******************************************************************************
  Specific Statement evaluation
********************************************************************************)

and writeReturnStmt exp = 
  let expStr = (gen_expr exp) in
    sprintf "return %s;\n" expStr

and writeIfStmt condTupleList elseStmtList = 
  let string_of_tuple (condExpr, stmtList) =
    let body = writeStmtList stmtList
    and cond = gen_expr condExpr in
    sprintf "if(%s){
    %s\n}" cond  body
  in let ifString =
    let rec string_of_tupleList = function
        [] -> ""
      | a::[] -> string_of_tuple a ^"\n"
      | a::tl -> (string_of_tuple a) ^ " else " ^ string_of_tupleList tl
    in string_of_tupleList condTupleList 
  and elseString = 
    let checkForNone str = match str with 
        Some(str) -> "else{\n" ^ writeStmtList str ^ "\n}"
      | None -> ""
    in checkForNone elseStmtList 
in sprintf "%s\n%s\n" ifString elseString

and writeForLoop asnTuple cond incrTuple stmtList =
  let asn = 
    let matchTuple = function
        Some(asnTup) ->   gen_expr (fst asnTup) ^ "=" ^ gen_expr (snd asnTup)
      | None -> ""
    in matchTuple asnTuple
  and stmtString = writeStmtList stmtList  
  and cond = 
    let matchCond = function
        Some(cond) -> gen_expr cond
      | None -> ""
    in matchCond cond 
  and incrString = 
    let matchTuple = function
        Some(incrTup) ->   gen_expr (fst incrTup) ^ "=" ^ gen_expr (snd incrTup)
      | None -> ""
    in matchTuple incrTuple
  in
  sprintf "for(%s;%s;%s){\n%s\n}\n" asn cond incrString stmtString

and writeWhileLoop cond stmtList =
  let condString = gen_expr cond 
  and stmtString = writeStmtList stmtList in 
    sprintf "while(%s)\n{\n    %s\n}" condString stmtString


(*ASSIGNING IS SPECIAL SO WE HANDWROTE THESE WITH LOVE*)
and writeAssign expr1 expr2 =
    let lhs_type = match type_of expr2 with
      | TFunc(a,b) -> "IPCFunction" 
      | _ ->  "PCObject"
    in
    let e2string = gen_expr expr2 in
      match expr1 with
        | AId(name, typed, typ) -> 
          let typeString = if typed then lhs_type else "" in
          let e1string = typeString ^ " " ^ name in
          sprintf "%s = %s;\n" e1string e2string
        | AListAccess(listName, idx, _) -> 
          let listNamestring = gen_expr listName and idxstring = gen_expr idx in
          sprintf "%s.set(%s,%s);\n" listNamestring idxstring e2string
        | AObjAccess(objName, fieldName, _)->
          let objNamestring = gen_expr objName in 
          sprintf "%s.set(\"%s\", %s)" objNamestring fieldName e2string

        | _ -> failwith "How'd we get all the way to java with this!!!! Not a valid LHS"
    

and writeFuncCallStmt fNameExpr paramsListExpr = 
  let fName = gen_expr fNameExpr
  and params = params_to_string paramsListExpr in
  sprintf "%s.call(%s);\n" fName params



(*******************************************************************************  
    Function handling - helper functions
********************************************************************************)

and writeFunc params stmtList = 
  let fName = function_name_gen() in
  let fileName = "bin/" ^ fName ^ ".java" in 
   let file = open_out fileName in
    let paramSetting = snd (List.fold_left (
                            fun a p -> 
                              let count = fst a in
                              let sofar = snd a in 
                              let newString = "PCObject " ^ (fst p) ^
                              " = args[" ^ string_of_int count ^ "];\n"
                              in
                              (count +1, sofar ^ newString)
                          ) (0, "") params)

    and body = writeStmtList stmtList in
      fprintf file "import java.io.Serializable; 
      public class %s extends PCObject implements IPCFunction, Serializable{
  public %s(){}

  public PCObject call(PCObject... args){
  %s
  %s\n}
  }" fName fName paramSetting body;
  sprintf "new %s()" fName


  
and writeFuncCall toCallExp paramsExp =
  let toCall = (gen_expr toCallExp) and params = (params_to_string paramsExp) in 
  sprintf "%s.call(%s)" toCall params


and params_to_string paramsList= 
  let paramsStringList = List.map gen_expr paramsList in
    let rec paramsListtoString = function
        [] -> ""
      | [a] -> sprintf("%s") a 
      | hd::tl -> (sprintf("%s,") hd)^paramsListtoString tl
    in paramsListtoString paramsStringList 

and pNames_to_string paramTupleList = 
  let pNameList = List.map (fun (a,b) -> a) paramTupleList 
in List.fold_left (fun a b -> a^b) "" pNameList


  

(*******************************************************************************  
    List and Object handling - helper functions
********************************************************************************)


and writeObjectAccess objNameExpr fieldName =
  let objName = gen_expr objNameExpr in 
    sprintf "%s.get(\"%s\")" objName fieldName

and writeListAccess listNameExpr idxExpr =
  let listName = gen_expr listNameExpr and idx = gen_expr idxExpr in
  sprintf "%s.get(%s)" listName idx


and writeListCreate exprList =
  let concatAdds = (fun a b -> a^(sprintf(".add(%s)") b)) 
  and list_of_strings = List.map gen_expr exprList in 
  List.fold_left concatAdds "new PCList()" list_of_strings

and writeSubList listNameExpr startIdxExpr endIdxExpr = 
  let listName = gen_expr listNameExpr in  
  let startIdx = 
    let det = function
        Some(x) -> gen_expr x
      | None -> "0"
    in det startIdxExpr 
  and endIdx = 
    let det = function
        Some(x) -> gen_expr x
      | None -> sprintf "%s.size()" listName 
    in det endIdxExpr 
  in sprintf "%s.sublist(%s,%s)" listName startIdx endIdx

and writeObjCreate kvt_list = 
  let string_of_tuple (k , vExpr)  = 
    let v = gen_expr vExpr in
    sprintf "\n.set(\"%s\",%s)" k v
  in let stringList = List.map string_of_tuple kvt_list; in 
    List.fold_left (fun a b -> a^b) "new PCObject()" stringList



(*******************************************************************************  
    Binop and Not Handling - helper functions
********************************************************************************)

and writeUnaryNot boolExpr = 
  let boolObj = gen_expr boolExpr in
  sprintf "!%s" boolObj

and writeBinop expr1 op expr2 = 
  let e1 = gen_expr expr1 and e2 = gen_expr expr2 in
    let writeBinopHelper e1 op e2 = match op with
        Add  -> sprintf "new PCObject(%s.<Double>getBase() + %s.<Double>getBase())" e1 e2
      | Sub  -> sprintf "new PCObject(%s.<Double>getBase() - %s.<Double>getBase())" e1 e2  
      | Mult -> sprintf "new PCObject(%s.<Double>getBase() * %s.<Double>getBase())" e1 e2
      | Div  -> sprintf "new PCObject(%s.<Double>getBase() / %s.<Double>getBase())" e1 e2
      | Mod -> sprintf "new PCObject(%s.<Double>getBase() %% %s.<Double>getBase())" e1 e2
      | Less -> sprintf "new PCObject(%s.<Double>getBase() < %s.<Double>getBase())" e1 e2
      | Leq -> sprintf "new PCObject(%s.<Double>getBase() <= %s.<Double>getBase())" e1 e2   
      | Greater -> sprintf "new PCObject(%s.<Double>getBase() > %s.<Double>getBase())" e1 e2 
      | Geq -> sprintf "new PCObject(%s.<Double>getBase() >= %s.<Double>getBase())" e1 e2
      | And -> sprintf "new PCObject(%s.<Boolean>getBase() && %s.<Boolean>getBase())" e1 e2    
      | Or -> sprintf "new PCObject(%s.<Boolean>getBase() ||h %s.<Boolean>getBase())" e1 e2 
      | Equal -> "new PCObject(%s.equals(%s))" (*PATTERN MATCH ON TYPE*)
      | Neq -> "new PCObject(%s != %s)" 
      | Concat -> "new PCList(\"TODO GET THIS WORKING\")"
    in writeBinopHelper e1 op e2


(*******************************************************************************  
    Id handling - helper function
********************************************************************************)

and writeID idName = function
    true -> sprintf "PCObject %s" idName
  | false -> sprintf "%s" idName


(*******************************************************************************  
    Literal expression handling - helper functions
********************************************************************************)
and writeNumLit numLit = 
  sprintf "new PCObject(%f)" numLit

and writeBoolLit boolLit = 
  sprintf "new PCObject(%b)" boolLit


and writeCharLit charLit = 
  sprintf "new PCObject('%c')" charLit


(*******************************************************************************  
    Function Name Generation - helper functions
********************************************************************************)
(*function name generator*)
and function_name_gen () = 
  incr x;
  sprintf "function_%d" !x

(*"static" variables for function naming*)
and init = 100
and x = ref init