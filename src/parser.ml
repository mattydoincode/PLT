type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | LITERAL of (int)
  | ID of (string)
  | EOF

open Parsing;;
# 1 "parser.mly"
  
# 33 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* EQ *);
  269 (* NEQ *);
  270 (* LT *);
  271 (* LEQ *);
  272 (* GT *);
  273 (* GEQ *);
  274 (* RETURN *);
  275 (* IF *);
  276 (* ELSE *);
  277 (* FOR *);
  278 (* WHILE *);
  279 (* INT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  280 (* LITERAL *);
  281 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\005\000\005\000\002\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\010\000\010\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\011\000\011\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\000\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\002\000\003\000\000\000\
\000\000\011\000\007\000\000\000\000\000\000\000\000\000\009\000\
\008\000\000\000\010\000\000\000\000\000\012\000\004\000\000\000\
\000\000\000\000\000\000\023\000\000\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\016\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\018\000\000\000\000\000\019\000"

let yydgoto = "\002\000\
\003\000\006\000\007\000\012\000\018\000\020\000\013\000\030\000\
\031\000\056\000\059\000\060\000"

let yysindex = "\006\000\
\000\000\000\000\007\255\243\254\017\255\000\000\000\000\043\255\
\020\255\000\000\000\000\046\255\040\255\054\255\048\255\000\000\
\000\000\042\255\000\000\029\255\011\255\000\000\000\000\011\255\
\076\255\077\255\080\255\000\000\255\254\000\000\092\255\253\255\
\038\255\136\255\011\255\011\255\011\255\011\255\011\255\000\000\
\011\255\011\255\011\255\011\255\011\255\011\255\011\255\011\255\
\011\255\011\255\000\000\000\000\000\000\012\000\067\000\084\255\
\027\000\067\000\087\255\091\255\067\000\067\255\067\255\000\000\
\000\000\078\000\078\000\061\255\061\255\061\255\061\255\094\255\
\011\255\094\255\000\000\011\255\083\255\109\255\000\000\067\000\
\094\255\011\255\000\000\108\255\094\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\114\000\000\000\000\000\000\000\000\000\000\000\
\114\255\000\000\000\000\000\000\118\255\000\000\000\000\000\000\
\000\000\062\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\119\255\000\000\000\000\000\000\
\000\000\000\000\000\000\122\255\000\000\121\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\255\000\000\
\000\000\002\255\000\000\127\255\003\255\153\255\170\255\000\000\
\000\000\047\000\060\000\187\255\204\255\221\255\238\255\000\000\
\122\255\000\000\000\000\000\000\070\255\000\000\000\000\035\255\
\000\000\135\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\121\000\000\000\000\000\000\000\118\000\000\000\229\000\
\235\255\185\255\000\000\000\000"

let yytablesize = 351
let yytable = "\032\000\
\038\000\078\000\034\000\035\000\040\000\035\000\001\000\040\000\
\035\000\039\000\084\000\008\000\021\000\054\000\055\000\057\000\
\058\000\061\000\009\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\004\000\021\000\005\000\
\022\000\023\000\028\000\029\000\022\000\041\000\022\000\021\000\
\041\000\022\000\052\000\010\000\011\000\015\000\024\000\025\000\
\014\000\026\000\027\000\055\000\028\000\029\000\080\000\024\000\
\025\000\016\000\026\000\027\000\055\000\028\000\029\000\012\000\
\004\000\012\000\012\000\041\000\042\000\043\000\044\000\017\000\
\017\000\017\000\017\000\043\000\044\000\035\000\036\000\012\000\
\012\000\037\000\012\000\012\000\073\000\012\000\012\000\017\000\
\017\000\075\000\017\000\017\000\040\000\017\000\017\000\021\000\
\076\000\022\000\041\000\042\000\043\000\044\000\081\000\045\000\
\046\000\047\000\048\000\049\000\050\000\082\000\085\000\024\000\
\025\000\042\000\026\000\027\000\005\000\028\000\029\000\024\000\
\006\000\024\000\021\000\038\000\024\000\024\000\024\000\024\000\
\024\000\039\000\024\000\024\000\024\000\024\000\024\000\024\000\
\053\000\021\000\019\000\033\000\000\000\000\000\041\000\042\000\
\043\000\044\000\000\000\045\000\046\000\047\000\048\000\049\000\
\050\000\025\000\000\000\025\000\000\000\000\000\025\000\025\000\
\025\000\000\000\000\000\000\000\025\000\025\000\025\000\025\000\
\025\000\025\000\026\000\000\000\026\000\000\000\000\000\026\000\
\026\000\026\000\000\000\000\000\000\000\026\000\026\000\026\000\
\026\000\026\000\026\000\031\000\000\000\031\000\000\000\000\000\
\031\000\000\000\000\000\000\000\000\000\000\000\031\000\031\000\
\031\000\031\000\031\000\031\000\032\000\000\000\032\000\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\000\000\032\000\
\032\000\032\000\032\000\032\000\032\000\033\000\000\000\033\000\
\000\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\
\033\000\033\000\033\000\033\000\033\000\033\000\034\000\000\000\
\034\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\034\000\034\000\034\000\034\000\034\000\034\000\051\000\
\000\000\000\000\000\000\041\000\042\000\043\000\044\000\000\000\
\045\000\046\000\047\000\048\000\049\000\050\000\072\000\000\000\
\000\000\000\000\041\000\042\000\043\000\044\000\000\000\045\000\
\046\000\047\000\048\000\049\000\050\000\074\000\000\000\000\000\
\000\000\041\000\042\000\043\000\044\000\000\000\045\000\046\000\
\047\000\048\000\049\000\050\000\077\000\000\000\079\000\029\000\
\000\000\029\000\000\000\000\000\029\000\083\000\000\000\000\000\
\000\000\086\000\029\000\029\000\030\000\000\000\030\000\000\000\
\000\000\030\000\000\000\000\000\000\000\000\000\000\000\030\000\
\030\000\041\000\042\000\043\000\044\000\000\000\045\000\046\000\
\047\000\048\000\049\000\050\000\041\000\042\000\043\000\044\000\
\000\000\000\000\000\000\047\000\048\000\049\000\050\000"

let yycheck = "\021\000\
\002\001\073\000\024\000\001\001\003\001\003\001\001\000\006\001\
\006\001\011\001\082\000\025\001\002\001\035\000\036\000\037\000\
\038\000\039\000\002\001\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\023\001\002\001\025\001\
\004\001\005\001\024\001\025\001\001\001\003\001\003\001\002\001\
\006\001\004\001\005\001\001\001\025\001\006\001\018\001\019\001\
\003\001\021\001\022\001\073\000\024\001\025\001\076\000\018\001\
\019\001\004\001\021\001\022\001\082\000\024\001\025\001\002\001\
\023\001\004\001\005\001\007\001\008\001\009\001\010\001\002\001\
\025\001\004\001\005\001\009\001\010\001\002\001\002\001\018\001\
\019\001\002\001\021\001\022\001\001\001\024\001\025\001\018\001\
\019\001\003\001\021\001\022\001\001\001\024\001\025\001\002\001\
\006\001\004\001\007\001\008\001\009\001\010\001\020\001\012\001\
\013\001\014\001\015\001\016\001\017\001\001\001\003\001\018\001\
\019\001\000\000\021\001\022\001\003\001\024\001\025\001\001\001\
\003\001\003\001\001\001\003\001\006\001\007\001\008\001\009\001\
\010\001\003\001\012\001\013\001\014\001\015\001\016\001\017\001\
\001\001\003\001\018\000\022\000\255\255\255\255\007\001\008\001\
\009\001\010\001\255\255\012\001\013\001\014\001\015\001\016\001\
\017\001\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\003\001\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\003\001\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\072\000\255\255\074\000\001\001\
\255\255\003\001\255\255\255\255\006\001\081\000\255\255\255\255\
\255\255\085\000\012\001\013\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\012\001\
\013\001\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
                 ( "" )
# 260 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 26 "parser.mly"
                 ( _2 ^ _1 )
# 268 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 27 "parser.mly"
                 (  _1 ^ _2 )
# 276 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 31 "parser.mly"
     (_1 ^ " " ^ _3 ^ " " ^  _6 ^  _7 )
# 286 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
                  ( " " )
# 292 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 35 "parser.mly"
                  (_1)
# 299 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
                         ( _1 )
# 306 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                         ( _3 ^ ", " ^ _1 )
# 314 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                     ( " " )
# 320 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 43 "parser.mly"
                     ( _2  ^ "/n" ^ _1 )
# 328 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 46 "parser.mly"
               ( _2 )
# 335 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                   ( " " )
# 341 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 50 "parser.mly"
                   ( _2 ^ "\n " ^ _1 )
# 349 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
              ( " " ^ _1 ^ ";\n"  )
# 356 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                     ( "RETURN " ^ _2 )
# 363 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 55 "parser.mly"
                            ( _2 )
# 370 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 56 "parser.mly"
                                            ( "If: " ^ _3 ^ " then do: \n" ^ _5 ^ "\n" )
# 378 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 57 "parser.mly"
                                            ( "If: " ^ _3 ^ " then do: \n" ^ _5 ^ "\n else do: \n" ^ _7)
# 387 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 59 "parser.mly"
     ( "FOR LOOP: " ^ _3 ^ " " ^ _5 ^ " " ^ _7 ^ " " ^ _9 )
# 397 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 60 "parser.mly"
                                  ( "WHILE LOOP " ^ _3 ^ " " ^ _5 )
# 405 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                  ( " " )
# 411 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                  ( _1 )
# 418 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
                     ( "'" ^ (string_of_int _1) ^ "'" )
# 425 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                     ( "_" ^ _1 ^ "_" )
# 432 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                     ( _1 ^ " + " ^ _3 )
# 440 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                     ( _1  ^ " - " ^ _3 )
# 448 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                     ( _1  ^ " * " ^ _3 )
# 456 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                     ( _1  ^ " / " ^ _3  )
# 464 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                     ( _1^ " == " ^ _3 )
# 472 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                     ( _1^ " != " ^ _3 )
# 480 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                     ( _1^ " < " ^ _3 )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                     ( _1^ " <= " ^ _3 )
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                     ( _1^ " > " ^ _3 )
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                     ( _1^ " >= " ^ _3 )
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( _1 ^ "=" ^ _3 )
# 520 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 80 "parser.mly"
                                 ( "call " ^ _1 ^ "with: " ^ _3 )
# 528 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                       ( "(" ^ _2 ^ ")" )
# 535 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                  ( " " )
# 541 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 85 "parser.mly"
                  ( _1 )
# 548 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                            (_1)
# 555 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                            ( _3 ^ ", " ^ _1 )
# 563 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string)
