{ open Parser }

let fp1 = ['0'-'9']* '.' ['0'-'9']+
let fp2 = ['0'-'9']+ '.' ['0'-'9']*
let exp = 'e' ['+' '-']? ['0'-'9']+

(* code reference from ocaml lex *)
(* http://caml.inria.fr/svn/ocaml/trunk/lex/lexer.mll *)

let back_slash = '\\'
let back_slash_escapes = back_slash ['n' 'r' 't' 'a' '"' '\\']
let string_char = ([^ '"'] | back_slash_escapes)*

rule token = parse
| [' ' '\t'] { token lexbuf }
| ['\n' '\r']+ { NEWLINE }
| "//" { linecomment lexbuf }
| "/*" { blockcomment lexbuf }
| "||" { OR }
| "&&" { AND }
| '!' { NOT }
| '+' { PLUS }
| '*' { MULT }
| '-' { MINUS }
| '/' { DIV }
| '=' { ASSIGN }
| '<' { LT }
| '>' { GT }
| "<=" { LTE }
| ">=" { GTE }
| "==" { EQ }
| "!=" { NE }
| ';' { SEMI }
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '%' { MOD }
| "func" { FUNC }
| "if" { IF }
| "else" { ELSE }
| "for" { FOR }
| "break" { BREAK }
| "return" { RETURN }
| "true" | "false" as tf { BOOL_LIT (bool_of_string tf) }
| fp1 as fp { DOUBLE_LIT (float_of_string fp) }
| fp2 as fp { DOUBLE_LIT (float_of_string fp) }
| fp1 exp as fp { DOUBLE_LIT (float_of_string fp) }
| fp2 exp as fp { DOUBLE_LIT (float_of_string fp) }
| ['0'-'9']+ exp as fp { DOUBLE_LIT (float_of_string fp) }
| ['0'-'9']+ as lxm { INT_LIT (int_of_string lxm) }
| ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']* as id { ID (id) }
| eof { EOF }
| '"' (string_char as content) '"' { STRING (translate_escapes content) }
(* escaped character *)
| '\'' back_slash (_ as esc_char) '\''
    { CHAR_LIT (translate_escape(esc_char)) }
(* unescaped single character *)
| '\'' (_ as char_match) '\'' { CHAR_LIT (char_match) }
| _ as char { raise (Failure("illegal character:[" ^ Char.escaped char ^ "]")) }

and linecomment = parse
| ['\r' '\n'] { NEWLINE }
| _ {linecomment lexbuf}

and blockcomment = parse
| "*/" { token lexbuf }
| _ {blockcomment lexbuf}
