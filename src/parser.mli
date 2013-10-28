type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | SEMI
  | COMMA
  | ASSIGN
  | COLON
  | ARROW
  | CONCAT
  | ACCESS
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | NOT
  | RETURN
  | IF
  | ELIF
  | ELSE
  | FOR
  | WHILE
  | ID of (string)
  | NUM_LIT of (float)
  | BOOLEAN_LIT of (bool)
  | STRING_LIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
