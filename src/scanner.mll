{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }   (* Whitespace *)
| "/*"                 { comment lexbuf } (* Comments *)
| "//"                 { singlelinecom lexbuf}
| '('                  { LPAREN }
| ')'                  { RPAREN }
| '['                  { LBRACK }
| ']'                  { RBRACK }
| '{'                  { LBRACE }
| '}'                  { RBRACE }
| ';'                  { SEMI }
| ':'                  { COLON }
| "=>"                 { ARROW }
| ','                  { COMMA }
| '.'                  { ACCESS }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '*'                  { TIMES }
| '/'                  { DIVIDE }
| '='                  { ASSIGN }
| '^'                  { CONCAT }
| "=="                 { EQ }
| "!="                 { NEQ }
| '<'                  { LT }
| "<="                 { LEQ }
| ">"                  { GT }
| ">="                 { GEQ }
| "if"                 { IF }
| "elif"               { ELIF }
| "else"               { ELSE }
| "for"                { FOR }
| "while"              { WHILE }
| "return"             { RETURN }
| ['0'-'9']+ as lxm    { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof                  { EOF }
| _ as char            { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and singlelinecom = parse
  "\n" { token lexbuf }
| _	   { singlelinecom lexbuf}
