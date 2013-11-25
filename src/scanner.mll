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
| "->"                 { ARROW }
| ','                  { COMMA }
| '.'                  { ACCESS }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '*'                  { TIMES }
| '/'                  { DIVIDE }
| '%'                  { MOD }
| '='                  { ASSIGN }
| '^'                  { CONCAT }
| "=="                 { EQ }
| "!="                 { NEQ }
| '<'                  { LT }
| "<="                 { LEQ }
| ">"                  { GT }
| ">="                 { GEQ }
| "&&"                 { AND }
| "||"                 { OR }
| '!'                  { NOT }
| "if"                 { IF }
| "elif"               { ELIF }
| "else"               { ELSE }
| "for"                { FOR }
| "while"              { WHILE }
| "return"             { RETURN }
| "true"               { BOOLEAN_LIT(true) }
| "false"              { BOOLEAN_LIT(false) }
| ('\'' ([' '-'&' '('-'[' ']'-'~'] as c) '\'')
  (* from www.asciitable.com, all chars except ' and \
     NEED TO HANDLE 5 THINGS: \', \\, \t, \n, \r *)
					   { CHAR_LIT(c) }
| ('0' | ['1'-'9']+['0'-'9']*)(['.']['0'-'9']+)? as lxm 
					   { NUM_LIT(float_of_string lxm) }
| '"' ([^'"']* as s) '"' 
					   { STRING_LIT(s) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm 
					   { ID(lxm) }
| eof                  { EOF }
| _ as char            { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and singlelinecom = parse
  "\n" { token lexbuf }
| eof  { EOF }
| _	   { singlelinecom lexbuf}
