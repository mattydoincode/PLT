%{  %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA ASSIGN COLON ARROW CONCAT ACCESS
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELIF ELSE FOR WHILE
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc IF
%nonassoc ELSE
%nonassoc ELIF
%nonassoc ASSIGN COLON
%left CONCAT
%left EQ NEQ 
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc LBRACE RBRACE
%nonassoc LPAREN RPAREN
%nonassoc LBRACK RBRACK
%left ACCESS

%start program
%type <string> program

%%

program:
  stmt_list { $1 }

stmt_list:
   /* nothing */    { "" }
 | stmt_list stmt   { $1 ^ " " ^ $2 }

stmt:
    assignment SEMI  { " " ^ $1 ^ ";\n"  }
  | RETURN expr SEMI { "RETURN " ^ $2 }

assignment:
  ID ASSIGN expr { $1 ^ " = " ^ $3 }

expr:
    LITERAL            { "'" ^ (string_of_int $1) ^ "'" }
  | ID                 { $1 }
  | func_create        { $1 }
  | LPAREN expr RPAREN { "(" ^ $2 ^ ")" }
  | expr CONCAT expr   { $1 ^ " ^ "  ^ $3 }
  | expr PLUS   expr   { $1 ^ " + "  ^ $3 }
  | expr MINUS  expr   { $1 ^ " - "  ^ $3 }
  | expr TIMES  expr   { $1 ^ " * "  ^ $3 }
  | expr DIVIDE expr   { $1 ^ " / "  ^ $3 }
  | expr EQ     expr   { $1 ^ " == " ^ $3 }
  | expr NEQ    expr   { $1 ^ " != " ^ $3 }
  | expr LT     expr   { $1 ^ " < "  ^ $3 }
  | expr LEQ    expr   { $1 ^ " <= " ^ $3 }
  | expr GT     expr   { $1 ^ " > "  ^ $3 }
  | expr GEQ    expr   { $1 ^ " >= " ^ $3 }

/* 
  function can be:
  1. () -> body
  2. x -> body
  4. (x,y,z) -> body 
*/
func_create:
    LPAREN RPAREN ARROW func_body               { "() -> " ^ $4 }
  | ID ARROW func_body                          { $1 ^ " -> " ^ $3 }
  | LPAREN mult_formals RPAREN ARROW func_body { "(" ^ $2 ^ ") -> " ^ $5 }

func_body:
    LBRACE stmt_list RBRACE { " {\n" ^ $2 ^ "\n}\n" }
  | expr SEMI               { $1 ^ ";\n" }

mult_formals:
  formal_list COMMA ID { $3 ^ ", " ^ $1 }

formal_list:
    ID                   { $1 }
  | formal_list COMMA ID { $3 ^ ", " ^ $1 }
