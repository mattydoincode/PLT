%{  %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <string> program

%%

program:
   /* nothing */ { "" }
 | program vdecl { $2 ^ $1 }
 | program fdecl {  $1 ^ $2 }

fdecl:
   ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     {$1 ^ " " ^ $3 ^ " " ^  $6 ^  $7 }

formals_opt:
    /* nothing */ { " " }
  | formal_list   {$1}

formal_list:
    ID                   { $1 }
  | formal_list COMMA ID { $3 ^ ", " ^ $1 }

vdecl_list:
    /* nothing */    { " " }
  | vdecl_list vdecl { $2  ^ "/n" ^ $1 }

vdecl:
   INT ID SEMI { $2 }

stmt_list:
    /* nothing */  { " " }
  | stmt_list stmt { $2 ^ "\n " ^ $1 }

stmt:
    expr SEMI { " " ^ $1 ^ ";\n"  }
  | RETURN expr SEMI { "RETURN " ^ $2 }
  | LBRACE stmt_list RBRACE { $2 }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { "If: " ^ $3 ^ " then do: \n" ^ $5 ^ "\n" }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { "If: " ^ $3 ^ " then do: \n" ^ $5 ^ "\n else do: \n" ^ $7}
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { "FOR LOOP: " ^ $3 ^ " " ^ $5 ^ " " ^ $7 ^ " " ^ $9 }
  | WHILE LPAREN expr RPAREN stmt { "WHILE LOOP " ^ $3 ^ " " ^ $5 }

expr_opt:
    /* nothing */ { " " }
  | expr          { $1 }

expr:
    LITERAL          { "'" ^ (string_of_int $1) ^ "'" }
  | ID               { "_" ^ $1 ^ "_" }
  | expr PLUS   expr { $1 ^ " + " ^ $3 }
  | expr MINUS  expr { $1  ^ " - " ^ $3 }
  | expr TIMES  expr { $1  ^ " * " ^ $3 }
  | expr DIVIDE expr { $1  ^ " / " ^ $3  }
  | expr EQ     expr { $1^ " == " ^ $3 }
  | expr NEQ    expr { $1^ " != " ^ $3 }
  | expr LT     expr { $1^ " < " ^ $3 }
  | expr LEQ    expr { $1^ " <= " ^ $3 }
  | expr GT     expr { $1^ " > " ^ $3 }
  | expr GEQ    expr { $1^ " >= " ^ $3 }
  | ID ASSIGN expr   { $1 ^ "=" ^ $3 }
  | ID LPAREN actuals_opt RPAREN { "call " ^ $1 ^ "with: " ^ $3 }
  | LPAREN expr RPAREN { "(" ^ $2 ^ ")" }

actuals_opt:
    /* nothing */ { " " }
  | actuals_list  { $1 }

actuals_list:
    expr                    {$1}
  | actuals_list COMMA expr { $3 ^ ", " ^ $1 }
