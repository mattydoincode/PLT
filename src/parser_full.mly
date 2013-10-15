%{  %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA RBRACK LBRACK
%token PLUS MINUS TIMES DIVIDE ASSIGN COLON ARROW
%token EQ NEQ LT LEQ GT GEQ CONCAT
%token RETURN IF ELSE FOR WHILE ELIF
%token <int> LITERAL
%token <string> ID
%token EOF


%nonassoc NOELSE
%nonassoc ELSE
%nonassoc ELIF
%nonassoc ASSIGN COLON
%left EQ NEQ
%left CONCAT
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc ARROW
%nonassoc LBRACK
%nonassoc LPAREN

%start program
%type <string> program

%%

program:
   /* nothing */ { "" }
 | program stmt_list { $2 ^ $1 }

stmt_list:
 /* nothing */ { "" }
 | stmt_list stmt {$1 ^ " " ^ $2}

stmt:
    assignment SEMI { " " ^ $1 ^ ";\n"  }
  | functioncall SEMI { " " ^ $1 ^ ";\n"  }
  | fdecl { $1 }
  | RETURN expr SEMI { "RETURN " ^ $2 }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE elseifs elsehandling
  { "If: " ^ $3 ^ " then do: \n" ^ $6 ^ $8 ^ $9 ^ "\n" }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN LBRACE stmt_list RBRACE
     { "FOR LOOP: " ^ $3 ^ " " ^ $5 ^ " " ^ $7 ^ " " ^ $10 }
  | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { "WHILE LOOP " ^ $3 ^ " " ^ $6 }

elseifs:
 /*nothing */ { " " }
| ELIF LPAREN expr RPAREN LBRACE stmt_list RBRACE elseifs
  { "elif (" ^ $3 ^ ") {\n" ^ $6 ^ "}\n" }

elsehandling:
  /*nothing*/ { " " }
|  ELSE LBRACE stmt_list RBRACE { "Else {\n" ^ $3 ^ "}\n"}

assignment:
  ID ASSIGN expr {$1 ^ " = " ^ $3}

fdecl:
   ID func1 { $1 ^ $2 }

func1:
  LPAREN formals RPAREN ARROW LBRACE stmt_list RBRACE
     {"(" ^ $2 ^ ") -> {\n" ^  $6 ^  "}\n" }
  | LPAREN formals RPAREN ARROW expr SEMI
     {"(" ^ $2 ^ ") -> " ^  $5 ^ ";\n" }

functioncall:
  expr LPAREN actuals_opt RPAREN {$1 ^ "(" ^ $3 ^ ")"}


expr_opt:
    /* nothing */ { " " }
  | expr          { $1 }

expr:
    LITERAL           { "'" ^ (string_of_int $1) ^ "'" }
  | ID                { "" ^ $1 ^ "" }
  | functioncall      { $1 }
  | func1         { $1 }
  | access            { $1 }
  | creation          { $1 }
  | expr CONCAT expr  { $1 ^ " ^ " ^ $3}
  | expr PLUS   expr  { $1 ^ " + " ^ $3 }
  | expr MINUS  expr  { $1  ^ " - " ^ $3 }
  | expr TIMES  expr  { $1  ^ " * " ^ $3 }
  | expr DIVIDE expr  { $1  ^ " / " ^ $3  }
  | expr EQ     expr  { $1^ " == " ^ $3 }
  | expr NEQ    expr  { $1^ " != " ^ $3 }
  | expr LT     expr  { $1^ " < " ^ $3 }
  | expr LEQ    expr  { $1^ " <= " ^ $3 }
  | expr GT     expr  { $1^ " > " ^ $3 }
  | expr GEQ    expr  { $1^ " >= " ^ $3 }
  | LPAREN expr RPAREN { "(" ^ $2 ^ ")" }


creation: 
  LBRACK actuals_opt RBRACK { "[" ^ $2 ^ "]"}
  | LBRACE RBRACE { "{}" }
  | LBRACE properties RBRACE { "{" ^ $2 ^ "}" }

properties: 
  ID COLON expr { $1 ^ ": " ^ $3 }
  | properties COMMA ID COLON expr { $1 ^ "," ^ $3 ^ ": " ^ $5 }

access: 
  expr LBRACK expr RBRACK { $1 ^ "[" ^ $3 ^ "]"}
  | expr LBRACK expr_opt COLON expr_opt RBRACK { $1 ^ "[" ^ $3 ^ ":" ^ $5 ^ "]" }


actuals_opt:
    /* nothing */ { " " }
  | actuals_list  { $1 }

actuals_list:
    expr                    {$1}
  | actuals_list COMMA expr { $3 ^ ", " ^ $1 }

formals:
    /* nothing */ { " " }
  | formal_list   {$1}

formal_list:
    ID                   { $1 }
  | formal_list COMMA ID { $3 ^ ", " ^ $1 }

