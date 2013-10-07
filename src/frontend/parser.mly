%{ open Ast %}

%token OR AND NOT PLUS MULT MINUS DIV LT LTE GT GTE EQ NE ASSIGN
%token MOD
%token SEMI COMMA LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET NEWLINE DOUBLE_QUOTE EOF 
%token FUNC VAR ASYNC ARR IF ELSE NOELSE FOR BREAK RETURN
%token PRINTLN
%token <string> ESCAPESEQ
%token <string> ID
%token <string> STRING
%token <int> INT_LIT
%token <float> DOUBLE_LIT
%token <bool> BOOL_LIT
%token <char> CHAR_LIT

%start program
%type <Ast.program> program

%%

program:
	| program_rev { List.rev $1 }

program_rev:
	| declaration { [$1] }
	| program_rev declaration { $2::$1 }

declaration:
	| funcdec { $1 }
	| vardec NEWLINE { Vardec($1) }
	| NEWLINE { Newline() }

block: LBRACE stmt_list_opt RBRACE { $2 }

stmt_list_opt:
	| /* empty */ { [] }
	| stmt_list { List.rev $1 }

stmt_list:
	| NEWLINE { [] }
	| stmt_list stmt NEWLINE { $2::$1 }
	| stmt_list NEWLINE { $1 }

funcdec: FUNC ID LPAREN formal_list_opt RPAREN opt_vartype block 
	{  Funcdec{fid=$2; func_header = {ret_type=$6; params= $4} ; body = $7 } }

anon: FUNC LPAREN formal_list_opt RPAREN opt_vartype block {
	({ret_type = $5; params = $3}, $6)
}

vardec:
	| VAR new_id_list var_type 
	{ {id_list=List.rev $2; var_type=$3; actual_list = [] } } 
	
	| VAR new_id_list var_type ASSIGN actual_list 
	{ {id_list=List.rev $2; var_type=$3; actual_list = List.rev $5 } }

formal_list_opt:
	/* empty */ { [] }
	| formal_list { List.rev $1 }

formal_list:
	formal_type_cluster { formalize_cluster $1 }
	| formal_list COMMA formal_type_cluster { (formalize_cluster $3)@$1 }

formal_type_cluster:
	| var_type { ImplicitParam($1) }
	| new_id_list var_type { ExplicitParams($2, List.rev $1) }

new_id_list:
	| ID { [ $1 ] }
	| new_id_list COMMA ID { $3::$1 }

opt_vartype:
	/* empty */ { None }
	| var_type { Some($1) }

single_type:
	| FUNC LPAREN formal_list_opt RPAREN opt_vartype {
		FunkFunc({ret_type = $5; params = $3})
	}
	| INT {FunkInt}
	| DOUBLE {FunkDouble}
	| CHAR {FunkChar}
	| BOOL {FunkBool}

var_type:	
	| opt_array_marker single_type { ($2, $1) }
arr_header:
	| array_marker single_type { ($2, $1) }

opt_array_marker:
 	| /* empty */ { [] }
	| array_marker { List.rev $1 }

array_marker:
	| array_level { [$1] }
	| array_marker array_level { $2::$1 }
	
array_level:
	| LBRACKET RBRACKET { SingleConst(IntVal(-1)) }
	| LBRACKET expr RBRACKET { $2 }

/* Array value */
actual_arr:
	| arr_header LBRACE actual_list_opt RBRACE { ($1, $3) }
actual_list_opt:
	/* empty */ { [] }
	| actual_list { List.rev $1 }

/*
 * Any list of expressions. Could be paramater values (possibly of different
 * types) or array elements (must be of same type)
 */
actual_list:
	| expr { [ExprRVal($1)] }
	| anon { [FuncRVal($1)] }
	| actual_list COMMA expr { ExprRVal($3)::$1 }
	| actual_list COMMA anon { FuncRVal($3)::$1 }

obj_get_expr_list:
	obj_get_expr { [$1] }
	| obj_get_expr_list COMMA obj_get_expr { $3::$1 }

assign_stmt:
	| obj_get_expr_list ASSIGN actual_list { Assignment(List.rev $1, List.rev $3) }

assign_stmt_opt:
	/* empty */ { None }
	| assign_stmt { Some($1) } /* Will need to run regression on this */
stmt:
	| block { Block($1) }
	| func_call_expr { FunctionCall($1) }
	| RETURN expr { Return(Some(ExprRVal($2))) }
	| RETURN anon { Return(Some(FuncRVal($2))) }
	| BREAK { Break }
	| IF expr block { IfBlock(($2, $3)) }
	| IF expr block ELSE block { IfElseBlock(($2, $3, $5))}
	| FOR assign_stmt_opt SEMI expr_opt SEMI
	  assign_stmt_opt block { ForBlock(($2, $4, $6, $7)) } //for loop
	| FOR expr block { WhileBlock(($2, $3)) } // while loop
	| FOR block { WhileBlock((SingleConst(BoolVal(true)), $2)) } //forever loop
	| assign_stmt { $1 }
	| vardec { Declaration($1) }

expr_opt:
	/* empty */ { None }
	| expr { Some($1) }
expr:
	| or_expr { $1 }
or_expr:
	| and_expr { $1 }
	| or_expr OR and_expr { FunkBinExpr($1, Or, $3) }
and_expr:
	| bor_expr { $1 }
	| and_expr AND bor_expr { FunkBinExpr($1, And, $3) }
bor_expr:
	| bxor_expr { $1 }
	| bor_expr BOR bxor_expr { FunkBinExpr($1, BOr, $3) }
bxor_expr:
	| band_expr { $1 }
	| bxor_expr BXOR band_expr { FunkBinExpr($1, BXor, $3) }
band_expr:
	| eq_expr { $1 }
	| band_expr BAND eq_expr { FunkBinExpr($1, BAnd, $3) }
eq_expr:
	| comp_expr { $1 }
	| eq_expr eq_op comp_expr { FunkBinExpr($1, $2, $3) }
eq_op:
	| EQ { Eq }
	| NE { NEq }
comp_expr:
	| shift_expr { $1 }
	| comp_expr comp_op shift_expr { FunkBinExpr($1, $2, $3) }
comp_op:
	| LT { LeT }
	| LTE { LE }
	| GT { GrT }
	| GTE { GE }
shift_expr:
	| add_expr { $1 }
	| shift_expr shift_op add_expr { FunkBinExpr($1, $2, $3) }
shift_op:
	| LSHIFT { LSh }
	| RSHIFT { RSh }
add_expr:
	| mult_expr { $1 }
	| add_expr add_op mult_expr { FunkBinExpr($1, $2, $3) }
add_op:
	| PLUS { Add }
	| MINUS { Sub }
mult_expr:
	| un_expr { $1 }
	| mult_expr mult_op un_expr { FunkBinExpr($1, $2, $3) }
mult_op:
	| MULT { Mult }
	| DIV { Div }
	| MOD { Mod }
un_expr:
	| post_expr { $1 }
	| un_op un_expr { FunkUnExpr($1, $2) }
un_op:
	| MINUS { IntNeg }
	| BNOT { BitNot }
	| NOT { Not }
	| PLUS { Positive }
post_expr:
	| obj_get_expr { $1 }
	| func_call_expr { FunkCallExpr($1) }
obj_get_expr:
	| primary_expr { $1 }
	| post_expr LBRACKET expr RBRACKET { FunkArrExpr($1, $3) }
func_call_expr:
	| post_expr LPAREN actual_list_opt RPAREN { ($1, $3) }
primary_expr:
	| INT_LIT { SingleConst(IntVal($1)) }
	| DOUBLE_LIT { SingleConst(DoubleVal($1)) }
	| CHAR_LIT { SingleConst(CharVal($1)) }
	| BOOL_LIT { SingleConst(BoolVal($1)) }
	| STRING {
		let rec listify i s =
			if i < String.length s then
				ExprRVal(SingleConst(CharVal(s.[i])))
					::(listify (i + 1) s)
			else [] in
                let listed = listify 0 $1 in
		ArrayLit((FunkChar,[SingleConst(IntVal(List.length
                                                                   listed)
                                                          )
                                              ]
                                    ), listed
                        )
	}
	| ID { Variable({id = $1; bare_type = None}) }
	| actual_arr { ArrayLit(fst $1, snd $1) }
	| ASYNC block { FunkAsyncExpr($2) }
	| LPAREN expr RPAREN { $2 }
