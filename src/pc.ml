
(* pubCrawl compiler
	1. give stdin to scanner, get tokens
	2. give tokens to parser, get AST
	3. give AST to analyzer, get semantic tree
	4. give semantic tree to java converter, get java tree
	5. give java tree to java code generator, get java code
	6. give java code to java compiler, get executable
	7. run java executable
*)

type action = Ast | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast); ("-c", Compile) ]
  else Ast in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    Ast -> let listing = Ast.string_of_program program
           in print_string listing
  | Compile -> print_endline "Oops, not done yet!"
