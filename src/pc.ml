
(* pubCrawl compiler
	1. give stdin to scanner, get tokens
	2. give tokens to parser, get AST
	3. give AST to analyzer, get semantic tree
	4. give semantic tree to java converter, get java tree
	5. give java tree to java code generator, get java code
	6. give java code to java compiler, get executable
	7. run java executable
*)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let result = Parser.program Scanner.token lexbuf in
  print_endline (result)
