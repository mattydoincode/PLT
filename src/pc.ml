
(* pubCrawl compiler
	1. give stdin to scanner, get tokens
	2. give tokens to parser, get AST
	3. give AST to analyzer, get semantic tree
	4. give semantic tree to java converter, get java tree
	5. give java tree to java code generator, get java code
	6. give java code to java compiler, get executable
	7. run java executable
*)

type action = Ast | Compile | Sast | Java

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast); ("-c", Compile); ("-s", Sast); ("-j", Java);]
  else Sast in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
  | Ast -> 
      print_string (Ast.string_of_prog program)
  | Sast ->
      let ap = Analyzer.annotate_prog program in
      print_string "\n******* ORIG SAST ********\n";
      print_string (Sast.string_of_prog ap);
      let constraints = Analyzer.collect_prog ap in
      print_string "\n******** CONSTRAINTS ********\n";
      print_string (Sast.string_of_constraints constraints);
      let subs = Analyzer.unify (List.rev constraints) in
      print_string "\n******* SUBS ********\n";
      print_string (Sast.string_of_subs subs);
      let aProgram = Analyzer.apply_stmts ap subs in
      print_string "\n******* SAST ********\n";
      print_string (Sast.string_of_prog aProgram);
      print_string "\n******* INFERENCES ********\n";
      print_string (Sast.string_of_inferred_prog aProgram)
  | Java ->
      let ap = Analyzer.infer_prog program in
      let jp = Javagen.gen_program "alden" ap in
      print_string jp;
      print_string "\n"
  | Compile -> 
      print_string "not yet!"
