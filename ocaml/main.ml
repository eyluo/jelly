exception TestFail of string

module L = Lexer
module P = Parser
module T = Typecheck
module IR = Ir

let () = 
  (* Prints all of the tokens. *)
  let print_tokens lxr = 
    let rec acc_tokens acc =
      let tok = L.pop lxr in
      let tok_str = L.string_of_token tok in
      match tok with
      | L.Eof -> acc
      | _ -> acc_tokens (acc ^ tok_str ^ ";")
    in
    let result = "[ " ^ (acc_tokens "") ^ " ]\n" in
    print_string result
  in

  (* Ensures peek and pop work as expected. *)
  let rec test_peek lxr = 
    let tok1 = L.peek lxr in 
    let tok2 = L.pop lxr in
    if tok1 <> tok2 then raise (TestFail "peek and pop returned different values!")
    else if tok1 = L.Eof then ()
    else test_peek lxr
  in 

  (* Prints tokens to ensure they match with the test files. *)
  print_newline ();
  print_endline "Verifying tokens of arithmetic expressions...\n";

  print_tokens (L.create "../tests/add.test");
  print_tokens (L.create "../tests/whitespace.test");
  print_tokens (L.create "../tests/bigboys.test");
  print_tokens (L.create "../tests/pemdas.test");

  (* Test peeking against popping. *)
  print_newline ();
  print_endline "Verifying peek against pop operations...\n";

  test_peek (L.create "../tests/whitespace.test");

  (* Prints ASTs for expressions to ensure they match with the test files. *)
  print_newline ();
  print_endline "Verifying ASTs of arithmetic expressions...\n";

  let ast = P.parse (L.create "../tests/add.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (L.create "../tests/whitespace.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (L.create "../tests/bigboys.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (L.create "../tests/precedence.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (L.create "../tests/pemdas.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (L.create "../tests/pemdas2.test") in
  print_endline (Ast.string_of_exp ast);

  (* Prints tokens for programs to ensure they match with the test files. *)
  print_newline ();
  print_endline "Verifying tokens of programs...\n";

  print_tokens (L.create "../tests/assignment.test");
  print_tokens (L.create "../tests/abc.test");
  print_tokens (L.create "../tests/statements.test");

  (* Prints ASTs for programs to ensure they match with the test files. *)
  print_newline ();
  print_endline "Verifying ASTs of programs...\n";

  let prog = P.parse_program (L.create "../tests/abc.test") in
  print_endline (Ast.string_of_program prog);
  T.typecheck prog;
  let prog = P.parse_program (L.create "../tests/statements.test") in
  print_endline (Ast.string_of_program prog);
  T.typecheck prog;
  let ir_prog = IR.lower_program prog in
  print_endline (IR.string_of_ir ir_prog);
  let asm = Asm.string_of_asm ir_prog in
  Asm.asm_to_file asm;
  let (_ : Core.Unix.Exit_or_signal.t) = Core.Unix.system "gcc -c out.s -o out.o" in
  let (_ : Core.Unix.Exit_or_signal.t) = Core.Unix.system "gcc out.o runtime.c -o out" in
  let (_ : Core.Unix.Exit_or_signal.t) = Core.Unix.system "./out" in
  print_endline (IR.string_of_ir (IR.lower_program prog));
  let prog = P.parse_program (L.create "../tests/onevar.test") in
  print_endline (Ast.string_of_program prog);
  T.typecheck prog;
  print_endline (IR.string_of_ir (IR.lower_program prog));

  (* Illegal program tests. *)
  print_newline ();
  print_endline "Testing illegal programs...\n";

  (* Program without a return statement. *)
  let prog = P.parse_program (L.create "../tests/assignment.test") in
  print_endline (Ast.string_of_program prog);
  let () = 
    try T.typecheck prog
    with
    | T.TypeError msg -> print_endline msg
    | _ -> raise (TestFail "assignment.test does not have a return statement")
  in

  (* Program that references an undefined variable. *)
  let prog = P.parse_program (L.create "../tests/bad/early_ref.test") in
  print_endline (Ast.string_of_program prog);
  let () = 
    try T.typecheck prog
    with
    | T.TypeError msg -> print_endline msg
    | _ -> raise (TestFail "early_ref.test references e early")
  in

  (* Try to lex a program with illegal variable name. *)
  let () = 
    try print_tokens (L.create "../tests/bad/illegalvar.test")
    with
    | L.InvalidInt _ -> print_endline "caught illegal variable"
    | _ -> raise (TestFail "illegalvar.test has illegal variable names")
  in

  (* Try to parse a program with illegal assignment. *)
  try let (_ : Ast.program) = P.parse_program (L.create "../tests/bad/flipped_assignment.test") in ()
  with
  | P.ParserError s -> print_endline s
  | _ -> raise (TestFail "flipped_assignment.test reverses assignment order")
