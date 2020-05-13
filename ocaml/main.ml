exception TestFail of string

module L = Lexer
module P = Parser

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

  let prog = P.parse_program (L.create "../tests/assignment.test") in
  print_endline (Ast.string_of_program prog);
  let prog = P.parse_program (L.create "../tests/abc.test") in
  print_endline (Ast.string_of_program prog);
  let prog = P.parse_program (L.create "../tests/statements.test") in
  print_endline (Ast.string_of_program prog);

  (* Illegal program tests. *)
  print_newline ();
  print_endline "Testing illegal programs...\n";

  (* Try to lex a program with illegal variable name. *)
  let () = 
    try print_tokens (L.create "../tests/bad/illegalvar.test")
    with
    | L.InvalidInt _ -> print_endline "caught illegal variable"
    | _ -> assert false
  in

  (* Try to parse a program with illegal assignment. *)
  try let (_ : Ast.program) = P.parse_program (L.create "../tests/bad/flipped_assignment.test") in ()
  with
  | P.ParserError s -> print_endline s
  | _ -> assert false