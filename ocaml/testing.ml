exception TestFail of string

module M = Mark
module Lx = Lexer
module P = Parser
module T = Typecheck
module IR3 = Ir3
module IR2 = Ir2
module L = Live
module G = Graph

(* Prints all of the tokens. *)
let print_tokens lxr = 
  let rec acc_tokens acc =
    let mtok = Lx.pop lxr in
    let tok = M.obj mtok in
    let tok_str = Lx.string_of_token mtok in
    match tok with
    | Lx.Eof -> acc
    | _ -> acc_tokens (acc ^ tok_str ^ ";")
  in
  let result = "[ " ^ acc_tokens "" ^ " ]\n" in
  print_string result

let _test_lexer () =
  (* Ensures peek and pop work as expected. *)
  let rec test_peek lxr = 
    let mtok1 = Lx.peek lxr in 
    let mtok2 = Lx.pop lxr in
    if mtok1 <> mtok2 then raise (TestFail "peek and pop returned different values!")
    else if M.obj mtok1 = Lx.Eof then ()
    else test_peek lxr
  in
  (* Prints tokens to ensure they match with the test files. *)
  print_endline "Verifying tokens of arithmetic expressions...\n";

  print_tokens (Lx.create "../tests/add.test");
  print_tokens (Lx.create "../tests/whitespace.test");
  print_tokens (Lx.create "../tests/bigboys.test");
  print_tokens (Lx.create "../tests/pemdas.test");
  print_tokens (Lx.create "../tests/legal/bool_abc.test");
  print_tokens (Lx.create "../tests/legal/bool_int.test");

  (* Test peeking against popping. *)
  print_newline ();
  print_endline "Verifying peek against pop operations...\n";

  test_peek (Lx.create "../tests/whitespace.test");
  print_newline (); 
  ()

let _test_ast () = 
  (* Prints ASTs for expressions to ensure they match with the test files. *)
  print_endline "Verifying ASTs of arithmetic expressions...\n";

  let ast = P.parse (Lx.create "../tests/add.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (Lx.create "../tests/whitespace.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (Lx.create "../tests/bigboys.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (Lx.create "../tests/precedence.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (Lx.create "../tests/pemdas.test") in
  print_endline (Ast.string_of_exp ast);
  let ast = P.parse (Lx.create "../tests/pemdas2.test") in
  print_endline (Ast.string_of_exp ast);

  (* Prints tokens for programs to ensure they match with the test files. *)
  print_newline ();
  print_endline "Verifying tokens of programs...\n";

  print_tokens (Lx.create "../tests/assignment.test");
  print_tokens (Lx.create "../tests/legal/abc.test");
  print_tokens (Lx.create "../tests/legal/statements.test");

  (* Prints ASTs for programs to ensure they match with the test files. *)
  print_newline ();
  print_endline "Verifying ASTs of programs...\n";

  let lexer = Lx.create "../tests/legal/abc.test" in
  let prog = P.parse_program lexer in
  print_endline (Ast.string_of_program prog);
  T.typecheck lexer prog;
  let ir_prog = IR3.lower_program prog in
  print_endline (IR3.string_of_ir ir_prog);
  print_newline ();
  let ir2_prog = IR2.lower_ir ir_prog in
  print_endline (IR2.string_of_ir ir2_prog);
  let livescan = L.scan ir2_prog in
  print_endline (G.string_of_graph livescan);
  let seq = G.mcs livescan in
  print_endline (G.string_of_order seq);
  let colored = G.color livescan seq in
  print_endline (G.string_of_color colored);

  let lexer = Lx.create "../tests/legal/statements.test" in
  let prog = P.parse_program lexer in
  print_endline (Ast.string_of_program prog);
  T.typecheck lexer prog;
  let ir_prog = IR3.lower_program prog in
  print_endline (IR3.string_of_ir ir_prog);
  print_newline ();
  let ir2_prog = IR2.lower_ir ir_prog in
  print_endline (IR2.string_of_ir ir2_prog);
  let livescan = L.scan ir2_prog in
  print_endline (G.string_of_graph livescan);
  let seq = G.mcs livescan in
  print_endline (G.string_of_order seq);
  let colored = G.color livescan seq in
  print_endline (G.string_of_color colored);

  let lexer = Lx.create "../tests/legal/onevar.test" in
  let prog = P.parse_program lexer in
  print_endline (Ast.string_of_program prog);
  T.typecheck lexer prog;
  let ir_prog = IR3.lower_program prog in
  print_endline (IR3.string_of_ir ir_prog);
  print_newline ();
  let ir2_prog = IR2.lower_ir ir_prog in
  print_endline (IR2.string_of_ir ir2_prog);
  let livescan = L.scan ir2_prog in
  print_endline (G.string_of_graph livescan);
  let seq = G.mcs livescan in
  print_endline (G.string_of_order seq);
  let colored = G.color livescan seq in
  print_endline (G.string_of_color colored);

  let lexer = Lx.create "../tests/legal/pmas.test" in
  let prog = P.parse_program lexer in
  print_endline (Ast.string_of_program prog);
  T.typecheck lexer prog;
  let ir_prog = IR3.lower_program prog in
  print_endline (IR3.string_of_ir ir_prog);
  print_newline ();
  let ir2_prog = IR2.lower_ir ir_prog in
  print_endline (IR2.string_of_ir ir2_prog);
  let livescan = L.scan ir2_prog in
  print_endline (G.string_of_graph livescan);
  let seq = G.mcs livescan in
  print_endline (G.string_of_order seq);
  let colored = G.color livescan seq in
  print_endline (G.string_of_color colored);
  print_newline ();
  ()

let _test_illegal () = 
  (* Illegal program tests. *)
  print_endline "Testing illegal programs...\n";

  (* Program without a return statement. *)
  let lexer = Lx.create "../tests/assignment.test" in
  let prog = P.parse_program lexer in
  print_endline (Ast.string_of_program prog);
  let () = 
    try T.typecheck lexer prog
    with
    | T.TypeError msg -> print_endline msg
    | _ -> raise (TestFail "assignment.test does not have a return statement")
  in

  (* Program that references an undefined variable. *)
  let lexer = Lx.create "../tests/bad/early_ref.test" in
  let prog = P.parse_program lexer in
  print_endline (Ast.string_of_program prog);
  let () = 
    try T.typecheck lexer prog
    with
    | T.TypeError msg -> print_endline msg
    | _ -> raise (TestFail "early_ref.test references e early")
  in

  (* Try to lex a program with illegal operator. *)
  let () = 
    try print_tokens (Lx.create "../tests/bad/illegalchar.test")
    with
    | Lx.InvalidToken _ -> print_endline "caught illegal token"
    | _ -> raise (TestFail "illegalchar.test has illegal operator")
  in

  (* Try to lex a program with illegal variable name. *)
  let () = 
    try print_tokens (Lx.create "../tests/bad/illegalvar.test")
    with
    | Lx.InvalidInt _ -> print_endline "caught illegal variable"
    | _ -> raise (TestFail "illegalvar.test has illegal variable names")
  in

  (* Try to parse a program with illegal assignment. *)
  let () = 
    try let (_ : Ast.program) = P.parse_program (Lx.create "../tests/bad/flipped_assignment.test") in ()
    with
    | P.ParserError s -> print_endline s
    | _ -> raise (TestFail "flipped_assignment.test reverses assignment order")
  in

  (* Try to parse a program with early reference. *)
  let () = 
    try let (_ : Ast.program) = P.parse_program (Lx.create "../tests/bad/flipped_assignment.test") in ()
    with
    | P.ParserError s -> print_endline s
    | _ -> raise (TestFail "flipped_assignment.test reverses assignment order")
  in ()


let run_tests () = 
  _test_lexer ();
  (* _test_ast ();
     _test_illegal (); *)