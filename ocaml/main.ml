module L = Lexer
module P = Parser
module T = Typecheck
module IR3 = Ir3

let () = 
  (* Comment/uncomment this line to toggle testing. *)
  Testing.run_tests();

  (* After the tests, try to compile something! *)
  let compile fname = 
    let fpath = "../tests/legal/" ^ fname ^ ".test" in
    let lexer = L.create fpath in
    let prog = P.parse_program lexer in
    T.typecheck lexer prog;
    let ir_prog = IR3.lower_program prog in
    let asm = Asm.string_of_asm ir_prog in
    Asm.asm_to_file ("bin/" ^ fname ^ ".s") asm;
    let (_ : Core.Unix.Exit_or_signal.t) = Core.Unix.system ("gcc -c bin/" ^ fname ^ ".s -o bin/" ^ fname ^ ".o") in
    let (_ : Core.Unix.Exit_or_signal.t) = Core.Unix.system ("gcc bin/" ^ fname ^ ".o runtime.c -o bin/" ^ fname) in
    let (_ : Core.Unix.Exit_or_signal.t) = Core.Unix.system ("./bin/" ^ fname) in
    ()
  in

  List.iter compile ["abc" ; "statements" ; "onevar"; "pmas"]