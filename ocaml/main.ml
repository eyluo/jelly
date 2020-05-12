exception TestFail of string

let () = 
    (* Prints all of the tokens. *)
    let rec print_tokens lxr = 
        let tok = Lexer.pop lxr in
        let () = print_string (Lexer.string_of_token tok) in
        match tok with
        | Lexer.Eof -> ()
        | _ -> (print_tokens lxr)
    in

    let rec test_peek lxr = 
        let tok1 = Lexer.peek lxr in 
        let tok2 = Lexer.pop lxr in
        if tok1 <> tok2 then raise (TestFail "peek and pop returned different values!")
        else if tok1 = Lexer.Eof then ()
        else test_peek lxr
    in 
    
    let () = print_tokens (Lexer.create "../tests/add.test") in
    let () = print_endline("") in
    let () = print_tokens (Lexer.create "../tests/whitespace.test") in
    let () = print_endline("") in
    let () = print_tokens (Lexer.create "../tests/bigboys.test") in
    let () = print_endline("") in
    let () = print_tokens (Lexer.create "../tests/pemdas.test") in
    let () = print_endline("") in
    let () = test_peek (Lexer.create "../tests/whitespace.test") in


    let ast = Parser.parse (Lexer.create "../tests/add.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = Parser.parse (Lexer.create "../tests/whitespace.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = Parser.parse (Lexer.create "../tests/bigboys.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = Parser.parse (Lexer.create "../tests/precedence.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = Parser.parse (Lexer.create "../tests/pemdas.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = Parser.parse (Lexer.create "../tests/pemdas2.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    ()