exception TestFail of string

module L = Lexer
module P = Parser

let () = 
    (* Prints all of the tokens. *)
    let rec print_tokens lxr = 
        let tok = L.pop lxr in
        let () = print_string (L.string_of_token tok) in
        match tok with
        | L.Eof -> ()
        | _ -> (print_tokens lxr)
    in

    let rec test_peek lxr = 
        let tok1 = L.peek lxr in 
        let tok2 = L.pop lxr in
        if tok1 <> tok2 then raise (TestFail "peek and pop returned different values!")
        else if tok1 = L.Eof then ()
        else test_peek lxr
    in 
    
    let () = print_tokens (L.create "../tests/add.test") in
    let () = print_endline("") in
    let () = print_tokens (L.create "../tests/whitespace.test") in
    let () = print_endline("") in
    let () = print_tokens (L.create "../tests/bigboys.test") in
    let () = print_endline("") in
    let () = print_tokens (L.create "../tests/pemdas.test") in
    let () = print_endline("") in
    let () = test_peek (L.create "../tests/whitespace.test") in


    let ast = P.parse (L.create "../tests/add.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = P.parse (L.create "../tests/whitespace.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = P.parse (L.create "../tests/bigboys.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = P.parse (L.create "../tests/precedence.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = P.parse (L.create "../tests/pemdas.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    let ast = P.parse (L.create "../tests/pemdas2.test") in
    let () = print_endline (Ast.string_of_exp ast) in
    ()