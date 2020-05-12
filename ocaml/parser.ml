open Core

exception SyntaxError of string

let parse lexer =
    let precedence op = 
        let prec, assoc = Lexer.op_info op in
        match assoc with
        | Lexer.Left -> prec + 1
        | Lexer.Right -> prec
    in
    let rec parse_expr min_prec =
        let lhs = parse_atom () in
        let tok = Lexer.peek lexer in
        match tok with
        | Lexer.Eof ->
            let () = Lexer.drop lexer in lhs
        | Lexer.IntVal _ -> raise (SyntaxError "two consecutive IntVal")
        | Lexer.Operator op ->
            let () = Lexer.drop lexer in
            let prec = precedence op in             (* TODO: figure out how precedence works without cutting off other expressions *)
            let (_ : bool) = prec < min_prec in 
            let rhs = parse_expr prec in
            Ast.Operator(op, lhs, rhs)
        | _ -> lhs
    and parse_atom () =
        let tok = Lexer.pop lexer in
        match tok with
        | Lexer.IntVal i -> Ast.IntVal i
        | Lexer.LParen ->
            let e = parse_expr (1) in
            let tok2 = Lexer.pop lexer in
                (match tok2 with
                | Lexer.RParen -> e
                | _ -> raise (SyntaxError "Unbalanced parentheses"))
        | _ -> raise (SyntaxError "boogabooga")
    in
    parse_expr 0