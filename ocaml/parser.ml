(* Parser implementation.
 * Currently supports operation precedence and grouping by associativity. *)

open Core

exception SyntaxError of string

(* 
 * atom : int | lparen * exp * lparen
 * exp : atom * op * exp 
 *)

let parse lexer =
    let rec parse_expr min_prec =
        let lhs = parse_atom () in
        let result = parse_expr_prec min_prec lhs in
        result
    and parse_expr_prec min_prec lhs = 
        let tok = Lexer.peek lexer in
        match tok with
        | Lexer.Eof | Lexer.IntVal _ -> let () = Lexer.drop lexer in lhs
        | Lexer.Operator op ->
            let prec, assoc = Lexer.op_info op in
            if prec < min_prec then lhs
            else 
                (* Only consume the token if it will be used in the AST *)
                let () = Lexer.drop lexer in
                (* For left-associativity, increase precedence to left-group items *)
                let next_min_prec = 
                    match assoc with
                    | Lexer.Left -> prec + 1
                    | Lexer.Right -> prec
                in
                let rhs = (parse_expr next_min_prec) in 
                parse_expr_prec min_prec (Ast.Operator(op, lhs, rhs))
        | _ -> lhs
    and parse_atom () =
        let tok = Lexer.pop lexer in
        match tok with
        | Lexer.IntVal i -> Ast.IntVal i
        | Lexer.LParen ->
            let e = parse_expr (0) in
            let tok2 = Lexer.pop lexer in
                (match tok2 with
                | Lexer.RParen -> e
                | _ -> raise (SyntaxError "Unbalanced parentheses"))
        | _ -> raise (SyntaxError "Illegal atom grammer")
    in
    parse_expr 0