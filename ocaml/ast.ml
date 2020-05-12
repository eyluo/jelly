open Core

type exp = 
    | Operator of Lexer.op * exp * exp
    | IntVal of int

let rec string_of_exp e = 
    match e with
    | Operator (op, e1, e2) -> 
        let op_str = 
            (match op with
            | Lexer.Pow -> "^"
            | Lexer.Plus -> "+"
            | Lexer.Minus -> "-"
            | Lexer.Divide -> "/"
            | Lexer.Times -> "*")
        in String.concat ["(" ; string_of_exp e1 ; op_str ; string_of_exp e2 ; ")"]
    | IntVal i -> string_of_int i