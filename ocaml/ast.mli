type exp = 
    | Operator of Lexer.op * exp * exp
    | IntVal of int

val string_of_exp : exp -> string