exception ParserError of string

val parse : Lexer.t -> Ast.exp
val parse_stmt : Lexer.t -> Ast.stmt
val parse_program : Lexer.t -> Ast.program