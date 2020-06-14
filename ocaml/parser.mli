exception ParserError of string

val parse : Lexer.t -> Ast.mexp
val parse_stmt : Lexer.t -> Ast.mstmt
val parse_program : Lexer.t -> Ast.program
