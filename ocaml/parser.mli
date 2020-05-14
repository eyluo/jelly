exception ParserError of string

val parse : Lexer.lexer -> Ast.exp
val parse_stmt : Lexer.lexer -> Ast.stmt
val parse_program : Lexer.lexer -> Ast.program