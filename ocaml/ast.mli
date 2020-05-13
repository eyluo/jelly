type exp = 
  | Var of string
  | Operator of Lexer.op * exp * exp
  | IntVal of int

type stmt = 
  | Assign of Lexer.Symbol * exp
  | Return of exp

type program = stmt list

val string_of_exp : exp -> string
val string_of_stmt : stmt -> string
val string_of_program : program -> string