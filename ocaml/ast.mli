type exp = 
  | Var of Symbol.t
  | Operator of Lexer.op * exp * exp
  | IntVal of int

type stmt = 
  | Assign of Symbol.t * exp
  | Return of exp

type program = stmt list

val string_of_exp : exp -> string
val string_of_stmt : stmt -> string
val string_of_program : program -> string