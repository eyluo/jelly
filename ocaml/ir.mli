type operand = 
  | Immediate of int
  | Temporary of Temp.t

type instr =
  | Store of Temp.t * operand
  | BinOp of Temp.t * Lexer.op * operand * operand

type t = instr list

val lower_exp : Temp.t -> Ast.exp -> t
val lower_program : Ast.program -> t

val string_of_ir : t -> string