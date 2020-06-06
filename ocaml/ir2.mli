type operand =
  | Immediate of int
  | Temporary of Temp.t

type instr = 
  | Store of Temp.t * operand
  | BinOp of Lexer.Op.t * Temp.t * operand

type t = instr list

val lower_ir : Ir3.t -> t

val string_of_ir : t -> string