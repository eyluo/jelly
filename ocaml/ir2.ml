open Core
module IR3 = Ir3

type operand =
  | Immediate of int
  | Temporary of Temp.t

type instr =
  | Store of Temp.t * operand
  | BinOp of Lexer.Op.t * Temp.t * operand

type t = instr list

let lower_ir instrs =
  let lower_operand op =
    match op with
    | IR3.Immediate i -> Immediate i
    | IR3.Temporary t -> Temporary t
  in
  let lower_ir' instr =
    match instr with
    | IR3.Store (temp, op) -> [ Store (temp, lower_operand op) ]
    | IR3.BinOp (temp, optr, op1, op2) ->
      (* TODO: this will be way different when we extend support beyond arithmetic. *)
      let op1', op2' = lower_operand op1, lower_operand op2 in
      [ Store (temp, op2'); BinOp (optr, temp, op1') ]
  in
  List.concat (List.map instrs ~f:lower_ir')
;;

let string_of_ir instrs =
  let string_of_operand op =
    match op with
    | Immediate i -> string_of_int i
    | Temporary t -> Temp.string_of_temp t
  in
  let string_of_instr instr =
    match instr with
    | Store (temp, op) -> Temp.string_of_temp temp ^ " = " ^ string_of_operand op
    | BinOp (op, dest, src) ->
      Temp.string_of_temp dest
      ^ " = "
      ^ string_of_operand src
      ^ Lexer.string_of_op op
      ^ Temp.string_of_temp dest
  in
  let ir_strs = List.map instrs ~f:string_of_instr in
  String.concat ?sep:(Some "\n") ir_strs
;;
