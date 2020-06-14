open Core

exception IRError of string

type operand =
  | Immediate of int
  | Temporary of Temp.t

type instr =
  | Store of Temp.t * operand
  | BinOp of Temp.t * Lexer.Op.t * operand * operand

type t = instr list

(* Symbol -> Temp.t *)
let seen = Hashtbl.create (module Symbol)

(* Converts the expression into triple IR and then stores the result in temp. *)
let lower_exp temp exp =
  let rec lower_exp' temp exp =
    let exp' = Mark.obj exp in
    match exp' with
    | Ast.Var s ->
      (match Hashtbl.find seen s with
       | Some t -> [ Store (temp, Temporary t) ]
       | None -> raise (IRError "symbol does not have corresponding temp"))
    | Ast.Operator (op, e1, e2) ->
      let t1 = Temp.create () in
      let t2 = Temp.create () in
      let lower1 = lower_exp' t1 e1 in
      let lower2 = lower_exp' t2 e2 in
      List.concat [ lower1; lower2; [ BinOp (temp, op, Temporary t1, Temporary t2) ] ]
    | Ast.IntVal i -> [ Store (temp, Immediate i) ]
    | Ast.BoolVal b ->
      if b then [ Store (temp, Immediate 1) ] else [ Store (temp, Immediate 0) ]
  in
  lower_exp' temp exp
;;

let rec lower_program program =
  match program with
  | [] -> []
  | p :: ps ->
    let instr_ir =
      let p' = Mark.obj p in
      match p' with
      | Ast.Declare _ -> []
      | Ast.Assign (sym, exp) ->
        let t = Temp.create () in
        let result = lower_exp t exp in
        Hashtbl.set seen ~key:sym ~data:t;
        result
      | Ast.Return exp ->
        let t = Temp.create () in
        lower_exp t exp
    in
    List.append instr_ir (lower_program ps)
;;

let string_of_ir ir =
  let string_of_operand op =
    match op with
    | Immediate i -> string_of_int i
    | Temporary t -> Temp.to_string t
  in
  let string_of_instr instr =
    match instr with
    | Store (t, op) -> Temp.to_string t ^ " = " ^ string_of_operand op
    | BinOp (t, optr, op1, op2) ->
      Temp.to_string t
      ^ " = "
      ^ string_of_operand op1
      ^ Lexer.string_of_op optr
      ^ string_of_operand op2
  in
  let ir_strs = List.map ir ~f:string_of_instr in
  String.concat ?sep:(Some "\n") ir_strs
;;
