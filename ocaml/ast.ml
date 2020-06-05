open Core

module M = Mark
module L = Lexer
module S = Symbol

type exp = 
  | Var of Symbol.t
  | Operator of Lexer.op * mexp * mexp
  | IntVal of int
and mexp = exp Mark.t

type stmt = 
  | Assign of Symbol.t * mexp
  | Return of mexp
type mstmt = stmt Mark.t

type program = mstmt list

let rec string_of_exp e = 
  match Mark.obj e with
  | Var s -> S.string_of_symbol s
  | Operator (op, e1, e2) -> 
    let op_str = L.string_of_op op
    in "(" ^ string_of_exp e1 ^ op_str ^ string_of_exp e2 ^ ")"
  | IntVal i -> string_of_int i

let string_of_stmt s = 
  match M.obj s with 
  | Assign (s, e2) -> S.string_of_symbol s ^ " = " ^ string_of_exp e2 ^ "; "
  | Return e -> "return " ^ string_of_exp e ^ "; "

let string_of_program ss = 
  let stmts = String.concat (List.map ss ~f:string_of_stmt) in
  "[ " ^ stmts ^ "]\n"