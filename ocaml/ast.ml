open Core

module M = Mark
module L = Lexer
module S = Symbol
module T = Type

type exp = 
  | Var of Symbol.t
  | Operator of Lexer.Op.t * mexp * mexp
  | IntVal of int
  | BoolVal of bool
and mexp = exp Mark.t

type stmt = 
  | Declare of Type.t * Symbol.t
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
  | BoolVal b -> string_of_bool b

let string_of_stmt s = 
  match M.obj s with 
  | Declare (t, s) -> String.concat ?sep:(Some " ") [T.string_of_type t; S.string_of_symbol s]
  | Assign (s, e2) -> S.string_of_symbol s ^ " = " ^ string_of_exp e2 ^ "; "
  | Return e -> "return " ^ string_of_exp e ^ "; "

let string_of_program ss = 
  let stmts = String.concat (List.map ss ~f:string_of_stmt) in
  "[ " ^ stmts ^ "]\n"