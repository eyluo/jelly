open Core

module S = Symbol

type exp = 
  | Var of string
  | Operator of Lexer.op * exp * exp
  | IntVal of int

type stmt = 
  | Assign of Symbol.t * exp
  | Return of exp

type program = stmt list

let rec string_of_exp e = 
  match e with
  | Var s -> s
  | Operator (op, e1, e2) -> 
    let op_str = 
      (match op with
       | Lexer.Pow -> "^"
       | Lexer.Plus -> "+"
       | Lexer.Minus -> "-"
       | Lexer.Divide -> "/"
       | Lexer.Times -> "*")
    in "(" ^ (string_of_exp e1) ^ op_str ^ (string_of_exp e2) ^ ")"
  | IntVal i -> string_of_int i

let string_of_stmt s = 
  match s with
  | Assign (s, e2) -> (S.string_of_symbol s) ^ " = " ^ (string_of_exp e2) ^ "; "
  | Return e -> "return " ^ (string_of_exp e) ^ "; "

let string_of_program ss = 
  let stmts = List.fold (List.map ss ~f:string_of_stmt) ~init:"" ~f:(^) in
  "[ " ^ stmts ^ "]\n"