(* Parser implementation.
 * Currently supports operation precedence and grouping by associativity. *)

open Core

module L = Lexer

exception SyntaxError of string

(* Defines operation associativity. *)
type assoc = Left | Right

(* 
 * atom : int | lparen * exp * lparen
 * exp : atom * op * exp 
 *)

let parse lexer =
  let op_info op = 
    match op with
    | L.Pow -> (3, Right)
    | L.Plus | L.Minus -> (1, Left)
    | L.Times | L.Divide -> (2, Left)
  in
  let rec parse_expr min_prec =
    let lhs = parse_atom () in
    let result = parse_expr_prec min_prec lhs in
    result
  and parse_expr_prec min_prec lhs = 
    let tok = L.peek lexer in
    match tok with
    | L.Eof | L.IntVal _ -> L.drop lexer; lhs
    | L.Operator op ->
      let prec, assoc = op_info op in
      if prec < min_prec then lhs
      else 
        (* Only consume the token if it will be used in the AST *)
        let () = L.drop lexer in
        (* For left-associativity, increase precedence to left-group items *)
        let next_min_prec = 
          match assoc with
          | Left -> prec + 1
          | Right -> prec
        in
        let rhs = parse_expr next_min_prec in 
        parse_expr_prec min_prec (Ast.Operator(op, lhs, rhs))
    | _ -> lhs
  and parse_atom () =
    let tok = L.pop lexer in
    match tok with
    | L.IntVal i -> Ast.IntVal i
    | L.LParen ->
      let e = parse_expr 0 in
      let tok2 = L.pop lexer in
      (match tok2 with
       | L.RParen -> e
       | _ -> raise (SyntaxError "Unbalanced parentheses"))
    | _ -> raise (SyntaxError "Illegal atom grammar")
  in
  parse_expr 0

let parse_stmt lexer = let _ = lexer in Ast.Return (IntVal 0)

let parse_program lexer = [parse_stmt lexer]