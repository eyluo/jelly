(* Parser implementation.
 * Currently supports operation precedence and grouping by associativity. *)

open Core

module L = Lexer
module S = Symbol

exception ParserError of string

(* Defines operation associativity. *)
type assoc = Left | Right

(* 
 * atom : int | symbol | lparen * exp * lparen
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
    | L.Delim | L.Eof | L.IntVal _ -> L.drop lexer; lhs
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
    | L.Symbol s -> Ast.Var (S.to_sym s)
    | L.LParen ->
      let e = parse_expr 0 in
      let tok2 = L.pop lexer in
      (match tok2 with
       | L.RParen -> e
       | _ -> raise (ParserError "Unbalanced parentheses"))
    | _ -> raise (ParserError "Illegal atom grammar")
  in
  parse_expr 0

let rec parse_stmt lexer = 
  let tok = L.pop lexer in
  match tok with
  | L.Symbol s -> 
    let eq = L.pop lexer in
    (match eq with
     | L.Eq -> Ast.Assign (S.to_sym s, parse lexer)
     | _ -> raise (ParserError "var assignment: should be followed by ="))
  | L.Return -> Ast.Return (parse lexer)
  (* This only exists for empty-line commands. *)
  | L.Delim -> parse_stmt lexer
  | _ -> raise (ParserError "statement does not begin with assignment or return")

let parse_program lexer =
  let rec parse_program' prog = 
    let tok = L.peek lexer in
    match tok with
    | L.Eof -> prog
    | _ -> let stmt = parse_stmt lexer in parse_program' (List.append prog [stmt])
  in
  parse_program' []