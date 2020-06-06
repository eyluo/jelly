(* Parser implementation.
 * Currently supports operation precedence and grouping by associativity. *)

open Core

module M = Mark
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
    | L.Op.Pow -> (3, Right)
    | L.Op.Plus | L.Op.Minus -> (1, Left)
    | L.Op.Times | L.Op.Divide -> (2, Left)
    | _ -> (100, Left)
  in

  let rec parse_expr min_prec =
    let lhs = parse_atom () in
    let result = parse_expr_prec min_prec lhs in
    result
  and parse_expr_prec min_prec lhs = 
    let mtok = L.peek lexer in
    let tok = M.obj mtok in
    match tok with
    | L.Delim | L.Eof | L.IntVal _| L.BoolVal _ -> L.drop lexer; lhs
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
        let exp = Mark.create_from_range lhs rhs (Ast.Operator (op, lhs, rhs))
        in parse_expr_prec min_prec exp
    | _ -> lhs
  and parse_atom () =
    let mtok = L.pop lexer in
    let tok = Mark.obj mtok in
    match tok with
    | L.IntVal i -> Mark.with_mark (Ast.IntVal i) mtok
    | L.BoolVal b -> Mark.with_mark (Ast.BoolVal b) mtok
    | L.Symbol s -> Mark.with_mark (Ast.Var (S.create s)) mtok
    | L.LParen ->
      let e = parse_expr 0 in
      let mtok2 = L.pop lexer in
      let tok2 = Mark.obj mtok2 in
      (match tok2 with
       | L.RParen -> e
       | _ -> 
         Err.print (Lexer.file lexer) (Lexer.fname lexer) mtok2;
         raise (ParserError "Unbalanced parentheses"))
    | _ -> 
      Err.print (Lexer.file lexer) (Lexer.fname lexer) mtok;
      raise (ParserError "Illegal atom grammar")
  in
  parse_expr 0

let rec parse_stmt lexer = 
  let mtok = L.pop lexer in
  let tok = Mark.obj mtok in
  let stmt = 
    match tok with
    | L.Symbol s -> 
      let meq = L.pop lexer in
      let eq = Mark.obj meq in
      let assign = 
        match eq with
        | L.Eq -> Ast.Assign (S.create s, parse lexer)
        | _ -> 
          Err.print (Lexer.file lexer) (Lexer.fname lexer) meq;
          raise (ParserError "var assignment: should be followed by =")
      in
      Mark.with_mark assign mtok
    | L.Return -> Mark.with_mark (Ast.Return (parse lexer)) mtok
    (* This only exists for empty-line commands. *)
    | L.Delim -> parse_stmt lexer
    | L.BoolDecl ->
      let mvar = L.peek lexer in
      let var = Mark.obj mvar in
      let decl =
        match var with
        | L.Symbol s -> Ast.Declare(Type.Bool, S.create s)
        | _ -> 
          Err.print (Lexer.file lexer) (Lexer.fname lexer) mvar;
          raise (ParserError "var declaration: keyword bool should be followed by symbol")
      in
      Mark.with_mark decl mtok
    | L.IntDecl ->
      let mvar = L.peek lexer in
      let var = Mark.obj mvar in
      let decl =
        match var with
        | L.Symbol s -> Ast.Declare(Type.Int, S.create s)
        | _ -> 
          Err.print (Lexer.file lexer) (Lexer.fname lexer) mvar;
          raise (ParserError "var declaration: keyword int should be followed by symbol")
      in
      Mark.with_mark decl mtok
    | _ -> 
      Err.print (Lexer.file lexer) (Lexer.fname lexer) mtok;
      raise (ParserError "statement does not begin with assignment or return")
  in stmt

let parse_program lexer =
  let rec parse_program' prog = 
    let mtok = L.peek lexer in
    let tok = Mark.obj mtok in
    match tok with
    | L.Eof -> prog
    | _ -> let stmt = parse_stmt lexer in parse_program' (List.append prog [stmt])
  in
  parse_program' []