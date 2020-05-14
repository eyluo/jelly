open Core

exception TypeError of string

let typecheck program =
  let seen = Hash_set.create (module Symbol) in
  (* Ensures that an expression contains symbols we've seen so far. *)
  let rec valid_exp exp =
    match exp with
    | Ast.Var s -> Hash_set.mem seen s
    | Ast.Operator (_, e1, e2) -> (valid_exp e1) && (valid_exp e2)
    | _ -> true
  in
  let rec typecheck' program =
    match program with
    (* Gonna assume an empty program is "correct." *)
    | [] -> ()
    | s :: [] -> 
      (match s with
       | Ast.Return exp -> if valid_exp exp then () else raise (TypeError "unrecognized symbol in return statement")
       | _ -> raise (TypeError "last statement is not a return"))
    | s :: ss ->
      (match s with
       | Ast.Return _ -> raise (TypeError "return statement in middle of program")
       | Ast.Assign (s, exp) -> 
         if valid_exp exp then 
           let () = Hash_set.add seen s in typecheck' ss 
         else raise (TypeError "unrecognized symbol in assignment statement"))
  in
  typecheck' program