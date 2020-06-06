open Core

exception TypeError of string

let typecheck lexer program =
  (* TODO: it feels wrong to reset temp's here. Maybe there's a way to not have
   * to worry about this at all.
   * TODO: scan program, keep track of types associated with each symbol
   * declaration.
   * - Ensure that variables are declared before use.
   * - Ensure that a variable is not redefined.
   * - Ensure that functions respect definitions.
       Particularly for this point, I think there should just be some hash_tbl
       that maps function declarations (because I don't think it makes sense for
       the AST to also annotate types). When support is added for custom 
       functions, the typechecker should identify function declarations to put
       into the hash_tbl. *)
  Temp.reset ();
  let seen = Hash_set.create (module Symbol) in
  (* Ensures that an expression contains symbols we've seen so far. *)
  let rec valid_exp exp =
    let exp' = Mark.obj exp in
    match exp' with
    | Ast.Var s -> Hash_set.mem seen s
    | Ast.Operator (_, e1, e2) -> (valid_exp e1) && (valid_exp e2)
    | _ -> true
  in
  let rec typecheck' program =
    match program with
    (* Gonna assume an empty program is "correct." *)
    | [] -> ()
    | s :: [] -> 
      let s' = Mark.obj s in
      (match s' with
       | Ast.Return exp -> 
         if valid_exp exp then () 
         else 
           let () = Err.print (Lexer.file lexer) (Lexer.fname lexer) s in
           raise (TypeError "unrecognized symbol in return statement")
       | _ -> raise (TypeError "last statement is not a return"))
    | s :: ss ->
      let s' = Mark.obj s in
      (match s' with
       | Ast.Return _ -> 
         Err.print (Lexer.file lexer) (Lexer.fname lexer) s;
         raise (TypeError "return statement in middle of program")
       | Ast.Assign (str, exp) -> 
         if valid_exp exp then 
           let () = Hash_set.add seen str in typecheck' ss 
         else 
           let () = Err.print (Lexer.file lexer) (Lexer.fname lexer) s in
           raise (TypeError "unrecognized symbol in assignment statement"))
  in
  typecheck' program