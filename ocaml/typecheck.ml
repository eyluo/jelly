open Core

exception TypeError of string

let builtin_types = Hashtbl.of_alist_exn (module Lexer.Op) [
    (Lexer.Op.Pow, (Type.Int, Type.Int, Type.Int));
    (Lexer.Op.Plus, (Type.Int, Type.Int, Type.Int));
    (Lexer.Op.Minus, (Type.Int, Type.Int, Type.Int));
    (Lexer.Op.Times, (Type.Int, Type.Int, Type.Int));
    (Lexer.Op.Divide, (Type.Int, Type.Int, Type.Int));

    (Lexer.Op.CompEq, (Type.Alpha, Type.Alpha, Type.Bool));
    (Lexer.Op.Neq, (Type.Alpha, Type.Alpha, Type.Bool));
    (Lexer.Op.Less, (Type.Int, Type.Int, Type.Bool));
    (Lexer.Op.Leq, (Type.Int, Type.Int, Type.Bool));
    (Lexer.Op.Greater, (Type.Int, Type.Int, Type.Bool));
    (Lexer.Op.Geq, (Type.Int, Type.Int, Type.Bool));
    (Lexer.Op.BoolAnd, (Type.Bool, Type.Bool, Type.Bool));
    (Lexer.Op.BoolOr, (Type.Bool, Type.Bool, Type.Bool));
  ]

let typecheck lexer program =
  (* TODO: it feels wrong to reset temp's here. Maybe there's a way to not have
   * to worry about this at all. *)
  Temp.reset ();
  let seen = Hashtbl.create (module Symbol) in
  (* Ensures that an expression contains symbols we've seen so far. *)
  let valid_exp exp =
    let rec valid_exp' exp = 
      let exp' = Mark.obj exp in
      match exp' with
      | Ast.Var s -> 
        let result = 
          match Hashtbl.find seen s with
          | Some ty -> (true, ty)
          | None -> (false, Type.Alpha)
        in result
      | Ast.Operator (op, e1, e2) -> 
        let result = 
          let (v1, t1) = valid_exp' e1 in
          let (v2, t2) = valid_exp' e2 in
          let (op_ty1, op_ty2, return_ty) = Hashtbl.find_exn builtin_types op in
          if (v1 && v2) && (Type.equal t1 op_ty1) && (Type.equal t2 op_ty2) then
            (true, return_ty)
          else (false, Type.Alpha)
        in result
      | Ast.BoolVal _ -> (true, Type.Bool)
      | Ast.IntVal _ -> (true, Type.Int)
    in
    valid_exp' exp
  in
  let rec typecheck' program =
    match program with
    (* Gonna assume an empty program is "correct." *)
    | [] -> ()
    | s :: [] -> 
      let s' = Mark.obj s in
      (match s' with
       | Ast.Return exp -> 
         (* TODO: when we have actual functions, we will need to verify these 
          * return types. *)
         let (is_valid, (_ : Type.t)) = valid_exp exp in
         if is_valid then () 
         else 
           let () = Err.print (Lexer.file lexer) (Lexer.fname lexer) s in
           raise (TypeError "unrecognized symbol in return statement")
       | _ -> raise (TypeError "last statement is not a return"))
    | s :: ss ->
      let s' = Mark.obj s in
      let () = 
        match s' with
        | Ast.Return _ -> 
          Err.print (Lexer.file lexer) (Lexer.fname lexer) s;
          raise (TypeError "return statement in middle of program")
        | Ast.Assign (sym, exp) -> 
          let ty_opt = Hashtbl.find seen sym in
          let () = 
            match ty_opt with
            | None -> raise (TypeError "variable does not exist")
            | Some ty ->
              let (is_valid, return_ty) = valid_exp exp in
              if not is_valid then 
                raise (TypeError "expression does not typecheck")
              else if not (Type.equal ty return_ty) then 
                raise (TypeError "type of variable and expression do not match")
              else typecheck' ss
          in ()
        | Ast.Declare (ty, sym) ->
          (* let add_check = Hashtbl.add seen ~key:sym ~data:ty in
             let () =  
             match add_check with
             | `Duplicate -> raise (TypeError "redeclaration of existing variable")
             | `Ok -> ()
             in () *)
          let is_seen = Hashtbl.mem seen sym in
          if is_seen then raise (TypeError "redeclaration of existing variable")
          else Hashtbl.set seen ~key:sym ~data:ty; typecheck' ss
      in ()
  in
  typecheck' program