(* Lexer implementation.
 * Currently supports lexing basic arithmetic. *)

open Core

module M = Mark

(* Defines an exception if we encounter an invalid token. *)
exception InvalidToken of string
exception InvalidInt of string

(* Defines the legal operations. *)
module Op = struct
  type t = 
    | Pow 
    | Plus 
    | Minus 
    | Times 
    | Divide

    | CompEq
    | Neq
    | Less
    | Leq
    | Greater
    | Geq
    | BoolAnd
    | BoolOr [@@deriving sexp, compare, hash]
end

(* Defines legal tokens. *)
type token =
  | Symbol of string
  | IntDecl
  | IntVal of int
  | BoolDecl
  | BoolVal of bool
  | Operator of Op.t
  | Eq
  | LParen
  | RParen
  | Delim
  | Return
  | Eof

type mtoken = token M.t

(* Defines a custom lexer type. *)
type t = {
  fname : string;                   (* File name *)
  file : string;                    (* File contents *)
  length : int;                     (* Length of file contents *)
  pos : M.pos ref;                  (* Row, col position *)
  idx : int ref;                    (* Index into file contents *)
  saved_tok : mtoken option ref     (* Last token seen from peek *)
}

(* Stolen from my boy Henry. This takes a file name and returns the string
 * representation of the file. *)
let slurp fname = In_channel.with_file ~binary:false fname 
    ~f:(fun ch -> In_channel.input_all ch)

let file lexer = lexer.file
let fname lexer = lexer.fname

let string_of_op op =
  match op with
  | Op.Pow -> "^"
  | Op.Plus -> "+"
  | Op.Minus -> "-"
  | Op.Times -> "*"
  | Op.Divide -> "/"
  | Op.CompEq -> "=="
  | Op.Neq -> "!="
  | Op.Less -> "<"
  | Op.Leq -> "<="
  | Op.Greater -> ">"
  | Op.Geq -> ">="
  | Op.BoolAnd -> "&&"
  | Op.BoolOr -> "||"


(* For debugging: converts a token into a user-readable string. *)
let string_of_token mtok =
  let tok = M.obj mtok in
  let tok_str = 
    match tok with
    | Symbol s -> "SYM " ^ s
    | IntDecl -> "int"
    | IntVal i -> string_of_int i
    | BoolDecl -> "bool"
    | BoolVal b -> string_of_bool b
    | Operator op -> string_of_op op
    | Eq -> "="
    | LParen -> "("
    | RParen -> ")"
    | Delim -> ";"
    | Return -> "return"
    | Eof -> "EOF"
  in
  let (r1, c1) = M.start mtok in
  let (r2, c2) = M.stop mtok in
  tok_str ^ " (" ^ string_of_int r1 ^ ", " ^ string_of_int c1 ^ ")" ^ ", (" ^ string_of_int r2 ^ ", " ^ string_of_int c2 ^ ")"

(* Creates a new lexer. *)
let create fname = 
  let file = slurp fname in
  let result = {
    fname = fname;
    file = file;
    length = String.length file;
    pos = ref (1, 1);
    idx = ref 0;
    saved_tok = ref None;
  } in result

(* Fetches the next token from the lexer. *)
let rec next_token lxr = 
  let invalid_loc () =
    let r, c = !(lxr.pos) in
    Printf.sprintf "%s: line %d, character %d" lxr.fname r c
  in
  let lxr_print_err = Err.print lxr.file lxr.fname in
  let peek_char () =
    if !(lxr.idx) = lxr.length then None
    else let ch = String.get lxr.file !(lxr.idx) in Some ch
  in
  let drop_char () = 
    let incr_pos r c = lxr.idx := !(lxr.idx) + 1; lxr.pos := (r, c+1); in
    let incr_pos_new_row r = lxr.idx := !(lxr.idx) + 1; lxr.pos := (r+1, 1); in
    if !(lxr.idx) = lxr.length then ()
    else
      let ch = String.get lxr.file !(lxr.idx) in
      let (r, c) = !(lxr.pos) in
      match ch with
      | '\n' -> incr_pos_new_row r
      | _ -> incr_pos r c
  in
  let result = 
    match peek_char () with
    | None -> M.create Eof !(lxr.pos) !(lxr.pos)
    | Some ch ->
      let (r, c) = !(lxr.pos) in 
      let token = 
        match ch with
        | '\n' | ' ' | '\t' | '\r' -> drop_char (); next_token lxr
        (* Parsing int *)
        | '0' .. '9' -> 
          let rec parse_int num_str = 
            match peek_char () with
            | None -> num_str
            | Some ch' ->
              match ch' with 
              | '0' .. '9' -> drop_char (); parse_int (num_str ^ Char.to_string ch')
              | 'A' .. 'Z' | 'a' .. 'z' -> 
                lxr_print_err (Mark.create ch' !(lxr.pos) !(lxr.pos));
                raise (InvalidInt (invalid_loc ()))
              | _ -> num_str
          in 
          let num = int_of_string (parse_int "") in 
          M.create (IntVal num) (r,c) !(lxr.pos)
        (* Parsing symbols *)
        | 'A' .. 'Z' | 'a' .. 'z' ->
          let rec parse_symbol sym =
            match peek_char () with
            | None -> sym
            | Some ch' ->
              match ch' with
              | 'A' .. 'Z' | 'a' .. 'z' 
              | '0' .. '9' -> drop_char (); parse_symbol (sym ^ Char.to_string ch')
              | _ -> sym
          in
          let sym = parse_symbol (parse_symbol "") in
          let sym_token = 
            match sym with
            (* HERE: I suspect this is where we would extend to
             * support more keywords in the future. *)
            | "int" -> IntDecl
            | "bool" -> BoolDecl
            | "true" -> BoolVal true
            | "false" -> BoolVal false
            | "return" -> Return
            | _ -> Symbol sym
          in M.create sym_token (r,c) !(lxr.pos)
        (* Parsing operators *)
        | _ -> 
          let t = 
            drop_char ();
            let ch_opt = peek_char () in
            match ch with
            | '=' -> 
              if is_none ch_opt || not (phys_equal '=' (Option.value_exn ch_opt)) then
                Eq
              else let () = drop_char () in Operator CompEq
            | '(' -> LParen
            | ')' -> RParen
            | ';' -> Delim

            | '^' -> Operator Pow
            | '+' -> Operator Plus
            | '-' -> Operator Minus
            | '*' -> Operator Times
            | '/' -> Operator Divide

            | '>' ->
              if is_none ch_opt || not (phys_equal '=' (Option.value_exn ch_opt)) then
                Operator Greater
              else let () = drop_char () in Operator Geq
            | '<' ->
              if is_none ch_opt || not (phys_equal '=' (Option.value_exn ch_opt)) then
                Operator Less
              else let () = drop_char () in Operator Leq
            | '&' ->
              if is_none ch_opt || not (phys_equal '&' (Option.value_exn ch_opt)) then
                raise (InvalidToken (invalid_loc ()))
              else let () = drop_char () in Operator BoolAnd 
            | '|' -> 
              if is_none ch_opt || not (phys_equal '|' (Option.value_exn ch_opt)) then
                raise (InvalidToken (invalid_loc ()))
              else let () = drop_char () in Operator BoolOr 

            | _ -> 
              lxr_print_err (M.create ch (r,c) !(lxr.pos));
              raise (InvalidToken (invalid_loc ()))
          in 
          M.create t (r,c) !(lxr.pos)
      in token
  in result

(* Pops the next token from the lexer. *)
let pop lxr =
  match !(lxr.saved_tok) with
  | Some tok -> lxr.saved_tok := None; tok
  | None -> next_token lxr

(* Peek at the next token in the lexer. *)
let peek lxr =
  match !(lxr.saved_tok) with
  | Some tok -> tok
  | None -> let result = next_token lxr in lxr.saved_tok := Some result; result

(* Drops the next token from the lexer. Equivalent to pop, but it ignores the
 * token. *)
let drop lxr = let (_ : mtoken) = pop lxr in ()