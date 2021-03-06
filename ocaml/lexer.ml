(* Lexer implementation.
 * Currently supports lexing basic arithmetic. *)

open Core

module M = Mark

(* Defines an exception if we encounter an invalid token. *)
exception InvalidToken of string
exception InvalidInt of string

(* Defines the legal operations. *)
type op = Pow | Plus | Minus | Times | Divide

(* Defines legal tokens. *)
type token =
  | Symbol of string
  | IntVal of int
  | Operator of op
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
  | Pow -> "^"
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"

(* For debugging: converts a token into a user-readable string. *)
let string_of_token mtok =
  let tok = M.obj mtok in
  let tok_str = 
    match tok with
    | Symbol s -> "SYM " ^ s
    | IntVal i -> "" ^ string_of_int i
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
  let incr_pos r c = lxr.idx := !(lxr.idx) + 1; lxr.pos := (r, c+1); in
  let incr_pos_new_row r = lxr.idx := !(lxr.idx) + 1; lxr.pos := (r+1, 1); in
  let get_ch () = String.get lxr.file !(lxr.idx) in
  let lxr_print_err = Err.print lxr.file lxr.fname in
  let result = 
    if !(lxr.idx) = lxr.length then M.create Eof !(lxr.pos) !(lxr.pos)
    else 
      let ch = get_ch () in
      let (r, c) = !(lxr.pos) in 
      let token = 
        match ch with
        (* Whitespace produces no token, so skip it. *)
        | '\n' -> 
          incr_pos_new_row r;
          next_token lxr
        | ' ' | '\t' | '\r' -> 
          incr_pos r c; 
          next_token lxr
        (* If you encounter a digit, gobble up all of the subsequent numbers *)
        | '0' .. '9' -> 
          let rec parse_digits num_str = 
            if !(lxr.idx) = lxr.length then num_str
            else
              let (r, c) = !(lxr.pos) in 
              let digit = get_ch () in
              let result = 
                match digit with
                | '0' .. '9' -> 
                  incr_pos r c;
                  parse_digits (num_str ^ Char.to_string digit)
                (* Ints should not have letters in them. *)
                | 'A' .. 'Z' | 'a' .. 'z' -> 
                  lxr_print_err (Mark.create digit !(lxr.pos) !(lxr.pos));
                  raise (InvalidInt (invalid_loc ()))
                | _ -> num_str
              in result
          in 
          let num = int_of_string (parse_digits "")
          in M.create (IntVal num) (r,c) !(lxr.pos)
        (* If you encounter a letter, interpret as a symbol or keyword. *)
        | 'A' .. 'Z' | 'a' .. 'z' ->
          let rec parse_symbol str =
            if !(lxr.idx) = lxr.length then str
            else
              let (r, c) = !(lxr.pos) in 
              let ch' = get_ch () in
              let result =
                match ch' with
                | 'A' .. 'Z' | 'a' .. 'z'
                | '0' .. '9' -> 
                  incr_pos r c;
                  parse_symbol (str ^ Char.to_string ch')
                | _ -> str
              in result
          in
          (* Handle return keyword *)
          let sym = parse_symbol "" in 
          let kwd = 
            (match sym with
             (* HERE: I suspect this is where we would extend to
               * support more keywords in the future. *)
             | "return" -> Return
             | _ -> Symbol sym)
          in M.create kwd (r,c) !(lxr.pos) 
        | _ -> 
          let t = 
            match ch with
            | '=' -> Eq
            | ';' -> Delim
            | '^' -> Operator Pow
            | '+' -> Operator Plus
            | '-' -> Operator Minus
            | '*' -> Operator Times
            | '/' -> Operator Divide
            | '(' -> LParen
            | ')' -> RParen
            | _ -> 
              lxr_print_err (M.create ch (r,c) !(lxr.pos));
              raise (InvalidToken (invalid_loc ()))
          in 
          incr_pos r c;
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