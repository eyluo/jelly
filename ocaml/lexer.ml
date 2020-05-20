(* Lexer implementation.
 * Currently supports lexing basic arithmetic. *)

open Core

(* Defines an exception if we encounter an invalid token. *)
exception InvalidToken of char
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

(* Defines a custom lexer type. *)
type t = {
  file : string;                  (* File contents *)
  length : int;                   (* Length of file contents *)
  pos : int ref;                  (* Index into file contents *)
  saved_tok : token option ref    (* Last token seen from peek *)
}

(* Stolen from my boy Henry. This takes a file name and returns the string
 * representation of the file. *)
let slurp fname = In_channel.with_file ~binary:false fname 
    ~f:(fun ch -> In_channel.input_all ch)


let string_of_op op =
  match op with
  | Pow -> "^"
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"

(* For debugging: converts a token into a user-readable string. *)
let string_of_token tok =
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

(* Creates a new lexer. *)
let create fname = 
  let file = slurp fname in
  let result = {
    file = file;
    length = String.length file;
    pos = ref 0;
    saved_tok = ref None;
  } in result

(* Fetches the next token from the lexer. *)
let rec next_token lxr = 
  let result = 
    if !(lxr.pos) = lxr.length then Eof
    else 
      let ch = String.get lxr.file !(lxr.pos) in
      let token = 
        match ch with
        (* Whitespace produces no token, so skip it. *)
        | ' ' | '\n' | '\t' | '\r' -> lxr.pos := !(lxr.pos) + 1; next_token lxr
        (* If you encounter a digit, gobble up all of the subsequent numbers *)
        | '0' .. '9' -> 
          let rec parse_digits num_str = 
            if !(lxr.pos) = lxr.length then num_str
            else
              let digit = String.get lxr.file !(lxr.pos) in
              let result = 
                match digit with
                | '0' .. '9' -> 
                  lxr.pos := !(lxr.pos) + 1; 
                  parse_digits (num_str ^ Char.to_string digit)
                (* Ints should not have letters in them. *)
                | 'A' .. 'Z' | 'a' .. 'z' -> raise (InvalidInt num_str)
                | _ -> num_str
              in result
          in 
          let num = int_of_string (parse_digits "")
          in IntVal num
        (* If you encounter a letter, interpret as a symbol or keyword. *)
        | 'A' .. 'Z' | 'a' .. 'z' ->
          let rec parse_symbol str =
            if !(lxr.pos) = lxr.length then str
            else
              let c = String.get lxr.file !(lxr.pos) in
              let result =
                match c with
                | 'A' .. 'Z' | 'a' .. 'z'
                | '0' .. '9' -> 
                  lxr.pos := !(lxr.pos) + 1; 
                  parse_symbol (str ^ Char.to_string c)
                | _ -> str
              in result
          in
          (* Handle return keyword *)
          let sym = parse_symbol "" in 
          (match sym with
           (* HERE: I suspect this is where we would extend to
            * support more keywords in the future. *)
           | "return" -> Return
           | _ -> Symbol sym)
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
            | _ -> raise (InvalidToken ch)
          in lxr.pos := !(lxr.pos) + 1; t
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
let drop lxr = let (_ : token) = pop lxr in ()