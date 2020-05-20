(* Lexer interface. *)

exception InvalidToken of char
exception InvalidInt of string

type op = Pow | Plus | Minus | Times | Divide

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

type t

val string_of_op : op -> string
val string_of_token : token -> string

val create : string -> t
val pop : t -> token
val peek : t -> token
val drop : t -> unit