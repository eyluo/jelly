(* Lexer interface. *)

exception InvalidToken of string
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

type mtoken = token Mark.t

type t

val string_of_op : op -> string
val string_of_token : mtoken -> string

val create : string -> t
val pop : t -> mtoken
val peek : t -> mtoken
val drop : t -> unit