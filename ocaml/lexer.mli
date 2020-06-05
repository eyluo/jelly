(* Lexer interface. *)

exception InvalidToken of string
exception InvalidInt of string

type op = 
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
  | BoolOr

type token =
  | Symbol of string
  | IntDecl
  | IntVal of int
  | BoolDecl
  | BoolVal of bool
  | Operator of op
  | Eq
  | LParen
  | RParen
  | Delim
  | Return
  | Eof

type mtoken = token Mark.t

type t

val file : t -> string
val fname : t -> string

val string_of_op : op -> string
val string_of_token : mtoken -> string

val create : string -> t
val pop : t -> mtoken
val peek : t -> mtoken
val drop : t -> unit