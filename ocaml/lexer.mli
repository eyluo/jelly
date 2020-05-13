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

type lexer

val string_of_token : token -> string

val create : string -> lexer
val pop : lexer -> token
val peek : lexer -> token
val drop : lexer -> unit