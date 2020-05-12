(* Lexer interface. *)

type op = Pow | Plus | Minus | Times | Divide

type token =
    | IntVal of int
    | Operator of op
    | LParen
    | RParen
    | Eof

type lexer

val string_of_token : token -> string

val create : string -> lexer
val pop : lexer -> token
val peek : lexer -> token
val drop : lexer -> unit