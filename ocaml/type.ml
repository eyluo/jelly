type t = Int | Bool

let string_of_type ty =
  match ty with
  | Int -> "int"
  | Bool -> "bool"