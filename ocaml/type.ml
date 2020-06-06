type t = Int | Bool | Alpha

let string_of_type ty =
  match ty with
  | Int -> "int"
  | Bool -> "bool"
  | Alpha -> "'a"