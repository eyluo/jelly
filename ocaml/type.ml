open Core

type t = Int | Bool | Alpha

let string_of_type ty =
  match ty with
  | Int -> "int"
  | Bool -> "bool"
  | Alpha -> "'a"

(* TODO: equal doesn't _really_ work because it doesn't account for changing the
 * remainder of alpha types. *)
let equal t1 t2 = 
  match (t1, t2) with
  | (Alpha, _) -> true
  | (_, Alpha) -> true
  | _ -> phys_equal t1 t2