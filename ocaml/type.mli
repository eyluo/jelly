type t =
  | Int
  | Bool
  | Alpha

val to_string : t -> string
val equal : t -> t -> bool
