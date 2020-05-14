type t [@@deriving sexp, compare, hash]

val create : string -> t
val string_of_symbol : t -> string