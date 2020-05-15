type t [@@deriving sexp, compare, hash]

val create : unit -> t

val string_of_temp : t -> string