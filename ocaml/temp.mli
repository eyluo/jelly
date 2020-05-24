type t [@@deriving sexp, compare, hash]

val base : t
val reset : unit -> unit
val create : unit -> t

val string_of_temp : t -> string
val int_of_temp : t -> int