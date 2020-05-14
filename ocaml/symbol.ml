open Core

type t = string [@@deriving sexp, compare, hash]

let create s = s
let string_of_symbol sym = sym