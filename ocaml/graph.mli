type t
type edges

val create : unit -> t
val add_edge : t -> Temp.t -> Temp.t -> unit

val mcs : t -> Temp.t list
(* val color : t -> Temp.t list -> (Temp.t, int) Hashtbl.t *)
val string_of_graph : t -> string
val string_of_order : Temp.t list -> string