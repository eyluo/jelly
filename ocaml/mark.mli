type 'a t
type pos = int * int

val create : 'a -> pos -> pos -> 'a t
val obj : 'a t -> 'a
val map : 'a t -> f:('a -> 'b) -> 'b t
val start : 'a t -> pos
val stop : 'a t -> pos
val with_mark : 'a -> 'b t -> 'a t
val create_from_range : 'a t -> 'b t -> 'c -> 'c t
