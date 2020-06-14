exception AssemblyError of string

type t

val to_string : Ir3.t -> t
val asm_to_file : string -> t -> unit
