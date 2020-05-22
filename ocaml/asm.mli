exception AssemblyError of string

type t

val string_of_asm : Ir3.t -> t
val asm_to_file : string -> t -> unit