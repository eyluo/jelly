exception AssemblyError of string

type t

val string_of_asm : Ir.t -> t
val asm_to_file : t -> unit