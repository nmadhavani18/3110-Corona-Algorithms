
val get_price : string -> int -> float

val get_url : string -> string

val get_file : string -> string

val file : string -> string

val save_file : string -> unit

val buy : string -> int -> Base.unit

val sell : string -> int -> Base.unit

val data_lines : string -> (string * string * int) list

val data_processor : (string * string * int) list -> (string * int) list -> (string * int) list

val compare : float -> float -> bool