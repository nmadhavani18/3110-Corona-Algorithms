(** 
   Engine for performing trades. 
*)


val get_price : string -> int -> float

val get_url : string -> string

val get_NYSE_url : string -> string

val get_file : string -> string

val file : string -> string

val save_file : string -> unit

val buy : string -> int -> Base.unit

val sell : string -> int -> Base.unit

val record : string -> string -> int -> float -> string -> string

val line_read : in_channel -> string option

val data_lines : string -> (string * string * int * float) list

val data_processor : (string * string * int * float) list -> 
  (string * int) list -> (string * int) list

val profit_calc : (string * string * int * float) list ->
  (string * float) list -> (string * float) list

val compare : float -> float -> bool

val shares_search : string -> (string * int) list -> int

val profit_search : string -> (string * float) list -> float