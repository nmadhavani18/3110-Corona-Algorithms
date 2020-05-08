(* Auto-generated from "transactions.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type transaction = Transactions_t.transaction = {
  action: string;
  stock: string;
  volume: int;
  price: float;
  time: string
}

val write_transaction :
  Bi_outbuf.t -> transaction -> unit
  (** Output a JSON value of type {!transaction}. *)

val string_of_transaction :
  ?len:int -> transaction -> string
  (** Serialize a value of type {!transaction}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_transaction :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> transaction
  (** Input JSON data of type {!transaction}. *)

val transaction_of_string :
  string -> transaction
  (** Deserialize JSON data of type {!transaction}. *)

