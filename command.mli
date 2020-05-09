exception Empty
exception Malformed

type command = 
  | Info
  | Price of string list
  | Buy of string list
  | Sell of string list
  | Threshold of string list
  | Portfolio
  | Stop
  | Quit

val parse : string -> command