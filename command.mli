exception Empty
exception Malformed

type command = 
  | Info
  | Price of string list
  | Buy of string list
  | Sell of string list
  | Threshold of string list
  | Mean_reversion of string list
  | Profit of string list
  | Portfolio
  | History
  | Clear
  | Quit

val parse : string -> command