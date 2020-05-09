type stock = string
type volume = int
type cost = float

exception Empty
exception Malformed

type command = 
  | Info
  | Price of stock
  | Buy of (stock * volume)
  | Sell of (stock * volume)
  | Threshold of (cost * cost)
  | Portfolio

let check_valid_price_command command = 
  match command with 
  | Price "" -> raise (Empty)
  | _ -> command

let check_valid_buy_command command = 
  match command with 
  | Buy ("", 0) -> raise (Empty)
  | _ -> command

let check_valid_sell_command command = 
  match command with 
  | Sell ("", 0) -> raise (Empty)
  | _ -> command

