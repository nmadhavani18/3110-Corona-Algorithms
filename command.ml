
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

let check_valid_price_command command = 
  match command with 
  | [] -> raise Empty
  | _ -> if List.length command != 2 then raise Malformed else Price command

let check_valid_buy_command command = 
  match command with 
  | [] -> raise (Empty)
  | _ -> if List.length command != 2 then raise Malformed else Buy command

let check_valid_sell_command command = 
  match command with 
  | [] -> raise (Empty)
  | _ -> if List.length command != 2 then raise Malformed else Sell command

let check_valid_threshold_command command = 
  match command with 
  | [] -> raise Empty
  | _ -> Threshold command

let check_valid_quit_command command = 
  match command with
  | [] -> Quit
  | h::t -> raise Malformed

let info_command command = 
  Info

let portfolio_command command = 
  match command with 
  | [] -> Portfolio
  | _ -> raise Malformed

let stop_command command = 
  Stop

let parse str =
  match (String.split_on_char ' ' str |> List.filter (fun x -> x <> "") ) with
  | [] -> raise Empty
  | h::t -> if (h = "price") then (check_valid_price_command t) else 
    if (h = "buy") then (check_valid_buy_command t) else if (h = "sell") 
    then (check_valid_sell_command t) else if (h = "threshold") then 
      (check_valid_threshold_command t) else if (h = "info") then 
      (info_command t) else if (h = "portfolio") then (portfolio_command t) else
    if (h = "stop") then (stop_command t) else if (h = "quit") then 
      (check_valid_quit_command t) else raise Malformed
