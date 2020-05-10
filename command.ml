
exception Empty
exception Malformed

type command = 
  | Info
  | Price of string list
  | Buy of string list
  | Sell of string list
  | Threshold of string list
  | Portfolio
  | History
  | Stop
  | Quit

let price_command command = 
  match command with 
  | [] -> raise Empty
  | _ -> if List.length command != 2 then raise Malformed else Price command

let buy_command command = 
  match command with 
  | [] -> raise (Empty)
  | _ -> if List.length command != 2 then raise Malformed else Buy command

let sell_command command = 
  match command with 
  | [] -> raise (Empty)
  | _ -> if List.length command != 2 then raise Malformed else Sell command

let threshold_command command = 
  match command with 
  | [] -> raise Empty
  | _ -> if List.length command != 4 then raise Malformed else Threshold command

let quit_command command = 
  match command with
  | [] -> Quit
  | h::t -> raise Malformed

let info_command command = 
  match command with 
  | [] -> Info
  | _ -> raise Malformed

let portfolio_command command = 
  match command with 
  | [] -> Portfolio
  | _ -> raise Malformed

let history_command command = 
  match command with 
  | [] -> History
  | _ -> raise Malformed

let stop_command command = 
  Stop

let parse str =
  match (String.split_on_char ' ' str |> List.filter (fun x -> x <> "") ) with
  | [] -> raise Empty
  | h::t -> if (h = "price") then (price_command t) else 
    if (h = "buy") then (buy_command t) else if (h = "sell") 
    then (sell_command t) else if (h = "threshold") then 
      (threshold_command t) else if (h = "info") then 
      (info_command t) else if (h = "portfolio") then (portfolio_command t) else
    if (h = "stop") then (stop_command t) else if (h = "quit") then 
      (quit_command t) else if (h = "history") then 
      (history_command t) else raise Malformed
