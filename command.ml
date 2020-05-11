
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

(** [price_command command] parses the inputted price command into a command 
    and a tail list, raising an exception if the input format is not correct. *)
let price_command command = 
  match command with 
  | [] -> raise Empty
  | _ -> if List.length command != 2 then raise Malformed else Price command

(** [buy_command command] parses the inputted buy command into a command 
    and a tail list, raising an exception if the input format is not correct. *)
let buy_command command = 
  match command with 
  | [] -> raise (Empty)
  | _ -> if List.length command != 2 then raise Malformed else Buy command

(** [sell_command command] parses the inputted sell command into a command 
    and a tail list, raising an exception if the input format is not correct. *)
let sell_command command = 
  match command with 
  | [] -> raise (Empty)
  | _ -> if List.length command != 2 then raise Malformed else Sell command

(** [threshold_command command] parses the inputted threshold command into a 
    command and a tail list, raising an exception if the input format 
    is not correct. *)
let threshold_command command = 
  match command with 
  | [] -> raise Empty
  | _ -> if List.length command != 4 then raise Malformed else Threshold command

(** [mean_reversion_command command] parses the inputted mean_reversion command 
    into a command and a tail list, raising an exception if the 
    input format is not correct. *)
let mean_reversion_command command = 
  match command with 
  | [] -> raise Empty
  | _ -> 
    if List.length command != 4 then raise Malformed else Mean_reversion command

(** [profit_command command] parses the inputted profit command 
    into a command and a tail list, raising an exception if the 
    input format is not correct. *)
let profit_command command = 
  match command with 
  | [] -> raise Empty
  | _ -> 
    if List.length command != 4 then raise Malformed else Profit command

(** [quit_command command] returns a Quit command and raises exception Malformed
    if Quit is followed by another input. *)
let quit_command command = 
  match command with
  | [] -> Quit
  | h::t -> raise Malformed

(** [info_command command] returns a Info command and raises exception Malformed
    if Info is followed by another input. *)
let info_command command = 
  match command with 
  | [] -> Info
  | _ -> raise Malformed

(** [portfolio_command command] returns a Portfolio command and raises 
    exception Malformed if Portfolio is followed by another input. *)
let portfolio_command command = 
  match command with 
  | [] -> Portfolio
  | _ -> raise Malformed

(** [history_command command] returns a History command and raises 
    exception Malformed if History is followed by another input. *)
let history_command command = 
  match command with 
  | [] -> History
  | _ -> raise Malformed

(** [clear_command command] returns a Clear command and raises 
    exception Malformed if History is followed by another input. *)
let clear_command command = 
  match command with 
  | [] -> Clear
  | _ -> raise Malformed

(** [parse str] checks [str] for a valid command and raises Malformed if one
    is not found.  *)
let parse str =
  match (String.split_on_char ' ' str |> List.filter (fun x -> x <> "") ) with
  | [] -> raise Empty
  | h::t -> if (h = "price") then (price_command t) else 
    if (h = "buy") then (buy_command t) else if (h = "sell") 
    then (sell_command t) else if (h = "threshold") then 
      (threshold_command t) else if (h = "means") then 
      (mean_reversion_command t) else if (h = "profit") then 
      (profit_command t) else if (h = "info") then 
      (info_command t) else if (h = "portfolio") then (portfolio_command t) else
    if (h = "clear") then (clear_command t) else if (h = "quit") then 
      (quit_command t) else if (h = "history") then 
      (history_command t) else raise Malformed
