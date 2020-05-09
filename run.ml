open Engine
open Command

let info_helper = 
  let message = 
    "Type 'price (stock ticker) (volume)' to get the price for a stock. \n 
  I.e. 'price AAPL 2' will give you the price of 2 shares of Apple stock. \n
  Type 'buy (stock ticker) (volume)' to buy a specified number of shares of \n
  a stock. I.e. 'buy AAPL 30' will buy 30 shares of Apple stock. \n
  Type '" in 
  print_endline message

let rec run =
  let input = read_line () in 
  match Command.parse input with 
  | Price stock -> 
    (Engine.get_price (List.nth stock 0) (int_of_string (List.nth stock 1))) 
    |> string_of_float |> print_string
  | Info -> info_helper
  | Buy stock -> 
    (Engine.buy (List.nth stock 0) (int_of_string (List.nth stock 1))) 
  | Sell stock ->
    (Engine.sell (List.nth stock 0) (int_of_string (List.nth stock 1)))
  | Threshold prices -> ()
  | Portfolio -> ()
  | Stop -> ()

let play_game =
  run



(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  play_game 

(* Execute the game engine. *)
let () = main ()