open Engine
open Command

let message = 
  "\n\n Type 'price (stock ticker) (volume)' to get the price for a stock. \n 
  I.e. 'price AAPL 2' will give you the price of 2 shares of Apple stock. \n
  Type 'buy (stock ticker) (volume)' to buy a specified number of shares of \n
  a stock. I.e. 'buy AAPL 30' will buy 30 shares of Apple stock. \n
  Type '"

let rec run () =
  print_endline message;
  let input = read_line () in 
  match Command.parse input with 
  | Price stock -> 
    (Engine.get_price (List.nth stock 0) (int_of_string (List.nth stock 1))) 
    |> string_of_float |> print_string;
    run ()
  | Info -> run ()
  | Buy stock -> 
    (Engine.buy (List.nth stock 0) (int_of_string (List.nth stock 1)));
    run () 
  | Sell stock ->
    (Engine.sell (List.nth stock 0) (int_of_string (List.nth stock 1)));
    run ()
  | Threshold prices -> run ()
  | Portfolio -> run ()
  | Stop -> run ()
  | Quit -> 
    print_endline "You are now exiting the system.";
    Stdlib.exit 0

let play_game =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Stock Trading Platform.\n");
  print_string  "> ";
  run ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game

(* Execute the game engine. *)
let () = main ()