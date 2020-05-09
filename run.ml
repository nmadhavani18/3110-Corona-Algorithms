open Engine
open Command

let message = 
  "\nType 'price (stock ticker) (volume)' to get the price for a stock.\n 
I.e. 'price AAPL 2' will give you the price of 2 shares of Apple stock.\n
Type 'buy (stock ticker) (volume)' to buy a specified number of shares of\n
a stock. I.e. 'buy AAPL 30' will buy 30 shares of Apple stock.\n
Type 'sell (stock ticker) (volume)' to sell a specified number of shares of\n
a stock. I.e. 'sell AAPL 30' will sell 30 shares of Apple stock.\n"

let rec run () =

  try
    print_string "\nType your command here. If you need help, type 'info'. To quit, type 'quit'.\n> ";
    let input = read_line () in 
    match Command.parse input with 
    | Price stock -> 
      let price = (Engine.get_price (List.nth stock 0) (int_of_string (List.nth stock 1))) in
      print_string "Price of "; print_string (List.nth stock 1); print_string " ";
      print_string (List.nth stock 0); print_string " stock is: ";
      price |> string_of_float |> ANSITerminal.(print_string [green]);
      run ()
    | Info -> ANSITerminal.(print_string [green]
                              message); run ()
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

  with _ -> print_endline "Invalid command. Try again."; run ()

let play_game =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Stock Trading Platform.\n");
  run ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game

(* Execute the game engine. *)
let () = main ()