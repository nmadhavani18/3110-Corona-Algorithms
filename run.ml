open Engine
open Command

(** let port_helper1 line = try Some (input_line line) with End_of_file -> None

    let port_printer filename = 
    let rec port_print_helper line acc = 
    begin match (port_helper1 line) with 
      | None -> List.rev acc
      | Some a -> port_print_helper line (a :: acc)end in 
    port_print_helper (open_in filename) [] *)

let rec data_printer lst = 
  match lst with 
  | [] -> []
  | (stock,vol)::t -> stock::string_of_int vol::"shares"::data_printer t

let message = 
  "\nType 'price (stock ticker) (volume)' to get the price for a stock.\n 
I.e. 'price AAPL 2' will give you the price of 2 shares of Apple stock.\n
Type 'buy (stock ticker) (volume)' to buy a specified number of shares of a stock.\n
I.e. 'buy AAPL 30' will buy 30 shares of Apple stock.\n
Type 'sell (stock ticker) (volume)' to sell a specified number of shares of a stock.\n
I.e. 'sell AAPL 30' will sell 30 shares of Apple stock.\n
Type 'portfolio' to see all of the transactions you have made.\n"

let rec run () =
  print_string "\nType your command here. If you need help, type 'info'. To quit, type 'quit'.\n> ";
  let input = read_line () in 
  match Command.parse input with 
  | Price stock -> 
    let price = (Engine.get_price (List.nth stock 0) (int_of_string (List.nth stock 1))) in
    print_string "\nPrice of "; 
    print_string (List.nth stock 1); 
    print_string " ";
    print_string (List.nth stock 0); 
    print_string " shares is: ";
    price 
    |> string_of_float 
    |> ANSITerminal.(print_string [green]);
    print_string "\n";
    run ()
  | Info -> ANSITerminal.(print_string [red]
                            message); run ()
  | Buy stock -> 
    (Engine.buy (List.nth stock 0) (int_of_string (List.nth stock 1)));
    print_string "\n"; 
    print_string (List.nth stock 1); 
    print_string " "; 
    print_string (List.nth stock 0); 
    print_string " shares bought!\n";
    run () 
  | Sell stock ->
    (Engine.sell (List.nth stock 0) (int_of_string (List.nth stock 1)));
    print_string "\n"; 
    print_string (List.nth stock 1); 
    print_string " "; 
    print_string (List.nth stock 0); 
    print_string " shares sold!\n";
    run ()
  | Threshold prices -> run ()
  | Portfolio -> 
    print_string "\n";
    Engine.data_processor (Engine.data_lines "transactions.txt") [] |>
    data_printer |> String.concat " " |> print_string;
    print_string "\n";
    run ()
  | Stop -> run ()
  | Quit -> 
    print_endline "You are now exiting the system.";
    Stdlib.exit 0
  | exception Malformed -> print_endline "Invalid command. Try again"; 
    run ()
  | exception Empty -> print_endline "You entered an empty command.";
    run () 

let play_game =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Stock Trading Platform.\n");
  run ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game

(* Execute the game engine. *)
let () = main ()