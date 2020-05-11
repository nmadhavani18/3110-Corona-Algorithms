open Engine
open Command
open Simple_threshold
open Mean_reversion
open Profit

(** [overwrite filename str] replaces the lines in [file] with 
    the string [str].*)
let overwrite filename str = 
  let a = open_out filename in
  output_string a str;
  close_out a

(** [history_printer filename] reads lines from a file and converts them into
    a string list for printing. *)
let history_printer filename = 
  let rec history_print_helper line acc = 
    begin match (Engine.line_read line) with 
      | None -> List.rev acc
      | Some a -> history_print_helper line (a :: acc)end in 
  history_print_helper (open_in filename) [] 

(** [data_printer lst] takes a tuple list of stocks and share amounts and 
    converts it to a string list for printing. *)
let rec data_printer lst = 
  match lst with 
  | [] -> []
  | (stock,vol)::t -> stock::string_of_int vol::"shares"::data_printer t

(** [price_helper stock] returns the price of a specified number of shares
    of a stock based on the inputted price command. *)
let price_helper stock = 
  try
    let stock_name = List.nth stock 0 in
    let stock_volume = (int_of_string (List.nth stock 1)) in 
    if stock_volume >= 0 then 
      (let price = (Engine.get_price stock_name stock_volume) in
       print_string "\nPrice of "; print_string stock_name; print_string " ";
       print_string (List.nth stock 1); print_string " shares is: ";
       price |> string_of_float |> ANSITerminal.(print_string [green]);
       print_string "\n")
    else print_endline "Bad command."
  with _ -> print_endline "\nInvalid Stock."

(** [buy_helper stock] buys a specified number of shares of 
    a stock based on the inputted buy command. *)
let buy_helper stock = 
  try
    let stock_name = List.nth stock 0 in
    let stock_volume = (int_of_string (List.nth stock 1)) in 
    if stock_volume >= 0 then 
      (Engine.buy stock_name stock_volume;
       print_string "\n"; 
       print_string "Your transaction was successful!\n")
    else print_endline "Bad command."
  with _ -> print_endline "\nInvalid Stock."

(** [sell_helper stock] sells a specified number of shares of 
    a stock based on the inputted sell command. *)
let sell_helper stock = 
  try 
    let stock_name = List.nth stock 0 in
    let stock_volume = (int_of_string (List.nth stock 1)) in 
    if stock_volume >= 0 then
      (Engine.sell stock_name stock_volume;
       print_string "\n"; 
       print_string "Your transaction was successful!\n")
    else print_endline "Bad command."
  with _ -> print_endline "\nInvalid Stock."

(** [threshold_helper stock_bounds] takes in a stock name, a upper and lower
    bound for price, and a total investment amount for use 
    in a customized algorithm.*)
let threshold_helper stock_bounds = 
  try
    let data = Engine.data_processor (data_lines "transactions.txt") [] in
    let stock = (List.nth stock_bounds 0) in
    let counter = Engine.shares_search stock data in 
    let upper = (float_of_string (List.nth stock_bounds 1)) in
    let lower = (float_of_string (List.nth stock_bounds 2)) in
    let amount = (float_of_string (List.nth stock_bounds 3)) in
    if upper >= 0.00 && lower >= 0.00 && amount >= 0.00 && upper >= lower then
      Simple_threshold.threshold counter stock upper lower amount 
    else print_endline "Bad command."
  with _ -> print_endline "\nInvalid Stock."
(** [mean_reversion_helper stock_bounds] takes in a stock name, a range, a stock 
    price mean, and a total investment amount for use in 
    a customized algorithm.*)
let mean_reversion_helper stock_ranges = 
  try 
    let data = Engine.data_processor (data_lines "transactions.txt") [] in
    let stock = (List.nth stock_ranges 0) in
    let counter = Engine.shares_search stock data in 
    let range = (float_of_string (List.nth stock_ranges 1)) in
    let mean = (float_of_string (List.nth stock_ranges 2)) in
    let amount = (float_of_string (List.nth stock_ranges 3)) in
    if range >= 0.00 && mean >= 0.00 && amount >= 0.00 && mean >= range then
      Mean_reversion.mean_reversion counter stock range mean amount 
    else print_endline "Bad command."
  with _ -> print_endline "\nInvalid Stock."

(** [profit_helper stock_set] takes in a stock name, a minimum profit, a
    maximum loss, and a total investment amount for use in a customized 
    algorithm. *)
let profit_helper stock_set = 
  let data = Engine.data_processor (data_lines "transactions.txt") [] in
  let data2 = Engine.profit_calc (data_lines "transactions.txt") [] in
  let stock = (List.nth stock_set 0) in
  let counter = Engine.shares_search stock data in
  let current = Engine.profit_search stock data2 in
  let minprof = (float_of_string (List.nth stock_set 1)) in
  let maxloss = (float_of_string (List.nth stock_set 2)) in
  let amount = (float_of_string (List.nth stock_set 3)) in
  Profit.profit_alg counter stock current minprof maxloss amount


(* Info Message to inform users *)
let message = 
  "\nType 'price (stock ticker) (volume)' to get the price for a stock. 
  I.e. 'price AAPL 2' will give you the price of 2 shares of Apple stock.\n
Type 'buy (stock ticker) (volume)' to buy specified number of shares of stock.
  I.e. 'buy AAPL 30' will buy 30 shares of Apple stock.\n
Type 'sell (stock ticker) (volume)' to sell specified number of shares of stock.
  I.e. 'sell AAPL 30' will sell 30 shares of Apple stock.\n
Type 'portfolio' to see the number of shares of each stock you own.
  I.e 'portfolio' will return all the stock you own. \n
Type 'history' to see every transaction you have made before. \n
Type 'clear' to clear the transaction history and the portfolio. \n
Type 'threshold (stock ticker) (upper bound) (lower bound) (amount to invest)' 
to run the threshold algorithm. The program will automatically buy the stock
if it is below your inputted lower bound and will sell the stock if the stock 
goes above the upper bound.
  I.e. 'threshold AAPL 315 312 1000' will buy Apple stock if the price is 
  below $312 and sell Apple stock if the price is above $315. It has $1000 to 
  invest. Otherwise, it will do nothing.\n
Type 'means (stock ticker) (range) (mean) (amount to invest)' 
to run the mean-reversion algorithm. The program will automatically buy the 
stock if the share price is below your mean-range and will sell the stock 
if the share price goes above the mean+range.
  I.e. 'means AAPL 5 300 1000' will buy Apple stock if the price is 
  below $295 and sell Apple stock if the price is above $305. It has $1000 to 
  invest. Otherwise, it will do nothing.\n
Type 'profit (stock ticker) (min profit) (max loss) (amount to invest)'
to run the profit-loss limit algorithm. The program will automatically buy
the stock if the historical value of that stock in your portfolio exceeds your
specified minimum profit and sell the stock if the historical value of that 
stock is below your maximum loss tolerance.
  I.e. 'profit AAPL 1300 5000 1500 will buy Apple stock if you have previously
  made more than $1300 on Apple stock transactions and sell Apple stock if you
  have previously lost more than $8000 on Apple stock transactions. It can buy
  up to $1500 of stock. Otherwise, it will do nothing."

(** run () processes user inputs and performs the appropriate action based on 
    the inputted command. *)
let rec run () =
  print_string 
    "\nType your command here. For help, type 'info'. To quit, type 'quit'.\n>";
  let input = read_line () in 
  match Command.parse input with 
  | Price stock -> price_helper stock; 
    run ()
  | Info -> ANSITerminal.(print_string [red]
                            message); run ()
  | Buy stock -> buy_helper stock; 
    run () 
  | Sell stock -> sell_helper stock; 
    run ()
  | Threshold stock_bounds -> threshold_helper stock_bounds; 
    run ()
  | Mean_reversion stock_ranges -> mean_reversion_helper stock_ranges; 
    run () 
  | Profit stock_set -> profit_helper stock_set;
    run ()
  | Portfolio -> print_string "\n";
    Engine.data_processor (Engine.data_lines "transactions.txt") [] |>
    data_printer |> String.concat " " |> print_string;
    print_string "\n";
    run ()
  | History -> 
    print_string "\n";
    history_printer "transactions.txt" |> List.iter (Printf.printf "%s\n");
    run ()
  | Clear -> overwrite "transactions.txt" "";
    run ()
  | Quit -> print_endline "You are now exiting the system."; 
    Stdlib.exit 0
  | exception Malformed -> print_endline "Invalid command. Try again"; 
    run ()
  | exception Empty -> print_endline "You entered an empty command.";
    run () 

(* Welcome Message to the game *)
let play_game =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Stock Trading Platform.\n");
  run ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game

(* Execute the game engine. *)
let () = main ()