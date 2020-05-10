open Engine
open Stdlib

let check_no_money amount stock_price buy_message = 
  amount -. stock_price < 0.00

let check_counter (counter:int) (none_message:string) = 
  Stdlib.Int.equal counter 0

(** [threshold counter stock upper lower amount] checks the current price of
    [stock] and makes the appropriate transaction based on the inputted [upper], 
    [lower], and [amount].
    Requires:
    [stock] is a valid stock ticker in string form.
    [upper] is a float >= 0.
    [lower] is a float >= 0.
    [amount] is an int >= 0.
    [upper] >= [lower]. *)
let rec threshold counter stock upper lower amount = 
  let stock_price = Engine.get_price stock 1 in
  let buy_message = "\nYou have no more money to spend :(\n" in 
  let none_message = String.concat " " ["\nYou have no more"; stock; "stock\n"] in
  let no_purchase_message = "\nNo purchase\n" in 
  if Engine.compare lower stock_price then
    (if check_no_money amount stock_price buy_message then 
       ANSITerminal.(print_string [green] buy_message)
     else (Engine.buy stock 1;
           print_string "\n1 share of ";
           print_string stock;
           print_string " bought!\n";
           threshold (counter+1) stock upper lower (amount -. stock_price)))
  else if Engine.compare stock_price upper then
    (if check_counter counter none_message then 
       ANSITerminal.(print_string [green] none_message)
     else (Engine.sell stock 1;
           print_string "\n1 share of ";
           print_string stock;
           print_string " sold!\n";
           threshold (counter - 1) stock upper lower (amount +. stock_price)))
  else 
    ANSITerminal.(print_string [green] no_purchase_message)
(* threshold  counter stock upper lower amount) *)

