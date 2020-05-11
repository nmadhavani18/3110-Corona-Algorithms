open Engine
open Stdlib

(** [check_no_money amount stock_price buy_message] checks if the user has 
    enough money to buy more of a specified stock. If the user does not have 
    enough money, it returns true, otherwise it returns false. *)
let check_no_money amount stock_price buy_message = 
  if (amount -. stock_price) < 0.00 then true else false

(** [check_counter counter none_message] checks the current volume of a
    specified stock in the user's portfolio and returns true if the user does 
    not have any stock remaining. Otherwise it returns false. *)
let check_counter (counter:int) (none_message:string) = 
  if (Stdlib.Int.equal counter 0) then true else false

(** [mean_reversion counter stock range mean amount] checks the current price of
    [stock] and makes the appropriate transaction based on the inputted [range], 
    [mean], and [amount].
    Requires:
    [stock] is a valid stock ticker in string form.
    [range] is a float >= 0.
    [mean] is a float >= 0.
    [amount] is an int >= 0.
    [mean] >= [range]. *)
let rec mean_reversion counter stock range mean amount = 
  let stock_price = Engine.get_price stock 1 in
  let buy_message = "\nYou have no more money to spend :(\n" in 
  let none_message = String.concat " " ["\nYou have no more"; stock; "stock\n"] in
  let no_purchase_message = "\nNo purchase\n" in 
  let lower = mean -. range in
  let upper = mean +. range in
  if Engine.compare lower stock_price then
    (if check_no_money amount stock_price buy_message then 
       ANSITerminal.(print_string [green] buy_message)
     else (Engine.buy stock 1;
           print_string "\n1 share of ";
           print_string stock;
           print_string " bought!\n";
           mean_reversion (counter+1) stock range mean (amount -. stock_price)))
  else if Engine.compare stock_price upper then
    (if check_counter counter none_message then 
       ANSITerminal.(print_string [green] none_message)
     else (Engine.sell stock 1;
           print_string "\n1 share of ";
           print_string stock;
           print_string " sold!\n";
           mean_reversion (counter - 1) stock range mean (amount +. stock_price)))
  else 
    ANSITerminal.(print_string [green] no_purchase_message)

