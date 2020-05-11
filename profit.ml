open Engine


(** [profit_alg counter stock current minp maxl amount] checks the current 
    value of a stock in [current] and performs the appropriate transaction based
    on the data in [minp], [maxl], and [amount].
    Requires:
    [minp] >= 0.
    [maxl] >= 0.
    [amount] >= 0.*)
let rec profit_alg counter stock current minp maxl amount =
  let stock_price = Engine.get_price stock 1 in
  let buy_message = "\nYou have no more money to spend :(\n" in 
  let none_message = String.concat " " ["\nYou have no more"; stock; "stock\n"] in
  let no_purchase_message = "\nYour stock does not meet the conditions\n
  for a transaction." in 
  if current > minp then 
    (if amount -. stock_price < 0.00 then 
       ANSITerminal.(print_string [green] buy_message)
     else (Engine.buy stock 1;
           print_string "\n1 share of ";
           print_string stock;
           print_string " bought!\n";
           profit_alg (counter+1) stock current minp maxl (amount -. stock_price)))
  else if current < 0.00 -. maxl then 
    (if Stdlib.Int.equal counter 0 then
       ANSITerminal.(print_string [green] none_message)
     else (Engine.sell stock 1;
           print_string "\n1 share of ";
           print_string stock;
           print_string " sold!\n";
           profit_alg (counter - 1) stock current minp maxl amount))
  else 
    ANSITerminal.(print_string [green] no_purchase_message)