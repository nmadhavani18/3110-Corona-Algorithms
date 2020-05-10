open Engine
open Stdlib

let check_no_money amount stock_price buy_message = 
  if (amount -. stock_price) < 0.00 then true else false

let check_counter (counter:int) (none_message:string) = 
  if (Stdlib.Int.equal counter 0) then true else false

let rec threshold counter stock upper lower amount = 
  let stock_price = Engine.get_price stock 1 in
  let buy_message = "\nYou have no more money to spend :(\n" in 
  let none_message = String.concat " " ["\nYou have no more"; stock; "stock\n"] in
  let no_purchase_message = "\nNo purchase\n" in 
  if (Engine.compare lower stock_price) then
    (if check_no_money amount stock_price buy_message then 
       ANSITerminal.(print_string [green]
                       buy_message)
     else (Engine.buy stock 1;
           print_string "\n";
           print_string "1 share of ";
           print_string stock;
           print_string " bought!\n";
           threshold (counter+1) stock upper lower (amount -. stock_price)))
  else if (Engine.compare stock_price upper) then
    (if check_counter counter none_message then 
       ANSITerminal.(print_string [green]
                       none_message)
     else (Engine.sell stock 1;
           print_string "\n";
           print_string "1 share of ";
           print_string stock;
           print_string " sold!\n";
           threshold  (counter - 1) stock upper lower (amount +. stock_price)))
  else 
    ANSITerminal.(print_string [green]
                    no_purchase_message)
(* threshold  counter stock upper lower amount) *)

