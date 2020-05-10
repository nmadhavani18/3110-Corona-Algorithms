open Engine

let rec threshold stock upper lower (amount:float) = 
  let stock_price = Engine.get_price stock 1 in
  let message = "\nalgorithm has spent your money :)\n\n" in
  if (amount -. stock_price) < 0.00 then 
    (ANSITerminal.(print_string [green]
                     message); 
     Stdlib.exit 0)
  else if (Engine.compare lower stock_price) then
    (Engine.buy stock 1;
     print_string "\n";
     print_string "1 share of ";
     print_string stock;
     print_string " bought!\n";
     threshold stock upper lower (amount -. stock_price))
  else if (Engine.compare stock_price upper) then
    (Engine.sell stock 1;
     print_string "\n";
     print_string "1 share of ";
     print_string stock;
     print_string " sold!\n";
     threshold stock upper lower (amount +. stock_price))
  else 
    (print_string "no purchase";
     threshold stock upper lower amount)

