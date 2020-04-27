open Soup
open Core
open Mechaml
open Printf
open List

let get_url (stock:string) = 
  "https://finance.yahoo.com/quote/AAPL/key-statistics/"

let get_html stock = 
  Soup.parse (read_file "/html/apple.html") 
  |> Soup.R.select_one "Trsdu(0.3s) Fw(b) Fz(36px) Mb(-4px) D(ib)" 
  |> Soup.to_string
(* Soup.parse (read_file "html/apple.html") 
   |> Soup.select "span" 
   |> Soup.to_list 
   |> List.map (fun span -> Soup.R.id span, Soup.R.leaf_text span) *)

let print_price stock = 
  print_string (get_html stock)

let get_price stock volume= 
  get_html stock |> float_of_string

let record transType stock volume price time =
  ""

let time = 
  Core.Time.of_date_ofday_precise

let buy stock volume = 
  record "buy" stock volume (get_price stock volume) time

let sell stock volume = 
  record "sell" stock volume (get_price stock volume) time

let average stock = 
  ""

let compare price1 price2 = 
  if price1 > price2 then price1 else if price2 > price1 then price1 else -1

