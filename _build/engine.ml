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

let print_price stock = 
  print_string (get_html stock)

let get_price stock volume= 
  get_html stock |> float_of_string

let buy stock volume = 
  failwith "unimplemented"

let sell stock volume = 
  failwith "unimplemented"

let average stock = 
  failwith "unimplemented"

let time = 
  failwith "unimplemented"

let compare price1 price2 = 
  failwith "unimplemented"

let record transType stock volume price time =
  failwith "unimplemented"