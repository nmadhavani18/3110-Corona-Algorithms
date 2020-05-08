open Soup
open Core
open Mechaml
open Printf
open List
open Stdlib
open Transactions_t
open Transactions_j

let get_url (stock:string) = 
  "https://finance.yahoo.com/quote/AAPL/key-statistics/"

let rec id_helper id = 
  match id with 
  | None -> false
  | Some a -> if a = "neilmadhavani" then true else false

let rec leaf_helper leaf = 
  match leaf with 
  | None -> ""
  | Some a -> a

let rec get_html_helper (lst: (string option * string option) list) = 
  match lst with 
  | [] -> ""
  | (id,leaf)::t -> if id_helper id then leaf_helper leaf else get_html_helper t

let get_html stock = 
  Soup.parse (read_file "html/apple.html") 
  |> Soup.select "title" 
  |> Soup.to_list 
  |> List.map (fun span -> Soup.id span, Soup.leaf_text span)
  |> get_html_helper

let print_price stock = 
  print_string (get_html stock)

let get_price stock volume= 
  get_html stock 
(* |> float_of_string  *)

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

