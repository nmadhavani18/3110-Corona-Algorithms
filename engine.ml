open Soup
open Core
open Mechaml
open Printf
open List
open Stdlib

let get_url_helper (result) = 
  match result with
  | (x,y) -> Agent.HttpResponse.content y |> Lwt.return

(* let get_url (stock:string) = 
   Lwt.bind (Mechaml.Agent.get "https://www.marketbeat.com/stocks/NASDAQ/AAPL/")
   get_url_helper *)

let rec id_helper id = 
  match id with 
  | None -> true
  | Some a -> if a = "some-other-id" then true else false

let rec leaf_helper leaf = 
  match leaf with 
  | None -> ""
  | Some a -> a

let rec get_html_helper (lst: (string option * string option) list) = 
  match lst with 
  | [] -> ""
  | (id,leaf)::t -> if id_helper id then leaf_helper leaf else get_html_helper t

let get_html stock = 
  let nums = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '.'] in
  Soup.parse (read_file "html/apple2.html") 
  |> Soup.select "strong" 
  |> Soup.to_list 
  |> List.map (fun span -> Soup.id span, Soup.leaf_text span)
  |> get_html_helper
  |> Core.String.filter ~f: (fun x -> List.mem x nums)
  |> float_of_string

let print_price stock = 
  print_float (get_html stock)

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

