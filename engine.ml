open Soup
open Core
open Printf
open List
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Stdlib

let transactions = []

let get_url stock = 
  String.concat "" ["https://www.marketbeat.com/stocks/NASDAQ/"; stock; "/"] 

let get_file stock =
  String.concat "" [stock; ".html"]

let body stock=
  Client.get (Uri.of_string (get_url stock)) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let file stock =
  let body = Lwt_main.run (body stock) in
  (* print_endline ("Received body\n" ^ body); *)
  body

let rec id_helper id = 
  match id with 
  | None -> true
  | Some a -> if a = "some-other-id" then true else false

let rec leaf_helper leaf = 
  match leaf with 
  | None -> ""
  | Some a -> a

let rec parse_html_helper lst = 
  match lst with 
  | [] -> ""
  | (id,leaf)::t -> if id_helper id then leaf_helper leaf else parse_html_helper t

let parse_html stock = 
  let nums = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '.'] in
  Soup.parse (read_file "html/apple3.html") 
  |> Soup.select "strong" 
  |> Soup.to_list 
  |> List.map (fun span -> Soup.id span, Soup.leaf_text span)
  |> parse_html_helper
  |> Core.String.filter ~f: (fun x -> List.mem x nums)
  |> float_of_string

let print_price stock = 
  print_float (parse_html stock)

let get_price stock volume= 
  parse_html stock 

let record transType stock volume price time =
  Out_channel.write_all "transactions.txt" ~data:
    (String.concat " " [transType; stock; (string_of_int volume); 
                        (string_of_float price); time])

let time = 
  Core.Time.now () |> Core.Time.to_string

let buy stock volume = 
  record "buy" stock volume (get_price stock volume) time

let sell stock volume = 
  record "sell" stock volume (get_price stock volume) time

let average stock = 
  ""

let compare price1 price2 = 
  if price1 > price2 then price1 else if price2 > price1 then price1 else -1

