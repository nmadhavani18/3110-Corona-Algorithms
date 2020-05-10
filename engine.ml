open Soup
open Core
open Printf
open List
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Stdlib


let get_url stock = 
  String.concat "" ["https://www.marketbeat.com/stocks/NASDAQ/"; stock; "/"] 

let get_NYSE_url stock = 
  String.concat "" ["https://www.marketbeat.com/stocks/NYSE/"; stock; "/"]

let get_file stock =
  String.concat "" ["html/"; stock; ".html"]

let body stock=
  Client.get (Uri.of_string (get_url stock)) >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

let body_NYSE stock=
  Client.get (Uri.of_string (get_NYSE_url stock)) >>= fun (resp, body) ->
  body 
  |> Cohttp_lwt.Body.to_string 
  >|= fun body ->
  body

let file stock =
  let body = Lwt_main.run (body stock) in
  body

let file_NYSE stock =
  let body = Lwt_main.run (body_NYSE stock) in
  body

let save_file stock = 
  Core.Out_channel.write_all (get_file stock) ~data:(file stock)

let save_file_NYSE stock = 
  Core.Out_channel.write_all (get_file stock) ~data:(file_NYSE stock)

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
  Soup.parse (read_file (get_file stock)) 
  |> Soup.select "strong" 
  |> Soup.to_list 
  |> List.map (fun span -> Soup.id span, Soup.leaf_text span)
  |> parse_html_helper
  |> Core.String.filter ~f: (fun x -> List.mem x nums)
  |> float_of_string

let print_price stock = 
  print_float (parse_html stock)

let get_price stock volume= 
  try 
    (save_file stock;
     let price = parse_html stock in
     let float_volume = float_of_int volume in
     price *. float_volume)
  with _ ->
    (save_file_NYSE stock;
     let price = parse_html stock in
     let float_volume = float_of_int volume in
     price *. float_volume)

(** [record trans stock volume price time] converts the transaction record 
    into a single string containing transaction type, number of shares, stock
    ticker, price of transaction, and time of transaction. *)
let record trans stock volume price time =
  (String.concat " " [trans; (string_of_int volume); "shares of"; stock;
                      "at"; (string_of_float price); "on"; time])

(** [record_file filename str] writes a transaction in string format 
    into a storage text file. *)
let record_file filename str = 
  let out_channel = Out_channel.create ~append:true filename in 
  protect ~f:(fun () -> fprintf out_channel "%s\n" str)
    ~finally:(fun () -> Out_channel.close out_channel) 

(** [line_read line] attempts to read a single line from a file and throws
    an End_of_file exception if no line exists. *)
let line_read line = try Some (input_line line) with End_of_file -> None

(** [data_lines filename] reads through every line in a file, parsing out
    the stock ticker, transaction type, and number of shares, and storing those
    values in a tuple list. *)
let data_lines filename = 
  let rec line_list line acc= 
    begin match line_read line with 
      | None -> List.rev acc
      | Some a -> let lst = String.split_on_char ' ' a in
        let stock = List.nth lst 4 in 
        let action = List.nth lst 0 in 
        let vol = int_of_string (List.nth lst 1) in
        line_list line ((stock, action, vol)::acc) end in
  line_list (open_in filename) []

(** [data_processor lst acc] reads through a tuple list and calculates the
    current number of shares for each stock based on the transaction history,
    storing the stock ticker and shares in a pair.
    Requires:
      [lst] is a (string * string * int) list.*)
let rec data_processor (lst : (string * string * int) list) acc = 
  match lst with 
  | [] -> acc
  | (stock,action,vol)::t -> 
    if (List.mem_assoc stock acc) = false && action = "bought" 
    then data_processor t ((stock,vol)::acc) 
    else if (List.mem_assoc stock acc) = true && action = "bought" 
    then let rest = List.remove_assoc stock acc in 
      data_processor t ((stock,vol+(List.assoc stock acc))::rest)
    else if (List.mem_assoc stock acc) = true && action = "sold"
    then let rest = List.remove_assoc stock acc in
      data_processor t ((stock, (List.assoc stock acc) - vol)::rest)
    else data_processor t acc

(** [shares_search stock lst] returns the number of shares of a specified stock,
    based on data from a pair list [lst].
    Requires:
      [stock] is a valid stock ticker in string form.
      [lst] is a tuple list of (string * int). *)
let rec shares_search (stock : string) lst= 
  match lst with 
  | [] -> 0
  | (sto,vol)::t -> if stock = sto then vol else shares_search stock t

(** [time] returns the current time. *)
let time =  
  Core.Time.now () |> Core.Time.to_string

(** [buy stock volume] records a buying of an amount of a stock, for a certain
    price, at a certain time, to be written into a transaction file.
    Requires:
      [stock] is a valid stock ticker in string form. 
      [volume] is an int >= 0. *)
let buy stock volume = 
  let record_string = 
    record "bought" stock volume (get_price stock volume) time in
  record_file "transactions.txt" record_string

(** [sell stock volume] records a selling of an amount of a stock, for a certain
    price, at a certain time, to be written into a transaction file. If the 
    specified amount of shares to be sold exceeds the amount of shares held,
    then all shares currently held are sold. 
    Requires:
      [stock] is a valid stock ticker in string form. 
      [volume] is an int >= 0. *)
let sell stock volume = 
  let data = data_processor (data_lines "transactions.txt") [] in 
  let shares = shares_search stock data in 
  if volume > shares then 
    let record_string = 
      record "sold" stock (shares) (get_price stock shares) time in
    record_file "transactions.txt" record_string
  else let record_string = 
         record "sold" stock (volume) (get_price stock volume) time in
    record_file "transactions.txt" record_string

(** [compare price1 price2] returns True if [price1] is greater than or equal to
[price2] and False otherwise. *)
let compare price1 price2 = 
  if price1 > price2 then true else if price2 > price1 then false else true

