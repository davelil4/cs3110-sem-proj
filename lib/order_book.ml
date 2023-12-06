type asset_type = Stock of string | Option of string
type o_type = B | S

type order = {
  o_type : o_type;
  asset : asset_type;
  price : int;
  quantity : int;
  user : string;
}

type asset = string * o_type
type b_s = { buy : order list; sell : order list }

module OrderListKey : Map.OrderedType = struct
  type t = asset

  let compare a1 a2 =
    let n1, _ = a1 in
    let n2, _ = a2 in
    String.compare n1 n2
end

module AssetMap = Map.Make (OrderListKey)

type t = b_s AssetMap.t

type user = {
  username : string;
  order_history : t;
  pending_orders : t;
  profit : int;
}

type users = user list

let empty = []
let add_order book order = order :: book

let rec remove_order book order =
  match book with
  | [] -> []
  | h :: t -> if h = order then t else h :: remove_order t order

(*
  
let best_bid = failwith("Unimplemented")

let best_ask = failwith("Unimplemented")

let get_profit = failwith("Unimplemented")

let get_loss = failwith("Unimplemented")

*)

let to_list book = book
let string_order_type ot = match ot with B -> "Buy" | S -> "Sell"
let string_of_asset a = match a with Stock b -> b | Option b -> b

let print_order o =
  Printf.printf
    "\n Order Type: %s, \nAsset: %s, \nPrice: %d, \nQuantity: %d, \nUser: %s\n"
    (string_order_type o.o_type)
    (string_of_asset o.asset) o.price o.quantity o.user

let rec print_book (book : order list) =
  match book with
  | [] -> Printf.printf "\n Emtpy Order Book \n"
  | h :: [] -> print_order h
  | h :: t ->
      print_order h;
      print_endline "--------------";
      print_book t
