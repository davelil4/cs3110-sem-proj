type asset = string
type o_type = B | S

type order = {
  o_type : o_type;
  asset : asset;
  price : int;
  quantity : int;
  user : string;
}

type b_s = { buy : order list; sell : order list }
(** RI: buy is in descending order and sell is in ascending order *)

module StringMap = Map.Make (String)

type t = b_s StringMap.t

type user = {
  username : string;
  order_history : t;
  pending_orders : t;
  profit : int;
}

type users = user StringMap.t

let rec sort_book b = List.sort compare_order_asc b
and compare_order_asc o1 o2 = compare o1.price o2.price

let empty = StringMap.empty

let rec add_order book users order =
  let new_book = add_to_book book order in
  let new_users =
    StringMap.add order.user
      (match StringMap.find_opt order.user users with
      | Some u ->
          {
            u with
            pending_orders = add_to_book u.pending_orders order;
            order_history = add_to_book u.order_history order;
          }
      | None ->
          {
            username = order.user;
            order_history = add_to_book StringMap.empty order;
            pending_orders = add_to_book StringMap.empty order;
            profit = 0;
          })
      users
  in
  (new_book, new_users)

and add_to_book book order =
  match StringMap.find_opt order.asset book with
  | Some { buy; sell } -> (
      match order.o_type with
      | B ->
          StringMap.add order.asset
            { buy = sort_book (order :: buy); sell }
            book
      | S ->
          StringMap.add order.asset
            { buy; sell = List.rev (sort_book (order :: sell)) }
            book)
  | None -> (
      match order.o_type with
      | B -> StringMap.add order.asset { buy = [ order ]; sell = [] } book
      | S -> StringMap.add order.asset { buy = []; sell = [ order ] } book)

let rec remove_order book order =
  match book with
  | [] -> []
  | h :: t -> if h = order then t else h :: remove_order t order

let best_bid asset_map asset_name =
  match StringMap.find_opt asset_name asset_map with
  | None -> None
  (*Assume b is sorted *)
  | Some (b, s) -> ( match b with h :: _ -> Some h | _ -> None)

let best_ask asset_map asset_name =
  match StringMap.find_opt asset_name asset_map with
  | None -> None
  (*Assume s is sorted *)
  | Some (b, s) -> ( match s with h :: _ -> Some h | _ -> None)

let find_user user book_and_users =
  let u = snd book_and_users in
  match StringMap.find_opt user u with None -> None | Some users -> Some users

let to_list book = book
let string_order_type ot = match ot with B -> "Buy" | S -> "Sell"
(*let string_of_asset a = match a with Stock b -> b | Option b -> b*)

let print_order o =
  Printf.printf
    "\n Order Type: %s, \nAsset: %s, \nPrice: %d, \nQuantity: %d, \nUser: %s\n"
    (string_order_type o.o_type)
    o.asset o.price o.quantity o.user

let rec print_book (book : order list) =
  match book with
  | [] -> Printf.printf "\n Emtpy Order Book \n"
  | h :: [] -> print_order h
  | h :: t ->
      print_order h;
      print_endline "--------------";
      print_book t

let print_profile u = Printf.printf ""
