type user = string

type asset = Stock of string | Option of string

type o_type = B | S

type order = {
  o_type: o_type;
  asset: asset;
  price: int;
  quantity: int;
  user: user
}

type t = order list

let empty = []

let add_order book order = order :: book

let rec remove_order book order = 
  match book with
  | [] -> []
  | h :: t -> if h = order then t else h :: (remove_order t order)

(*
  
let best_bid = failwith("Unimplemented")

let best_ask = failwith("Unimplemented")

let get_profit = failwith("Unimplemented")

let get_loss = failwith("Unimplemented")

*)

let to_list book = book 

let string_order_type ot = match ot with
| B -> "Buy"
| S -> "Sell"

let string_of_asset a = match a with 
| Stock b -> b
| Option b -> b 

let print_order o = Printf.printf "\n Order Type: %s, 
Asset: %s, 
Price: %d, 
Quantity: %d, 
User: %s\n" 
(string_order_type o.o_type) (string_of_asset o.asset) o.price o.quantity o.user

let rec print_book (book : t) = match book with
| [] -> Printf.printf "\n Emtpy Order Book \n"
| h :: [] -> print_order h
| h :: t -> print_order h; print_endline "--------------"; print_book t