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
  
let best_bid = failwith("Unimplemented")

let best_ask = failwith("Unimplemented")

let get_profit = failwith("Unimplemented")

let get_loss = failwith("Unimplemented")

let to_list = failwith("Unimplemented")

let print_book = failwith("Unimplemented")