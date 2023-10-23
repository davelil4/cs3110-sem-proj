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

let add_order book order = failwith("Unimplemented")

let remove_order book order = failwith("Unimplemented")

let best_bid book = failwith("Unimplemented")

let best_ask book = failwith("Unimplemented")

let get_profit book user = failwith("Unimplemented")

let get_loss book user = failwith("Unimplemented")

let to_list book = failwith("Unimplemented")

let print_book book = failwith("Unimplemented")