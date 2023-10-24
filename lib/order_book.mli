(** Type representing what the asset in the order is. *)
type asset = Stock of string | Option of string

(** Type represensting if the order is being bought or sold. *)
type o_type = B | S

(** Type representing a single order. *)
type order = {
  o_type: o_type;
  asset: asset;
  price: int;
  quantity: int;
  user: string
}

(** Type representing the order book. *)
type t

(** Creates an empty order book. *)
val empty: t

(** Adds an order to an order book. *)
val add_order: t -> order -> t

(** Removes an order to an order book. *)
val remove_order: t -> order -> t

(** Get the best bid order from the book. *)
val best_bid: t -> order

(** Get the best ask order from the book. *)
val best_ask: t -> order

(** Gets the profit made from the trades of a single user. *)
val get_profit: t -> string -> int

(** Gets the loss made from the trades of a signle user *)
val get_loss: t -> string -> int

(** Returns the list representation of the book. *)
val to_list: t -> order list

(** Prints the order book *)
val print_book: t -> unit
