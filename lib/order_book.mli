type o_type =
  | B
  | S  (** Type represensting if the order is being bought or sold. *)

type asset = string
(** Type representing what the asset in the order is. *)

type order = {
  o_type : o_type;
  asset : asset;
  price : int;
  quantity : int;
  user : string;
}
(** Type representing a single order. *)

type t
(** Type representing the order book. *)

type user = {
  username : string;
  order_history : t;
  pending_orders : t;
  profit : int;
}
(** Type representing a single user. *)

type users
(** Type respresenting all of the users. *)

val empty : t * users
(** Creates an empty order book. *)

val add_order : t -> users -> order -> (t * users)
(** Adds an order to an order book. *)

val remove_order : t -> users -> order -> (t * users)
(** Removes an order to an order book. *)

val best_bid : t -> asset -> order option
(** Get the best bid order from the book. *)

val best_ask : t -> asset -> order option
(** Get the best ask order from the book. *)

val get_profit : users -> string -> int
(** Gets the profit made from the trades of a single user. *)

val get_loss : users -> string -> int
(** Gets the loss made from the trades of a signle user *)

val orderbook_to_list : t -> asset -> string -> order list
(** Returns the list representation of all orders of a single asset. *)

val marketorders_to_list : t -> order list
(** Returns the list representation of all orders on the market. *)

val print_market_book : t -> unit
(** Prints the order book. *)

val print_asset_book : t -> asset -> string -> unit
(** Prints a single assets order book. *)

val find_user : string -> users -> user option
(** Returns a speicifc users orders*)

val print_profile : user -> unit
(** Prints a users profile *)