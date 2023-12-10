type o_type =
  | B
  | S  (** Type represensting if the order is being bought or sold. *)

type asset = string
(** Type representing what the asset in the order is. *)

type order = {
  o_type : o_type;
  asset : string;
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

val add_order : t -> users -> order -> t * users
(** Adds an order to an order book. *)

val remove_order : t -> users -> order -> t * users
(** Removes an order to an order book. *)

val best_bid : t -> string -> order option
(** Get the best bid order from the book. *)

val best_ask : t -> string -> order option
(** Get the best ask order from the book. *)

val get_profit : users -> string -> int
(** Gets the profit made from the trades of a single user. *)

val get_loss : users -> string -> int
(** Gets the loss made from the trades of a signle user *)

val orderbook_to_list : t -> string -> o_type -> order list
(** Returns the list representation of all orders of a single asset. *)

val marketorders_to_list : t -> order list
(** Returns the list representation of all orders on the market. *)

val print_market_book : t -> unit
(** Prints the order book. *)

val asset_book_to_string : t -> string -> string -> string
(** Prints a single assets order book. *)

val find_user : string -> users -> user option
(** Returns a speicifc users orders*)

val print_profile : user -> unit
(** Prints a users profile *)

val get_top_profiter : users -> string * int
val get_top_3_profiters : users -> (string * int) list
val get_top_loss : users -> string * int
val get_top_3_loss : users -> (string * int) list
val get_top_orderer : users -> string * int
val get_top_3_orderers : users -> (string * int) list
val get_bottom_orderer : users -> string * int
val get_bottom_3_orderers : users -> (string * int) list
val get_most_pending_orders : users -> string * int
val get_top_3_nr_pending_orders : users -> (string * int) list
val get_least_pending_orders : users -> string * int
val get_bottom_3_nr_pending_orders : users -> (string * int) list

val t_to_string : t -> string
(**Converts an orderbook of type t to a string representation*)

val user_to_string : user -> string
(**Creates a string representation of a user*)

val users_to_string : users -> string
(**Public string representation of all usernames, not outputting any private information*)

val available_buys : t -> string
(**Creates a list of the available assets to buy*)

val order_to_string : order -> string
(**Creates a string representation of an order*)
