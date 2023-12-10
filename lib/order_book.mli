(** Module representing an order book and its users. *)

type o_type = B | S  
(** Type represensting if the order is being bought or sold. *)

type asset = string
(** Type representing what the asset in the order is. *)

type order = {
  o_type : o_type;
  asset : asset;
  price : int;
  quantity : int;
  user : asset;
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
(** Given an order book [book], a set of users [u], and an order [o], 
    adds [o] to [book] and [o] to the order history and pending orders
    of the user that submitted [o]. Matches the order in the book if a 
    valid match exists. *)

val remove_order : t -> users -> order -> t * users
(** Given an order book [book], a set of users [u], and an order [o], 
    removes [o] from [book] and [o] from the user's history who
    submitted [o]. *)

val best_bid : t -> asset -> order option
(** Given an order book [book] and asset [a], get the best bid 
    for asset [a] in [book]. *)

val best_ask : t -> asset -> order option
(** Given an order book [book] and asset [a], get the best ask 
    for asset [a] in [book]. *)

val get_profit : users -> string -> int
(** Given users [u] and username [name], get the profit made by [name]. *)

val get_loss : users -> string -> int
(** Given users [u] and username [name], get the loss made by [name]. *)

val orderbook_to_list : t -> asset -> o_type -> order list
(** Given order book [book], asset [a], and order type [typ],
    Returns the list representation of all [typ] orders of a [a]. *)

val marketorders_to_list : t -> order list
(** Returns the list representation of all orders on the market book [book]. *)

val print_market_book : t -> unit
(** Prints the order book [book]. *)

val asset_book_to_string : t -> asset -> string -> string
(** Given order book [book], asset [a], and order type [typ], return a
    string representation of the list of orders from [a] of type [typ]. *)

val find_user : string -> users -> user option
(** Given username [un] and users [u], return the user profile of [un].
    If they don't exist, return None. *)

val print_profile : user -> unit
(** Prints a users profile *)

val get_top_profiter : users -> string * int
(**Returns a tuple containing the higher profiter's username and profit*)

val get_top_3_profiters : users -> (string * int) list
(** Returns a tuple containing the username and profit of for each of
    the three highest profiters*)

val get_top_loss : users -> string * int
(** Returns a tuple containing the username and loss of the user
with the largest loss*)

val get_top_3_loss : users -> (string * int) list
(** Returns a tuple containing the username and profit of for each of
    the three highest losers*)

val get_top_orderer : users -> string * int
(** Returns a tuple containing the username and loss of the user
with the most orders*)

val get_top_3_orderers : users -> (string * int) list
(** Returns a tuple containing the username and profit of for each of
    the three highest orderers*)

val get_bottom_orderer : users -> string * int
(** Returns a tuple containing the username and loss of the user
with the least orders*)

val get_bottom_3_orderers : users -> (string * int) list
(** Returns a tuple containing the username and profit of for each of
    the three lowest orderers*)

val get_most_pending_orders : users -> string * int
(** Returns a tuple containing the username and loss of the user
with the most orders*)

val get_top_3_nr_pending_orders : users -> (string * int) list
(** Returns a tuple containing the username and profit of for each of
    the three highest pending orders*)

val get_least_pending_orders : users -> string * int
(** Returns a tuple containing the username and loss of the user
with the least orders*)

val get_bottom_3_nr_pending_orders : users -> (string * int) list
(** Returns a tuple containing the username and profit of for each of
    the three least pending orders*)

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
