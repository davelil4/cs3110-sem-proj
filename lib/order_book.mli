(** The signature of order books  *)

module type OrderBook = sig
    
    (** Type representing a user who would be putting in an order. *)
    type user

    (** Type representing a single order. *)
    type order

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
    val get_profit: t -> user -> int

    (** Gets the loss made from the trades of a signle user *)
    val get_loss: t -> user -> int

    (** Returns the list representation of the book. *)
    val to_list: t -> order list

    (** Prints the order book *)
    val print_book: t -> unit


end