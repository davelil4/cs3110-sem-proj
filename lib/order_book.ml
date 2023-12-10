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

let get_profit users un =
  let u = StringMap.find un users in
  u.profit

let get_loss users un =
  let u = StringMap.find un users in
  -u.profit

let get_top_profiter users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, b.profit)
    | (un, b) :: t ->
        let u2, p2 = get_top_aux t in
        if b.profit > p2 then (un, b.profit) else (u2, p2)
  in
  get_top_aux users

let get_top_3_profiters users1 =
  let ((u1, _) as us1) = get_top_profiter users1 in
  let users2 = StringMap.remove u1 users1 in
  let ((u2, _) as us2) = get_top_profiter users2 in
  let users3 = StringMap.remove u2 users2 in
  let us3 = get_top_profiter users3 in
  [ us1; us2; us3 ]

let get_top_loss users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, b.profit)
    | (un, b) :: t ->
        let u2, p2 = get_top_aux t in
        if b.profit < p2 then (un, b.profit) else (u2, p2)
  in
  get_top_aux users

let get_top_3_loss users1 =
  let ((u1, _) as us1) = get_top_loss users1 in
  let users2 = StringMap.remove u1 users1 in
  let ((u2, _) as us2) = get_top_loss users2 in
  let users3 = StringMap.remove u2 users2 in
  let us3 = get_top_loss users3 in
  [ us1; us2; us3 ]

let get_top_orderer users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, List.length (StringMap.bindings b.order_history))
    | (un, b) :: t ->
        let u2, p2 = get_top_aux t in
        if List.length (StringMap.bindings b.order_history) > p2 then
          (un, List.length (StringMap.bindings b.order_history))
        else (u2, p2)
  in
  get_top_aux users

let get_top_3_orderers users1 =
  let ((u1, _) as us1) = get_top_orderer users1 in
  let users2 = StringMap.remove u1 users1 in
  let ((u2, _) as us2) = get_top_orderer users2 in
  let users3 = StringMap.remove u2 users2 in
  let us3 = get_top_orderer users3 in
  [ us1; us2; us3 ]

let get_bottom_orderer users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, List.length (StringMap.bindings b.order_history))
    | (un, b) :: t ->
        let u2, p2 = get_top_aux t in
        if List.length (StringMap.bindings b.order_history) < p2 then
          (un, List.length (StringMap.bindings b.order_history))
        else (u2, p2)
  in
  get_top_aux users

let get_bottom_3_orderers users1 =
  let ((u1, _) as us1) = get_bottom_orderer users1 in
  let users2 = StringMap.remove u1 users1 in
  let ((u2, _) as us2) = get_bottom_orderer users2 in
  let users3 = StringMap.remove u2 users2 in
  let us3 = get_bottom_orderer users3 in
  [ us1; us2; us3 ]

let get_most_pending_orders users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, List.length (StringMap.bindings b.pending_orders))
    | (un, b) :: t ->
        let u2, p2 = get_top_aux t in
        if List.length (StringMap.bindings b.pending_orders) > p2 then
          (un, List.length (StringMap.bindings b.pending_orders))
        else (u2, p2)
  in
  get_top_aux users

let get_top_3_nr_pending_orders users1 =
  let ((u1, _) as us1) = get_most_pending_orders users1 in
  let users2 = StringMap.remove u1 users1 in
  let ((u2, _) as us2) = get_most_pending_orders users2 in
  let users3 = StringMap.remove u2 users2 in
  let us3 = get_most_pending_orders users3 in
  [ us1; us2; us3 ]

let get_least_pending_orders users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, List.length (StringMap.bindings b.pending_orders))
    | (un, b) :: t ->
        let u2, p2 = get_top_aux t in
        if List.length (StringMap.bindings b.pending_orders) < p2 then
          (un, List.length (StringMap.bindings b.pending_orders))
        else (u2, p2)
  in
  get_top_aux users

(**let print_asset_book = failwith "Unimplemented" *)

let get_bottom_3_nr_pending_orders users1 =
  let ((u1, _) as us1) = get_least_pending_orders users1 in
  let users2 = StringMap.remove u1 users1 in
  let ((u2, _) as us2) = get_least_pending_orders users2 in
  let users3 = StringMap.remove u2 users2 in
  let us3 = get_least_pending_orders users3 in
  [ us1; us2; us3 ]

let orderbook_to_list book asset typ =
  let o = StringMap.find asset book in
  match typ with B -> o.buy | S -> o.sell

let marketorders_to_list book =
  let fold_func s f = f.buy @ f.sell @ s in
  List.fold_left fold_func []
    (List.map (fun (_, x) -> x) (StringMap.bindings book))

let best_bid asset_map asset_name =
  match StringMap.find_opt asset_name asset_map with
  | None -> None
  (*Assume b is sorted *)
  | Some { buy; _ } -> ( match buy with h :: _ -> Some h | _ -> None)

(**
let rec find_last lis =
  match lis with [] -> None | [ a ] -> Some a | _ :: t -> find_last t *)

(**
let lowest_bid asset_map asset_name =
  match StringMap.find_opt asset_name asset_map with
  | None -> None
  (*Assume b is sorted *)
  | Some { buy; _ } -> find_last buy *)

let best_ask asset_map asset_name =
  match StringMap.find_opt asset_name asset_map with
  | None -> None
  (*Assume s is sorted *)
  | Some { buy = _; sell } -> ( match sell with h :: _ -> Some h | _ -> None)

(**
let worst_ask asset_map asset_name =
  match StringMap.find_opt asset_name asset_map with
  | None -> None
  (*Assume b is sorted *)
  | Some { buy = _; sell } -> find_last sell *)

let rec sort_book b = List.sort compare_order_asc b
and compare_order_asc o1 o2 = compare o1.price o2.price

let empty = (StringMap.empty, StringMap.empty)

let rec remove_order book users order =
  let new_book = remove_from_book book order in
  let new_users =
    remove_from_user_history (remove_from_user_pending users order) order
  in
  (new_book, new_users)

and remove_from_book book order =
  match StringMap.find_opt order.asset book with
  | Some { buy; sell } -> (
      match order.o_type with
      | B ->
          StringMap.add order.asset
            { buy = List.filter (fun v -> v <> order) buy; sell }
            book
      | S ->
          StringMap.add order.asset
            { buy; sell = List.filter (fun v -> v <> order) sell }
            book)
  | None -> book

and remove_from_user_pending users order =
  match StringMap.find_opt order.user users with
  | Some u ->
      StringMap.add order.user
        { u with pending_orders = remove_from_book u.pending_orders order }
        users
  | None -> users

and remove_from_user_history users order =
  match StringMap.find_opt order.user users with
  | Some u ->
      StringMap.add order.user
        { u with order_history = remove_from_book u.order_history order }
        users
  | None -> users

let rec add_order book users order =
  let new_book = add_to_book book order in
  let new_users = add_to_users users order in
  let bb = best_bid new_book order.asset in
  let ba = best_ask new_book order.asset in
  match (bb, ba) with
  | None, _ | _, None -> (new_book, new_users)
  | Some bb, Some ba ->
      if bb.price > ba.price then
        let b1, u1 =
          ( remove_from_book new_book order,
            remove_from_user_pending new_users bb )
        in
        let b2, u2 =
          (remove_from_book b1 order, remove_from_user_pending u1 bb)
        in
        (b2, add_profit (add_profit u2 bb.user ~-(bb.price)) ba.user ba.price)
      else (new_book, new_users)

and add_to_book book order =
  match StringMap.find_opt order.asset book with
  | Some { buy; sell } -> (
      match order.o_type with
      | B ->
          StringMap.add order.asset
            { buy = sort_book (order :: buy) |> List.rev; sell }
            book
      | S ->
          StringMap.add order.asset
            { buy; sell = sort_book (order :: sell) }
            book)
  | None -> (
      match order.o_type with
      | B -> StringMap.add order.asset { buy = [ order ]; sell = [] } book
      | S -> StringMap.add order.asset { buy = []; sell = [ order ] } book)

and add_to_users users order =
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

and add_profit users un v =
  let u = StringMap.find un users in
  let new_u = { u with profit = u.profit + v } in
  StringMap.add un new_u users

(**let to_list book = book *)
let string_order_type ot = match ot with B -> "Buy" | S -> "Sell"

let find_user user users =
  match StringMap.find_opt user users with None -> None | Some u -> Some u

(**
let print_order o =
  Printf.printf
    "\n Order Type: %s, \nAsset: %s, \nPrice: %d, \nQuantity: %d, \nUser: %s\n"
    (string_order_type o.o_type)
    o.asset o.price o.quantity o.user
*)

let rec orderlist_to_string ol =
  let single_order_to_string o =
    "{Order Type: " ^ string_order_type o.o_type ^ "; Asset: " ^ o.asset
    ^ "Price: " ^ string_of_int o.price ^ "Quantity: "
    ^ string_of_int o.quantity ^ "User: " ^ o.user
  in
  match ol with
  | [] -> ""
  | [ h ] -> single_order_to_string h
  | h :: t -> single_order_to_string h ^ orderlist_to_string t

(**Helper function for t_to_string*)
let rec t_list_to_string t_list =
  match t_list with
  | [] -> ""
  | [ (k, v) ] ->
      let buys = v.buy in
      let sells = v.sell in
      "[" ^ k ^ "Buy Orders: " ^ orderlist_to_string buys ^ "; Sell Orders: "
      ^ orderlist_to_string sells ^ "]"
  | (k, v) :: tail ->
      let buys = v.buy in
      let sells = v.sell in
      "[" ^ k ^ "Buy Orders: " ^ orderlist_to_string buys ^ "; Sell Orders: "
      ^ orderlist_to_string sells ^ "]; " ^ t_list_to_string tail

(**Converts an orderbook of type t to a string representation*)
let t_to_string t =
  let t_list = StringMap.bindings t in
  t_list_to_string t_list

(**Creates a string representation of a specific user*)
let user_to_string (u : user) =
  "{Name: " ^ u.username ^ "; Order History: "
  ^ t_to_string u.order_history
  ^ "; Pending Orders: "
  ^ t_to_string u.pending_orders
  ^ "Profit: " ^ string_of_int u.profit

(**Public string representation of all usernames, 
    not outputting any private information*)
let rec public_user_list_string user_list =
  match user_list with
  | [] -> ""
  | [ (k, _) ] -> k
  | (k, _) :: t -> k ^ public_user_list_string t

let users_to_string user_map =
  let userlist = StringMap.bindings user_map in
  public_user_list_string userlist

let print_profile (u : user) =
  Printf.printf "Name: %s\n" u.username;
  Printf.printf "Order History: %s\n" (t_to_string u.order_history);
  Printf.printf "Pending Orders: %s\n" (t_to_string u.pending_orders);
  Printf.printf "Profit: %s\n" (string_of_int u.profit)

let rec asset_book_list_to_string book_list asset bs =
  match book_list with
  | [] -> ""
  | (k, v) :: t ->
      if k = asset then
        match bs with
        | "Buy" -> orderlist_to_string v.buy
        | "Sell" -> orderlist_to_string v.sell
        | _ -> ""
      else asset_book_list_to_string t asset bs

let asset_book_to_string book_map asset b =
  let blist = StringMap.bindings book_map in
  asset_book_list_to_string blist asset b

let rec available_buys_list ob_list =
  match ob_list with
  | [] -> ""
  | [ (k, _) ] -> k
  | (k, _) :: t -> k ^ "; " ^ available_buys_list t

let available_buys ob =
  let lis = StringMap.bindings ob in
  available_buys_list lis

let order_to_string o =
  "{Order Type: " ^ string_order_type o.o_type ^ "; Asset: " ^ o.asset
  ^ "Price: " ^ string_of_int o.price ^ "Quantity: " ^ string_of_int o.quantity
  ^ "User: " ^ o.user

let print_market_book e = print_endline (t_to_string e)
