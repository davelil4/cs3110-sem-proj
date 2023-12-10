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
  let u = StringMap.find un users in u.profit

let get_loss users un = 
  let u = StringMap.find un users in (- u.profit)


let get_top_profiter users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, b.profit)
    | (un, b) :: t -> 
      let u2, p2 = get_top_aux t in 
      if b.profit > p2 then (un, b.profit) else (u2, p2)
    in get_top_aux users

let get_top_3_profiters users1 =
  let ((u1, _) as us1) = get_top_profiter users1 in
    let users2 = StringMap.remove u1 users1 in
      let ((u2, _) as us2) = get_top_profiter users2 in 
        let users3 = StringMap.remove u2 users2 in
          let us3 = get_top_profiter users3 in
            [us1; us2; us3]

let get_top_loss users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, b.profit)
    | (un, b) :: t -> 
      let u2, p2 = get_top_aux t in 
      if b.profit < p2 then (un, b.profit) else (u2, p2)
    in get_top_aux users

let get_top_3_loss users1 =
  let ((u1, _) as us1) = get_top_loss users1 in
    let users2 = StringMap.remove u1 users1 in
      let ((u2, _) as us2) = get_top_loss users2 in 
        let users3 = StringMap.remove u2 users2 in
          let us3 = get_top_loss users3 in
            [us1; us2; us3]

let get_top_orderer users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, List.length (StringMap.bindings b.order_history))
    | (un, b) :: t -> 
      let u2, p2 = get_top_aux t in 
        if (List.length (StringMap.bindings b.order_history)) > p2 
          then (un, ((List.length (StringMap.bindings b.order_history)))) 
          else (u2, p2)
    in get_top_aux users

let get_top_3_orderers users1 =
  let ((u1, _) as us1) = get_top_orderer users1 in
    let users2 = StringMap.remove u1 users1 in
      let ((u2, _) as us2) = get_top_orderer users2 in 
        let users3 = StringMap.remove u2 users2 in
          let us3 = get_top_orderer users3 in
            [us1; us2; us3]

let get_bottom_orderer users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, List.length (StringMap.bindings b.order_history))
    | (un, b) :: t -> 
      let u2, p2 = get_top_aux t in 
        if (List.length (StringMap.bindings b.order_history)) < p2 
          then (un, ((List.length (StringMap.bindings b.order_history)))) 
          else (u2, p2)
    in get_top_aux users

let get_bottom_3_orderers users1 =
  let ((u1, _) as us1) = get_bottom_orderer users1 in
    let users2 = StringMap.remove u1 users1 in
      let ((u2, _) as us2) = get_bottom_orderer users2 in 
        let users3 = StringMap.remove u2 users2 in
          let us3 = get_bottom_orderer users3 in
            [us1; us2; us3]

let get_most_pending_orders users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, List.length (StringMap.bindings b.pending_orders))
    | (un, b) :: t -> 
      let u2, p2 = get_top_aux t in 
        if (List.length (StringMap.bindings b.pending_orders)) > p2 
          then (un, ((List.length (StringMap.bindings b.pending_orders)))) 
          else (u2, p2)
    in get_top_aux users

let get_top_3_nr_pending_orders users1 =
  let ((u1, _) as us1) = get_most_pending_orders users1 in
    let users2 = StringMap.remove u1 users1 in
      let ((u2, _) as us2) = get_most_pending_orders users2 in 
        let users3 = StringMap.remove u2 users2 in
          let us3 = get_most_pending_orders users3 in
            [us1; us2; us3]


let get_least_pending_orders users =
  let users = StringMap.bindings users in
  let rec get_top_aux lst =
    match lst with
    | [] -> failwith "No Users"
    | (un, b) :: [] -> (un, List.length (StringMap.bindings b.pending_orders))
    | (un, b) :: t -> 
      let u2, p2 = get_top_aux t in 
        if (List.length (StringMap.bindings b.pending_orders)) < p2 
          then (un, ((List.length (StringMap.bindings b.pending_orders)))) 
          else (u2, p2)
    in get_top_aux users

let get_bottom_3_nr_pending_orders users1 =
  let ((u1, _) as us1) = get_least_pending_orders users1 in
    let users2 = StringMap.remove u1 users1 in
      let ((u2, _) as us2) = get_least_pending_orders users2 in 
        let users3 = StringMap.remove u2 users2 in
          let us3 = get_least_pending_orders users3 in
            [us1; us2; us3]

let print_asset_book = failwith "Unimplemented"

let print_market_book = failwith "Unimplemented"

let orderbook_to_list book asset typ = 
  let o = StringMap.find asset book in
  match typ with
  | B -> o.buy
  | S -> o.sell

let marketorders_to_list book = 
  let fold_func s f =
    f.buy @ f.sell @ s 
  in List.fold_left fold_func [] (List.map (fun (_, x) -> x) (StringMap.bindings book))


let best_bid asset_map asset_name =
  match StringMap.find_opt asset_name asset_map with
  | None -> None
  (*Assume b is sorted *)
  | Some {buy; _} -> ( match buy with h :: _ -> Some h | _ -> None)

let best_ask asset_map asset_name =
  match StringMap.find_opt asset_name asset_map with
  | None -> None
  (*Assume s is sorted *)
  | Some { buy=_; sell } -> ( match sell with h :: _ -> Some h | _ -> None)


let rec sort_book b = (List.sort compare_order_asc b)
and compare_order_asc o1 o2 = 
  compare o1.price o2.price

let empty = (StringMap.empty, StringMap.empty)

let rec remove_order book users order =
  let new_book = remove_from_book book order
    in let new_users = remove_from_user_history (remove_from_user_pending users order) order
       in (new_book, new_users)

and remove_from_book book order =
  match StringMap.find_opt order.asset book with
  | Some {buy; sell} ->  begin
    match order.o_type with 
    | B -> StringMap.add order.asset { buy = List.filter (fun v -> v <> order) buy; sell = sell } book
    | S -> StringMap.add order.asset {buy = buy; sell = List.filter (fun v -> v <> order) sell} book
  end
  | None -> book

and remove_from_user_pending users order =
  match StringMap.find_opt order.user users with
      | Some u -> begin
        StringMap.add order.user {
          u with 
          pending_orders = (remove_from_book u.pending_orders order)
        } users
      end
      | None -> users

and remove_from_user_history users order =
  match StringMap.find_opt order.user users with
      | Some u -> begin
        StringMap.add order.user {
          u with 
          order_history = (remove_from_book u.order_history order)
        } users
      end
      | None -> users

let rec add_order book users order = 
  let new_book = add_to_book book order in
  let new_users = add_to_users users order in
  let bb = (best_bid new_book order.asset) in
  let ba = (best_ask new_book order.asset) in
  match bb, ba with
  | None, _
  | _, None -> (new_book, new_users)
  | Some bb, Some ba -> 
    if bb.price > ba.price
      then 
        let b1, u1 = (remove_from_book new_book order, remove_from_user_pending new_users bb) in
        let b2, u2 = (remove_from_book b1 order, remove_from_user_pending u1 bb) in
        (b2, add_profit (add_profit u2 bb.user ~-(bb.price)) ba.user ba.price)
    else
      (new_book, new_users)

  
and add_to_book book order = 
  match StringMap.find_opt order.asset book with
    | Some {buy; sell} ->  begin
      match order.o_type with 
      | B -> StringMap.add order.asset { buy = (sort_book (order :: buy)); sell = sell } book
      | S -> StringMap.add order.asset { buy = buy; sell = (List.rev (sort_book (order :: sell))) } book
    end
    | None -> begin
      match order.o_type with
      | B -> StringMap.add order.asset { buy = [order]; sell = []} book
      | S -> StringMap.add order.asset { buy=[]; sell=[order]} book
    end

  and add_to_users users order =
    StringMap.add order.user
      (match StringMap.find_opt order.user users with
      | Some u -> begin
        {
          u with 
          pending_orders = (add_to_book u.pending_orders order); 
          order_history = (add_to_book u.order_history order)
        }
      end
      | None -> {
        username=order.user;
        order_history = (add_to_book StringMap.empty order);
        pending_orders = (add_to_book StringMap.empty order);
        profit=0;
      }) users


and add_profit users un v =
  let u = StringMap.find un users in let new_u =
  { u with profit = (u.profit + v) } in 
  (StringMap.add un new_u users)


let to_list book = book
let string_order_type ot = match ot with B -> "Buy" | S -> "Sell"

let find_user user users =
  match StringMap.find_opt user users with None -> None | Some u -> Some u

let print_order o =
  Printf.printf
    "\n Order Type: %s, \nAsset: %s, \nPrice: %d, \nQuantity: %d, \nUser: %s\n"
    (string_order_type o.o_type)
    o.asset o.price o.quantity o.user

let rec print_book _ = failwith "Unimplemented"
  
  
  (* match book with
  | [] -> Printf.printf "\n Emtpy Order Book \n"
  | h :: [] -> print_order h
  | h :: t ->
      print_order h;
      print_endline "--------------";
      print_book t *)

let print_profile _ = failwith "Unimplemented"
