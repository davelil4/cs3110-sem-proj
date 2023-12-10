(**

open OUnit2
open Order_book

let string_of_order order =
  Printf.sprintf "Order(type: %s, asset: %s, price: %d, quantity: %d, user: %s)"
    (match order.o_type with B -> "Buy" | S -> "Sell")
    order.asset order.price order.quantity order.user

let string_of_order_list orders =
  "[" ^ String.concat "; " (List.map string_of_order orders) ^ "]"

(* Helper function to create orders *)
let make_order o_type asset price quantity user =
  { o_type; asset; price; quantity; user }

let empty_book, empty_users = Order_book.empty

(********************************Empty Test***********************************)
let test_empty _ =
  let asset = "AAPL" in
  let orders_buy = orderbook_to_list empty_book asset B in
  let orders_sell = orderbook_to_list empty_book asset S in
  assert_equal [] orders_buy ~printer:string_of_order_list;
  assert_equal [] orders_sell ~printer:string_of_order_list

(********************************Add_order Test***********************************)
let test_add_order_to_empty _ =
  let new_order = make_order B "AAPL" 100 10 "Alice" in
  let updated_book, _ = add_order empty_book empty_users new_order in
  let orders = orderbook_to_list updated_book "AAPL" B in
  assert_equal [ new_order ] orders ~printer:string_of_order_list

let test_add_existing_order _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let book_with_order, users_with_order =
    add_order empty_book empty_users order
  in
  let book_with_duplicate, _ =
    add_order book_with_order users_with_order order
  in
  let orders = orderbook_to_list book_with_duplicate "AAPL" B in
  assert_equal [ order; order ] orders ~printer:string_of_order_list

let test_add_orders_different_assets _ =
  let aapl_order = make_order B "AAPL" 100 10 "Alice" in
  let goog_order = make_order B "GOOG" 100 5 "Bob" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ aapl_order; goog_order ]
  in
  let aapl_orders = orderbook_to_list book_with_orders "AAPL" B in
  let goog_orders = orderbook_to_list book_with_orders "GOOG" B in
  assert_equal [ aapl_order ] aapl_orders ~printer:string_of_order_list;
  assert_equal [ goog_order ] goog_orders ~printer:string_of_order_list

let test_add_multiple_orders_same_asset_user _ =
  let orders =
    [
      make_order B "AAPL" 100 5 "Alice";
      make_order B "AAPL" 110 10 "Alice";
      make_order B "AAPL" 105 15 "Alice";
    ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  let retrieved_orders = orderbook_to_list book_with_orders "AAPL" B in
  assert_equal orders retrieved_orders ~printer:string_of_order_list

let test_add_orders_different_prices _ =
  let orders =
    [
      make_order B "AAPL" 100 10 "Alice";
      make_order B "AAPL" 110 20 "Alice";
      make_order B "AAPL" 105 15 "Alice";
    ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  let retrieved_orders = orderbook_to_list book_with_orders "AAPL" B in
  assert_bool "Contains orders with different prices"
    (List.for_all (fun order -> List.mem order retrieved_orders) orders)

let test_add_orders_same_asset_different_users _ =
  let orders =
    [ make_order B "AAPL" 100 10 "Alice"; make_order B "AAPL" 110 20 "Bob" ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  let alice_orders = orderbook_to_list book_with_orders "AAPL" B in
  let bob_orders = orderbook_to_list book_with_orders "AAPL" B in
  assert_equal [ List.hd orders ] alice_orders ~printer:string_of_order_list;
  assert_equal [ List.nth orders 1 ] bob_orders ~printer:string_of_order_list

let test_add_order_negative_price _ =
  let order = make_order B "AAPL" (-100) 10 "Alice" in
  let book_with_order, _ = add_order empty_book empty_users order in
  let orders = orderbook_to_list book_with_order "AAPL" B in
  assert_equal [] orders ~printer:string_of_order_list

let test_add_order_zero_quantity _ =
  let order = make_order B "AAPL" 100 0 "Alice" in
  let book_with_order, _ = add_order empty_book empty_users order in
  let orders = orderbook_to_list book_with_order "AAPL" B in
  assert_equal [] orders ~printer:string_of_order_list

let test_add_and_remove_multiple_orders _ =
  let orders =
    [ make_order B "AAPL" 100 10 "Alice"; make_order B "AAPL" 105 15 "Bob" ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  let book_after_removal, _ =
    List.fold_left
      (fun (book, users) order -> remove_order book users order)
      (book_with_orders, empty_users)
      orders
  in
  assert_bool "Book should be empty after adding and removing orders"
    (marketorders_to_list book_after_removal = [])

(***********************Remove_Order Tests********************************)
let test_remove_order_from_multiple _ =
  let order1 = make_order B "AAPL" 100 10 "Alice" in
  let order2 = make_order B "AAPL" 105 5 "Bob" in
  let book_with_orders, users_with_orders =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2 ]
  in
  let updated_book, _ =
    remove_order book_with_orders users_with_orders order1
  in
  let alice_orders = orderbook_to_list updated_book "AAPL" B in
  let bob_orders = orderbook_to_list updated_book "AAPL" B in
  assert_equal [] alice_orders ~printer:string_of_order_list;
  assert_equal [ order2 ] bob_orders ~printer:string_of_order_list

let test_remove_recently_added_order _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let book_with_order, _ = add_order empty_book empty_users order in
  let updated_book, _ = remove_order book_with_order empty_users order in
  let orders = orderbook_to_list updated_book "AAPL" B in
  assert_equal [] orders ~printer:string_of_order_list

let test_remove_single_order _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let book_with_order, _ = add_order empty_book empty_users order in
  let updated_book, _ = remove_order book_with_order empty_users order in
  assert_bool "Book should be empty after removing the only order"
    (marketorders_to_list updated_book = [])

let test_remove_nonexistent_order _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let updated_book, _ = remove_order empty_book empty_users order in
  let orders = orderbook_to_list updated_book "AAPL" B in
  assert_equal [] orders ~printer:string_of_order_list

let test_remove_order_from_empty_book _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let updated_book, _ = remove_order empty_book empty_users order in
  assert_bool "Book should remain empty" (marketorders_to_list updated_book = [])

(******************************best_bid tests**********************************)
let test_best_bid _ =
  let order1 = make_order B "AAPL" 100 10 "Alice" in
  let order2 = make_order B "AAPL" 105 5 "Bob" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2 ]
  in
  match best_bid book_with_orders "AAPL" with
  | Some best_bid_order -> assert_equal order2 best_bid_order
  | None -> failwith "No best bid found when expected"

let test_best_bid_with_no_buy_orders _ =
  let order = make_order S "AAPL" 150 5 "Charlie" in
  let book_with_order, _ = add_order empty_book empty_users order in
  match best_bid book_with_order "AAPL" with
  | None -> ()
  | Some _ -> failwith "Found a best bid when none should exist"

let test_best_bid_multiple_orders _ =
  let orders =
    [
      make_order B "AAPL" 100 10 "Alice";
      make_order B "AAPL" 105 5 "Bob";
      make_order B "AAPL" 110 15 "Charlie";
    ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  match best_bid book_with_orders "AAPL" with
  | Some best_bid_order ->
      assert_equal (make_order B "AAPL" 110 15 "Charlie") best_bid_order
  | None -> failwith "No best bid found when expected"

let test_best_bid_same_price_different_quantities _ =
  let orders =
    [ make_order B "AAPL" 100 10 "Alice"; make_order B "AAPL" 100 20 "Bob" ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  match best_bid book_with_orders "AAPL" with
  | Some best_bid_order ->
      assert_equal (make_order B "AAPL" 100 20 "Bob") best_bid_order
  | None -> failwith "No best bid found when expected"

let test_best_bid_different_assets _ =
  let orders =
    [ make_order B "AAPL" 100 10 "Alice"; make_order B "GOOG" 200 5 "Bob" ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  match best_bid book_with_orders "GOOG" with
  | Some best_bid_order ->
      assert_equal (make_order B "GOOG" 200 5 "Bob") best_bid_order
  | None -> failwith "No best bid for GOOG found when expected"

(*****************************best_ask tests************************************)
let test_best_ask _ =
  let order1 = make_order S "AAPL" 150 10 "Charlie" in
  let order2 = make_order S "AAPL" 145 5 "Diana" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2 ]
  in
  match best_ask book_with_orders "AAPL" with
  | Some best_ask_order -> assert_equal order2 best_ask_order
  | None -> failwith "No best ask found when expected"

let test_best_ask_with_no_sell_orders _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let book_with_order, _ = add_order empty_book empty_users order in
  match best_ask book_with_order "AAPL" with
  | None -> ()
  | Some _ -> failwith "Found a best ask when none should exist"

let test_best_ask_multiple_orders _ =
  let orders =
    [
      make_order S "AAPL" 150 10 "Charlie";
      make_order S "AAPL" 145 5 "Diana";
      make_order S "AAPL" 140 20 "Eve";
    ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  match best_ask book_with_orders "AAPL" with
  | Some best_ask_order ->
      assert_equal (make_order S "AAPL" 140 20 "Eve") best_ask_order
  | None -> failwith "No best ask found when expected"

let test_best_ask_same_price_different_quantities _ =
  let orders =
    [ make_order S "AAPL" 150 10 "Charlie"; make_order S "AAPL" 150 15 "Diana" ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  match best_ask book_with_orders "AAPL" with
  | Some best_ask_order ->
      assert_equal (make_order S "AAPL" 150 15 "Diana") best_ask_order
  | None -> failwith "No best ask found when expected"

let test_best_ask_different_assets _ =
  let orders =
    [ make_order S "AAPL" 150 10 "Charlie"; make_order S "GOOG" 300 5 "Eve" ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  match best_ask book_with_orders "GOOG" with
  | Some best_ask_order ->
      assert_equal (make_order S "GOOG" 300 5 "Eve") best_ask_order
  | None -> failwith "No best ask for GOOG found when expected"

(***************************to_list tests***********************************)
let test_marketorders_to_list_after_additions _ =
  let order1 = make_order B "AAPL" 100 10 "Alice" in
  let order2 = make_order S "AAPL" 150 5 "Charlie" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2 ]
  in
  let market_orders = marketorders_to_list book_with_orders in
  assert_equal [ order1; order2 ] market_orders ~printer:string_of_order_list

let test_marketorders_to_list_after_removals _ =
  let order1 = make_order B "AAPL" 100 10 "Alice" in
  let order2 = make_order S "AAPL" 150 5 "Charlie" in
  let book_with_orders, users_with_orders =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2 ]
  in
  let book_after_removal, _ =
    remove_order book_with_orders users_with_orders order1
  in
  let market_orders = marketorders_to_list book_after_removal in
  assert_equal [ order2 ] market_orders ~printer:string_of_order_list

let test_orderbook_to_list_empty_book _ =
  let empty_buy_orders = orderbook_to_list empty_book "AAPL" B in
  let empty_sell_orders = orderbook_to_list empty_book "AAPL" S in
  assert_equal [] empty_buy_orders ~printer:string_of_order_list;
  assert_equal [] empty_sell_orders ~printer:string_of_order_list

let test_marketorders_to_list_empty_book _ =
  assert_bool "No market orders should be present in an empty book"
    (marketorders_to_list empty_book = [])

(***********************view tests*********************************************)

let test_view_profile_non_existent_user _ =
  let non_existent_user = find_user "NonExistentUser" empty_users in
  assert_equal None non_existent_user

let test_view_mixed_order_book _ =
  let orders =
    [ make_order B "AAPL" 100 10 "Alice"; make_order S "AAPL" 150 5 "Charlie" ]
  in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) orders
  in
  let market_orders = marketorders_to_list book_with_orders in
  assert_equal orders market_orders ~printer:string_of_order_list

(**************************find_user tests***********************************)
let test_find_existing_user _ =
  let alice_order = make_order B "AAPL" 100 10 "Alice" in
  let bob_order = make_order B "GOOG" 200 20 "Bob" in
  let _, users =
    List.fold_left
      (fun (b, u) o -> add_order b u o)
      (empty_book, empty_users) [ alice_order; bob_order ]
  in
  match find_user "Alice" users with
  | Some user -> assert_equal "Alice" user.username
  | None -> failwith "Expected user Alice to be found"

let test_find_non_existent_user _ =
  match find_user "NonExistentUser" empty_users with
  | None -> ()
  | Some _ -> failwith "Non-existent user was found"

let test_find_user_with_multiple_users _ =
  let alice_order = make_order B "AAPL" 100 10 "Alice" in
  let bob_order = make_order S "GOOG" 200 5 "Bob" in
  let charlie_order = make_order B "MSFT" 150 15 "Charlie" in
  let _, users =
    List.fold_left
      (fun (b, u) o -> add_order b u o)
      (empty_book, empty_users)
      [ alice_order; bob_order; charlie_order ]
  in
  match find_user "Charlie" users with
  | Some user -> assert_equal "Charlie" user.username
  | None -> failwith "Expected user Charlie to be found"

let test_find_user_case_sensitivity _ =
  let alice_order = make_order B "AAPL" 100 10 "Alice" in
  let _, users =
    List.fold_left
      (fun (b, u) o -> add_order b u o)
      (empty_book, empty_users) [ alice_order ]
  in
  match find_user "alice" users with
  | None -> ()
  | Some _ -> failwith "User name should be case sensitive"

let test_find_user_complex_case _ =
  let alice_order1 = make_order B "AAPL" 100 10 "Alice" in
  let alice_order2 = make_order S "GOOG" 200 5 "Alice" in
  let alice_order3 = make_order B "MSFT" 150 15 "Alice" in
  let _, users =
    List.fold_left
      (fun (b, u) o -> add_order b u o)
      (empty_book, empty_users)
      [ alice_order1; alice_order2; alice_order3 ]
  in
  match find_user "Alice" users with
  | Some user ->
      assert_equal "Alice" user.username;
      assert_equal 3
        (List.length (orderbook_to_list user.order_history "AAPL" B)
        + List.length (orderbook_to_list user.order_history "GOOG" S)
        + List.length (orderbook_to_list user.order_history "MSFT" B))
  | None -> failwith "Expected user Alice with multiple orders to be found"

(***************************get_top_profiter tests****************************)

let suite =
  "Order Book Tests"
  >::: [
         "test_empty" >:: test_empty;
         "test_add_order_to_empty" >:: test_add_order_to_empty;
         "test_add_existing_order" >:: test_add_existing_order;
         "test_add_orders_different_assets" >:: test_add_orders_different_assets;
         "test_remove_order_from_multiple" >:: test_remove_order_from_multiple;
         "test_remove_nonexistent_order" >:: test_remove_nonexistent_order;
         (**"test_best_bid" >:: test_best_bid;
         "test_best_ask" >:: test_best_ask; *)
         "test_marketorders_to_list_after_additions"
         >:: test_marketorders_to_list_after_additions;
         "test_marketorders_to_list_after_removals"
         >:: test_marketorders_to_list_after_removals;
         "test_add_multiple_orders_same_asset_user"
         >:: test_add_multiple_orders_same_asset_user;
         "test_add_orders_different_prices" >:: test_add_orders_different_prices;
         "test_add_orders_same_asset_different_users"
         >:: test_add_orders_same_asset_different_users;
         "test_remove_recently_added_order" >:: test_remove_recently_added_order;
         "test_remove_single_order" >:: test_remove_single_order;
         "test_orderbook_to_list_empty_book"
         >:: test_orderbook_to_list_empty_book;
         "test_marketorders_to_list_empty_book"
         >:: test_marketorders_to_list_empty_book;
         "test_best_bid_with_no_buy_orders" >:: test_best_bid_with_no_buy_orders;
         "test_best_ask_with_no_sell_orders"
         >:: test_best_ask_with_no_sell_orders;
         "test_add_order_negative_price" >:: test_add_order_negative_price;
         "test_add_order_zero_quantity" >:: test_add_order_zero_quantity;
         "test_remove_order_from_empty_book"
         >:: test_remove_order_from_empty_book;
         "test_view_profile_non_existent_user"
         >:: test_view_profile_non_existent_user;
         "test_add_and_remove_multiple_orders"
         >:: test_add_and_remove_multiple_orders;
         "test_view_mixed_order_book" >:: test_view_mixed_order_book;
         "test_find_existing_user" >:: test_find_existing_user;
         "test_find_non_existent_user" >:: test_find_non_existent_user;
         "test_find_user_with_multiple_users"
         >:: test_find_user_with_multiple_users;
         "test_find_user_case_sensitivity" >:: test_find_user_case_sensitivity;
         "test_find_user_complex_case" >:: test_find_user_complex_case;
         "test_best_ask_multiple_orders " >:: test_best_ask_multiple_orders;
         "test_best_ask_same_price_different_quantities"
         >:: test_best_ask_same_price_different_quantities;
         "test_best_ask_different_assets" >:: test_best_ask_different_assets;
         "test_best_ask_same_price_different_quantities"
         >:: test_best_ask_same_price_different_quantities;
         "test_best_bid_different_assets" >:: test_best_bid_different_assets;
         "test_best_bid_same_price_different_quantities"
         >:: test_best_bid_same_price_different_quantities;
         "test_best_bid_multiple_orders" >:: test_best_bid_multiple_orders;
       ]

let _ = run_test_tt_main suite

*)
