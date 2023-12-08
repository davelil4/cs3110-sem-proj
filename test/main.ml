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

let test_empty _ =
  let asset = "AAPL" in
  let user_id = "user123" in
  let orders = orderbook_to_list empty_book asset user_id in
  assert_equal [] orders ~printer:string_of_order_list

let test_add_order_to_empty _ =
  let new_order = make_order B "AAPL" 100 10 "Alice" in
  let updated_book, _ = add_order empty_book empty_users new_order in
  let orders = orderbook_to_list updated_book "AAPL" "Alice" in
  assert_equal [ new_order ] orders ~printer:string_of_order_list

let test_add_existing_order _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let book_with_order, users_with_order =
    add_order empty_book empty_users order
  in
  let book_with_duplicate, _ =
    add_order book_with_order users_with_order order
  in
  let orders = orderbook_to_list book_with_duplicate "AAPL" "Alice" in
  assert_equal [ order; order ] orders ~printer:string_of_order_list

let test_add_orders_different_assets _ =
  let aapl_order = make_order B "AAPL" 100 10 "Alice" in
  let goog_order = make_order B "GOOG" 100 5 "Bob" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ aapl_order; goog_order ]
  in
  let aapl_orders = orderbook_to_list book_with_orders "AAPL" "Alice" in
  let goog_orders = orderbook_to_list book_with_orders "GOOG" "Bob" in
  assert_equal [ aapl_order ] aapl_orders ~printer:string_of_order_list;
  assert_equal [ goog_order ] goog_orders ~printer:string_of_order_list

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
  let orders = orderbook_to_list updated_book "AAPL" "Alice" in
  assert_equal [ order2 ] orders ~printer:string_of_order_list

let test_remove_nonexistent_order _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let updated_book, _ = remove_order empty_book empty_users order in
  let orders = orderbook_to_list updated_book "AAPL" "Alice" in
  assert_equal [] orders ~printer:string_of_order_list

let test_best_bid _ =
  let order1 = make_order B "AAPL" 100 10 "Alice" in
  let order2 = make_order B "AAPL" 105 5 "Bob" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2 ]
  in
  let best_bid_order = best_bid book_with_orders "AAPL" in
  assert_equal order2 best_bid_order

let test_best_ask _ =
  let order1 = make_order S "AAPL" 150 10 "Charlie" in
  let order2 = make_order S "AAPL" 145 5 "Diana" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2 ]
  in
  let best_ask_order = best_ask book_with_orders "AAPL" in
  assert_equal order2 best_ask_order

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
  let retrieved_orders = orderbook_to_list book_with_orders "AAPL" "Alice" in
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
  let retrieved_orders = orderbook_to_list book_with_orders "AAPL" "Alice" in
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
  let alice_orders = orderbook_to_list book_with_orders "AAPL" "Alice" in
  let bob_orders = orderbook_to_list book_with_orders "AAPL" "Bob" in
  assert_equal [ List.hd orders ] alice_orders ~printer:string_of_order_list;
  assert_equal [ List.nth orders 1 ] bob_orders ~printer:string_of_order_list

let test_remove_recently_added_order _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let book_with_order, _ = add_order empty_book empty_users order in
  let updated_book, _ = remove_order book_with_order empty_users order in
  let orders = orderbook_to_list updated_book "AAPL" "Alice" in
  assert_equal [] orders ~printer:string_of_order_list

let test_remove_single_order _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let book_with_order, _ = add_order empty_book empty_users order in
  let updated_book, _ = remove_order book_with_order empty_users order in
  assert_bool "Book should be empty after removing the only order"
    (marketorders_to_list updated_book = [])

let test_orderbook_to_list_empty_book _ =
  assert_bool "No orders should be present in an empty book"
    (orderbook_to_list empty_book "AAPL" "Alice" = [])

let test_marketorders_to_list_empty_book _ =
  assert_bool "No market orders should be present in an empty book"
    (marketorders_to_list empty_book = [])

let suite =
  "Order Book Tests"
  >::: [
         "test_empty" >:: test_empty;
         "test_add_order_to_empty" >:: test_add_order_to_empty;
         "test_add_existing_order" >:: test_add_existing_order;
         "test_add_orders_different_assets" >:: test_add_orders_different_assets;
         "test_remove_order_from_multiple" >:: test_remove_order_from_multiple;
         "test_remove_nonexistent_order" >:: test_remove_nonexistent_order;
         "test_best_bid" >:: test_best_bid;
         "test_best_ask" >:: test_best_ask;
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
       ]

let _ = run_test_tt_main suite
