open OUnit2
open Order_book
module StringMap = Map.Make (String)

(* Testing Strategy:
     This test suite aims to validate the correctness of the order
     book system.  It includes aset of tests covering various
     functionalities like adding and removing orders, finding the best bid and
     ask prices, handling multiple assets and users, etc. Tests were developed
     using a combination of black box and glass box testing.

     Black box example:
     test_add_order_to_empty checks whether adding an order to an
     empty book results in that order being the only one in the book.
     This test does not care how the order is added; it only checks the end result.

     Glass box example :
     Tests like test_add_and_remove_multiple_orders and
     test_marketorders_to_list_after_additions that verify the internal state
     of the order book after performing a series of additions and deletions are
     examples of glass box testing. They check the structure of the order book
     after operations, which requires knowledge of how the order book is implemented.


    What's Tested:
       --------------
    1. Adding Orders: Tests cover adding orders to an empty book, adding multiple
    orders for the same or different assets, and adding orders at different prices.

    2. Removing Orders: Tests validate removing recently added orders, handling
    orders from multiple users, and removing orders from books with a single order.

    3. Best Bid/Ask: These tests check for the highest bid and lowest ask,
    handling cases with no orders, multiple orders, and after order removals.

    4. User-related Operations: Tests include finding existing and
    non-existing users, profiling users, and identifying top profiteers and orderers.

    5. Market Orders: Tests validate the listing of all market orders after
    additions and removals, ensuring the correctness of the order book's state.

    6. Special Cases: Additional tests are conducted for edge cases such as
    handling orders with the same price but different quantities and various
    scenarios involving multiple assets.

    7. User Profiles: Tests check functionalities related to user profiles,
    including the retrieval of top profiteers, orderers, and those with
    pending orders. This ensures the system accurately tracks user activities.

    8. Complexity Scenarios: Complex test cases involving multiple orders,
    users, and assets are included to simulate real-world scenarios and ensure
    the system can handle them efficiently.



      Omissions:
      ----------
    - Stress testing with an extremely high volume of orders or users.
    - Although there are no test cases for the interface, we manually tested it
     after the interface was usable


    Justification for Test Suite:
    -----------------------------
    This test suite is designed to ensure thorough validation of the order book
    system. By covering a broad range of functionalities and scenarios,
    we tested the system's response to some complex situations.
    The inclusion of edge cases and user-related operations ensures that the
    system behaves as expected under different circumstances.
    These tests provide a strong foundation for asserting the
    correctness and reliability of the order book system, which makes it effective
    for any future maintenence or additions.

    Conclusion:
    -----------
    The test suite aims to cover a wide range of scenarios,
    balancing the complexity of a real order book with practical limitations.
    It is a key part of ensuring the quality and reliability of the order book
    system. Using these tests we wereable to go back and improve functionality
    multiple times.
*)

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
let is_order_book_empty order_book =
  Order_book.marketorders_to_list order_book = []

let test_empty _ =
  let empty_book, _ = Order_book.empty in
  assert_bool "Order book should be empty" (is_order_book_empty empty_book)

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
  let all_aapl_orders = orderbook_to_list book_with_orders "AAPL" B in
  let alice_orders =
    List.filter (fun order -> order.user = "Alice") all_aapl_orders
  in
  let bob_orders =
    List.filter (fun order -> order.user = "Bob") all_aapl_orders
  in
  assert_equal [ List.hd orders ] alice_orders ~printer:string_of_order_list;
  assert_equal [ List.nth orders 1 ] bob_orders ~printer:string_of_order_list

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
  let order1 = make_order B "GOOG" 100 10 "Alice" in
  let order2 = make_order B "AAPL" 105 5 "Bob" in
  let book_with_orders, users_with_orders =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2 ]
  in
  let updated_book, _ =
    remove_order book_with_orders users_with_orders order1
  in
  let alice_orders = orderbook_to_list updated_book "GOOG" B in
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

let test_remove_order_from_empty_book _ =
  let order = make_order B "AAPL" 100 10 "Alice" in
  let updated_book, _ = remove_order empty_book empty_users order in
  assert_bool "Book should remain empty" (marketorders_to_list updated_book = [])

(******************************best_bid tests**********************************)
let test_best_bid _ =
  let order1 = make_order B "AAPL" 105 5 "Bob" in
  let order2 = make_order B "AAPL" 100 10 "Alice" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2 ]
  in
  match best_bid book_with_orders "AAPL" with
  | Some best_bid_order ->
      Printf.printf "Best bid: %s\n" (string_of_order best_bid_order);
      assert_equal order1 best_bid_order
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

let test_best_bid_after_removal _ =
  let order1 = make_order B "AAPL" 100 10 "Alice" in
  let order2 = make_order B "AAPL" 105 5 "Bob" in
  let order3 = make_order B "AAPL" 110 15 "Charlie" in
  let book_with_orders, users =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2; order3 ]
  in
  let book_after_removal, _ = remove_order book_with_orders users order3 in
  match best_bid book_after_removal "AAPL" with
  | Some best_bid_order -> assert_equal order2 best_bid_order
  | None -> failwith "Expected best bid not found after removal"

let test_best_bid_same_price_different_quantities _ =
  let order1 = make_order B "AAPL" 100 10 "Alice" in
  let order2 = make_order B "AAPL" 100 15 "Bob" in
  let order3 = make_order B "AAPL" 100 5 "Charlie" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2; order3 ]
  in
  match best_bid book_with_orders "AAPL" with
  | Some best_bid_order -> assert_equal order2 best_bid_order
  | None ->
      failwith
        "Expected best bid not found with same price but different quantities"

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

let test_best_ask_after_removal _ =
  let order1 = make_order S "AAPL" 145 5 "Diana" in
  let order2 = make_order S "AAPL" 150 10 "Charlie" in
  let order3 = make_order S "AAPL" 140 20 "Eve" in
  let book_with_orders, users =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2; order3 ]
  in
  let book_after_removal, _ = remove_order book_with_orders users order3 in
  match best_ask book_after_removal "AAPL" with
  | Some best_ask_order -> assert_equal order1 best_ask_order
  | None -> failwith "Expected best ask not found after removal"

let test_best_ask_same_price_different_quantities_lower _ =
  let order1 = make_order S "AAPL" 150 15 "Diana" in
  let order2 = make_order S "AAPL" 150 10 "Charlie" in
  let order3 = make_order S "AAPL" 150 20 "Eve" in
  let book_with_orders, _ =
    List.fold_left
      (fun (book, users) order -> add_order book users order)
      (empty_book, empty_users) [ order1; order2; order3 ]
  in
  match best_ask book_with_orders "AAPL" with
  | Some best_ask_order -> assert_equal order2 best_ask_order
  | None ->
      failwith
        "Expected best ask not found with same price but different quantities"

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

let test_get_top_profiter_no_users _ =
  assert_raises (Failure "No Users") (fun () -> get_top_profiter empty_users)

let test_get_top_profiter_single_user _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users =
    remove_order empty_book users (make_order B "AAPL" 150 10 "Alice")
  in
  let top_user, _ = get_top_profiter users in
  assert_equal "Alice" top_user

let test_get_top_profiter_multiple_users _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users =
    remove_order empty_book users (make_order B "AAPL" 140 10 "Alice")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 20 "Bob")
  in
  let _, users =
    remove_order empty_book users (make_order B "AAPL" 130 20 "Bob")
  in

  let top_user, _ = get_top_profiter users in

  assert_equal "Bob" top_user
(***************************get_top3 profiters*********************************)

let test_get_top_3_profiters_exactly_three _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 20 "Bob")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 30 "Charlie")
  in
  let top_3 = get_top_3_profiters users in
  assert_equal [ "Charlie"; "Bob"; "Alice" ] (List.map fst top_3)

let test_get_top_3_profiters_more_than_three _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 20 "Bob")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 30 "Charlie")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 40 "David")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 50 "Eve")
  in
  let top_3 = get_top_3_profiters users in
  assert_equal [ "Eve"; "David"; "Charlie" ] (List.map fst top_3)

(*************************(get_top_loss test ********************************)
let test_get_top_loss_single_user _ =
  let _, users =
    add_order empty_book empty_users (make_order S "AAPL" 150 10 "Alice")
  in
  let top_user, _ = get_top_loss users in
  assert_equal "Alice" top_user

let test_get_top_loss_multiple_users _ =
  let _, users =
    add_order empty_book empty_users (make_order S "AAPL" 150 10 "Alice")
  in
  let _, users =
    add_order empty_book users (make_order S "AAPL" 150 25 "Charlie")
  in
  let _, users =
    add_order empty_book users (make_order S "AAPL" 150 20 "Bob")
  in
  let top_user, _ = get_top_loss users in
  assert_equal "Charlie" top_user
(*************************(get_top_3 loss test ********************************)

let test_get_top_3_loss_exactly_three _ =
  let _, users =
    add_order empty_book empty_users (make_order S "AAPL" 150 10 "Alice")
  in
  let _, users =
    add_order empty_book users (make_order S "AAPL" 150 20 "Bob")
  in
  let _, users =
    add_order empty_book users (make_order S "AAPL" 150 30 "Charlie")
  in
  let top_3 = get_top_3_loss users in
  assert_equal [ "Charlie"; "Bob"; "Alice" ] (List.map fst top_3)

let test_get_top_3_loss_more_than_three _ =
  let _, users =
    add_order empty_book empty_users (make_order S "AAPL" 150 10 "Alice")
  in
  let _, users =
    add_order empty_book users (make_order S "AAPL" 150 20 "Bob")
  in
  let _, users =
    add_order empty_book users (make_order S "AAPL" 150 30 "Charlie")
  in
  let _, users =
    add_order empty_book users (make_order S "AAPL" 150 40 "David")
  in
  let _, users =
    add_order empty_book users (make_order S "AAPL" 150 50 "Eve")
  in
  let top_3 = get_top_3_loss users in
  assert_equal [ "Eve"; "David"; "Charlie" ] (List.map fst top_3)

(**********************Top_orderer test***************************************)

let test_get_top_orderer_single_user _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let top_user, order_count = get_top_orderer users in
  assert_equal "Alice" top_user;
  assert_equal 1 order_count

let test_get_top_orderer_multiple_users _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 20 "Bob")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 25 "Charlie")
  in
  let top_user, order_count = get_top_orderer users in
  assert_equal "Charlie" top_user;
  assert (order_count > 0)

let test_get_top_3_orderers_exactly_three _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 20 "Bob")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 30 "Charlie")
  in
  let top_3 = get_top_3_orderers users in
  assert_equal [ "Charlie"; "Bob"; "Alice" ] (List.map fst top_3)

(*****************************bottom order tests******************************)
let test_get_bottom_orderer_single_user _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let bottom_user, order_count = get_bottom_orderer users in
  assert_equal "Alice" bottom_user;
  assert_equal 1 order_count

let test_get_bottom_orderer_multiple_users _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users = add_order empty_book users (make_order B "AAPL" 150 5 "Bob") in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 2 "Charlie")
  in
  let bottom_user, _ = get_bottom_orderer users in
  assert_equal "Charlie" bottom_user

let test_get_bottom_3_orderers_exactly_three _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users = add_order empty_book users (make_order B "AAPL" 150 5 "Bob") in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 15 "Charlie")
  in
  let bottom_3 = get_bottom_3_orderers users in
  assert_equal [ "Bob"; "Alice"; "Charlie" ] (List.map fst bottom_3)

let test_get_bottom_3_orderers_more_than_three _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users = add_order empty_book users (make_order B "AAPL" 150 5 "Bob") in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 15 "Charlie")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 150 25 "Diana")
  in
  let bottom_3 = get_bottom_3_orderers users in
  assert_equal [ "Bob"; "Alice"; "Charlie" ] (List.map fst bottom_3)

(**********************************pending orders ****************************)
let test_get_most_pending_orders_single_user _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let top_user, pending_order_count = get_most_pending_orders users in
  assert_equal "Alice" top_user;
  assert_equal 1 pending_order_count

let test_get_most_pending_orders_multiple_users _ =
  let _, users =
    add_order empty_book empty_users (make_order B "AAPL" 150 10 "Alice")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 160 15 "Bob")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 190 20 "Charlie")
  in
  let _, users =
    add_order empty_book users (make_order B "AAPL" 180 20 "Bob")
  in

  let top_user, _ = get_most_pending_orders users in
  assert_equal "Charlie" top_user

let suite =
  "Order Book Tests"
  >::: [
         "test_empty" >:: test_empty;
         "test_add_order_to_empty" >:: test_add_order_to_empty;
         "test_add_existing_order" >:: test_add_existing_order;
         "test_add_orders_different_assets" >:: test_add_orders_different_assets;
         "test_remove_order_from_multiple" >:: test_remove_order_from_multiple;
         "test_best_bid" >:: test_best_bid;
         "test_best_ask" >:: test_best_ask;
         "test_marketorders_to_list_after_additions"
         >:: test_marketorders_to_list_after_additions;
         "test_marketorders_to_list_after_removals"
         >:: test_marketorders_to_list_after_removals;
         "test_add_orders_different_prices" >:: test_add_orders_different_prices;
         "test_add_orders_same_asset_different_users"
         >:: test_add_orders_same_asset_different_users;
         "test_remove_recently_added_order" >:: test_remove_recently_added_order;
         "test_remove_single_order" >:: test_remove_single_order;
         "test_marketorders_to_list_empty_book"
         >:: test_marketorders_to_list_empty_book;
         "test_best_bid_with_no_buy_orders" >:: test_best_bid_with_no_buy_orders;
         "test_best_ask_with_no_sell_orders"
         >:: test_best_ask_with_no_sell_orders;
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
         "test_best_bid_multiple_orders" >:: test_best_bid_multiple_orders;
         "test_get_top_profiter_no_users" >:: test_get_top_profiter_no_users;
         "test_get_top_profiter_single_user"
         >:: test_get_top_profiter_single_user;
         "test_get_top_profiter_multiple_users"
         >:: test_get_top_profiter_multiple_users;
         "test_get_top_3_profiters_exactly_three"
         >:: test_get_top_3_profiters_exactly_three;
         "test_get_top_3_profiters_more_than_three"
         >:: test_get_top_3_profiters_more_than_three;
         "test_get_top_loss_single_user" >:: test_get_top_loss_single_user;
         "test_get_top_loss_multiple_users" >:: test_get_top_loss_multiple_users;
         "test_get_top_3_loss_exactly_three"
         >:: test_get_top_3_loss_exactly_three;
         "test_get_top_3_loss_more_than_three"
         >:: test_get_top_3_loss_more_than_three;
         "test_get_top_orderer_single_user" >:: test_get_top_orderer_single_user;
         "test_get_top_orderer_multiple_user"
         >:: test_get_top_orderer_multiple_users;
         "test_get_top_3_orderers_exactly_three"
         >:: test_get_top_3_orderers_exactly_three;
         "test_get_bottom_orderer_single_user"
         >:: test_get_bottom_orderer_single_user;
         "test_get_bottom_orderer_multiple_users"
         >:: test_get_bottom_orderer_multiple_users;
         "test_get_bottom_3_orderers_exactly_three"
         >:: test_get_bottom_3_orderers_exactly_three;
         "test_get_bottom_3_orderers_more_than_three"
         >:: test_get_bottom_3_orderers_more_than_three;
         "test_get_most_pending_orders_single_user"
         >:: test_get_most_pending_orders_single_user;
         "test_get_most_pending_orders_multiple_users"
         >:: test_get_most_pending_orders_multiple_users;
         "test_best_bid_after_removal" >:: test_best_bid_after_removal;
         "test_best_bid_same_price_different_quantities"
         >:: test_best_bid_same_price_different_quantities;
         "test_best_ask_after_removal" >:: test_best_ask_after_removal;
         "test_best_ask_same_price_different_quantities_lower"
         >:: test_best_ask_same_price_different_quantities_lower;
       ]

let () = run_test_tt_main suite
