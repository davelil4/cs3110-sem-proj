open Order_book

let book1 = match Order_book.empty with a, _ -> a
let users1 = match Order_book.empty with _, b -> b

let rec main_loop (book : t * users) : unit =
  print_endline "=================================";
  print_endline "Welcome to the Order Book System!";
  print_endline "Enter username";
  let name = read_line () in
  let current_book = fst book in
  let current_users = snd book in
  let user_profile =
    match find_user name current_users with
    | None ->
        {
          username = "string";
          order_history = fst empty;
          pending_orders = fst empty;
          profit = 0;
        }
    | Some u -> u
  in
  print_endline "1. Add Order";
  print_endline "2. Remove Order";
  print_endline "3. View Available Assets to Buy";
  print_endline "4. View Order Book";
  print_endline "5. Print Orders for a Specific Asset";
  print_endline "6. View Profile ";
  print_endline "7. View Username";
  print_endline "8. View Order History";
  print_endline "9. View Pending Orders";
  print_endline "10. View Profit";
  print_endline "11. View Users";
  print_endline "12. View Trade Leaderboard";
  print_endline "13. Exit";
  print_endline "=================================";
  print_string "Enter your choice: ";
  let choice = read_line () in
  match choice with
  | "1" ->
      print_string "Enter Order Type (Buy/Sell): ";
      let o_type1 =
        match read_line () with
        | "Buy" -> B
        | "Sell" -> S
        | _ -> raise (Invalid_argument "Invalid Order Type")
      in
      print_string "Enter Asset: ";
      let asset1 = read_line () in
      let user1 = name in
      print_string "Enter Price: ";
      let inp1 = int_of_string (read_line ()) in
      let price1 =
        if inp1 >= 0 then inp1 else raise (Invalid_argument "Negative Price")
      in
      print_string "Enter Quantity: ";
      let inp2 = int_of_string (read_line ()) in
      let quantity1 =
        if inp2 >= 0 then inp2 else raise (Invalid_argument "Negative Quantity")
      in
      let current_book = match book with b, _ -> b in
      let current_users = match book with _, u -> u in
      let o =
        {
          o_type = o_type1;
          asset = asset1;
          price = price1;
          quantity = quantity1;
          user = user1;
        }
      in
      print_endline
        ("Is this the order you would like to add (Y for Yes; N for No): "
       ^ order_to_string o);
      let response1 = read_line () in
      if response1 = "N" then main_loop (current_book, current_users)
      else
        let new_book =
          match
            add_order current_book current_users
              {
                o_type = o_type1;
                asset = asset1;
                price = price1;
                quantity = quantity1;
                user = user1;
              }
          with
          | b, _ -> b
        in
        let new_users =
          match
            add_order current_book current_users
              {
                o_type = o_type1;
                asset = asset1;
                price = price1;
                quantity = quantity1;
                user = user1;
              }
          with
          | _, u -> u
        in
        main_loop (new_book, new_users)
  | "2" ->
      print_string "Enter Order Type (Buy/Sell): ";
      let o_type1 =
        match read_line () with
        | "Buy" -> B
        | "Sell" -> S
        | _ -> raise (Invalid_argument "Invalid Order Type")
      in
      print_string "Enter Asset: ";
      let asset1 = read_line () in
      let user1 = name in
      print_string "Enter Price: ";
      let inp3 = int_of_string (read_line ()) in
      let price1 =
        if inp3 >= 0 then inp3 else raise (Invalid_argument "Negative Price")
      in
      print_string "Enter Quantity: ";
      let inp4 = int_of_string (read_line ()) in
      let quantity1 =
        if inp4 >= 0 then inp4 else raise (Invalid_argument "Negative Quantity")
      in
      let o =
        {
          o_type = o_type1;
          asset = asset1;
          price = price1;
          quantity = quantity1;
          user = user1;
        }
      in
      print_endline
        ("Is this the order you would like to remove (Y for Yes; N for No): "
       ^ order_to_string o);
      let response1 = read_line () in
      if response1 = "N" then main_loop (current_book, current_users)
      else
        let current_book = match book with b, _ -> b in
        let current_users = match book with _, u -> u in
        let new_book =
          match
            remove_order current_book current_users
              {
                o_type = o_type1;
                asset = asset1;
                price = price1;
                quantity = quantity1;
                user = user1;
              }
          with
          | b, _ -> b
        in
        let new_users =
          match
            remove_order current_book current_users
              {
                o_type = o_type1;
                asset = asset1;
                price = price1;
                quantity = quantity1;
                user = user1;
              }
          with
          | _, u -> u
        in
        main_loop (new_book, new_users)
  | "3" ->
      print_endline (available_buys current_book);
      main_loop (current_book, current_users)
  | "4" ->
      print_endline (t_to_string current_book);
      main_loop (current_book, current_users)
  | "5" ->
      print_endline "Which Asset are you looking for?";
      let desired_asset = read_line () in
      print_endline "Buy or Sell Order?";
      let bs = read_line () in
      print_endline (asset_book_to_string current_book desired_asset bs);
      main_loop (current_book, current_users)
  | "6" ->
      print_endline (user_to_string user_profile);
      main_loop (current_book, current_users)
  | "7" ->
      print_endline name;
      main_loop (current_book, current_users)
  | "8" -> (
      match find_user name current_users with
      | None -> raise (Invalid_argument "User not found")
      | Some us ->
          print_endline (t_to_string us.order_history);
          main_loop (current_book, current_users))
  | "9" -> (
      match find_user name current_users with
      | None -> raise (Invalid_argument "User not found")
      | Some us ->
          print_endline (t_to_string us.pending_orders);
          main_loop (current_book, current_users))
  | "10" -> (
      match find_user name current_users with
      | None -> raise (Invalid_argument "User not found")
      | Some us ->
          print_endline (string_of_int us.profit);
          main_loop (current_book, current_users))
  | "11" ->
      print_endline (users_to_string current_users);
      main_loop (current_book, current_users)
  | "12" -> (
      print_endline "Which of the following stats would you like to see: ";
      print_endline "Top Profiter";
      print_endline "Top Three Profiters";
      print_endline "Top Loss";
      print_endline "Top Three Loss";
      print_endline "Top Orderer";
      print_endline "Top Three Orderers";
      print_endline "Bottom Orderer";
      print_endline "Bottom Three Orderers";
      print_endline "Top Pending Orders";
      print_endline "Top Three Pending Orders";
      print_endline "Bottom Pending Orders";
      print_endline "Bottom Three Pending Orders";
      let choice = read_line () in
      match choice with
      | "Top Profiter" ->
          print_endline (fst (get_top_profiter current_users));
          main_loop (current_book, current_users)
      | "Top Three Profiters" ->
          let lis = get_top_3_profiters current_users in
          let str =
            match lis with [ a; b; c ] -> fst a ^ fst b ^ fst c | _ -> ""
          in
          print_endline str;
          main_loop (current_book, current_users)
      | "Top Loss" ->
          print_endline (fst (get_top_loss current_users));
          main_loop (current_book, current_users)
      | "Top Orderer" ->
          print_endline (fst (get_top_orderer current_users));
          main_loop (current_book, current_users)
      | "Top Pending Orders" ->
          print_endline (fst (get_most_pending_orders current_users));
          main_loop (current_book, current_users)
      | "Bottom Pending Orders" ->
          print_endline (fst (get_bottom_orderer current_users));
          main_loop (current_book, current_users)
      | "Top Three Loss" ->
          let lis = get_top_3_loss current_users in
          let str =
            match lis with [ a; b; c ] -> fst a ^ fst b ^ fst c | _ -> ""
          in
          print_endline str;
          main_loop (current_book, current_users)
      | "Top Three Orderers" ->
          let lis = get_top_3_orderers current_users in
          let str =
            match lis with [ a; b; c ] -> fst a ^ fst b ^ fst c | _ -> ""
          in
          print_endline str;
          main_loop (current_book, current_users)
      | "Bottom Three Orderers" ->
          let lis = get_bottom_3_orderers current_users in
          let str =
            match lis with [ a; b; c ] -> fst a ^ fst b ^ fst c | _ -> ""
          in
          print_endline str;
          main_loop (current_book, current_users)
      | "Top Three Pending Orders" ->
          let lis = get_top_3_nr_pending_orders current_users in
          let str =
            match lis with [ a; b; c ] -> fst a ^ fst b ^ fst c | _ -> ""
          in
          print_endline str;
          main_loop (current_book, current_users)
      | "Bottom Three Pending Orders" ->
          let lis = get_bottom_3_nr_pending_orders current_users in
          let str =
            match lis with [ a; b; c ] -> fst a ^ fst b ^ fst c | _ -> ""
          in
          print_endline str;
          main_loop (current_book, current_users)
      | _ ->
          print_endline "";
          main_loop (current_book, current_users))
  | "13" ->
      print_endline "Thank you for using the Order Book System!";
      exit 0
  | _ ->
      print_endline "Invalid choice. Try again.";
      main_loop (book1, users1)

let () = main_loop (book1, users1)
