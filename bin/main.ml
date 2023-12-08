open Order_book

let book1 = match Order_book.empty with a, _ -> a
let users1 = match Order_book.empty with _, b -> b

let rec main_loop (book : t * users) =
  print_endline "=================================";
  print_endline "Welcome to the Order Book System!";
  print_endline "Enter username";
  let name = read_line () in
  let current_book = fst book in
  let current_users = snd book in
  let user_profile = find_user name current_users in
  print_endline "1. Add Order";
  print_endline "2. Remove Order";
  print_endline "3. View Order Book";
  print_endline "4. Print Orders for a Specific Asset";
  print_endline "5. View Profile ";
  print_endline "6. Exit";
  print_endline "=================================";
  print_string "Enter your choice: ";
  let choice = read_line () in
  match choice with
  | "1" ->
      print_string "Enter Order Type (Buy/Sell): ";
      let o_type1 = match read_line () with "Buy" -> B | _ -> S in
      print_string "Enter Asset: ";
      let asset1 = read_line () in
      let user1 = name in
      print_string "Enter Price: ";
      let price1 = int_of_string (read_line ()) in
      print_string "Enter Quantity: ";
      let quantity1 = int_of_string (read_line ()) in
      let current_book = match book with b, u -> b in
      let current_users = match book with b, u -> u in
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
        | b, u -> b
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
        | b, u -> u
      in
      main_loop (new_book, new_users)
  | "2" ->
      print_string "Enter Order Type (Buy/Sell): ";
      let o_type1 = match read_line () with "Buy" -> B | _ -> S in
      print_string "Enter Asset: ";
      let asset1 = read_line () in
      let user1 = name in
      print_string "Enter Price: ";
      let price1 = int_of_string (read_line ()) in
      print_string "Enter Quantity: ";
      let quantity1 = int_of_string (read_line ()) in
      let current_book = match book with b, u -> b in
      let current_users = match book with b, u -> u in
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
        | b, u -> b
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
        | b, u -> u
      in
      main_loop (new_book, new_users)
  | "3" -> print_market_book current_book
  | "4" ->
      print_endline "Which Asset are you looking for?";
      let desired_asset = read_line () in
      print_endline "Buy or Sell Order?";
      let bs = read_line () in
      print_asset_book current_book desired_asset bs
  | "5" -> print_profile user_profile
  | "6" ->
      print_endline "Thank you for using the Order Book System!";
      exit 0
  | _ ->
      print_endline "Invalid choice. Try again.";
      main_loop (book1, users1)

let () = main_loop (book1, users1)
