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
    | None -> raise (Invalid_argument "Invalid Username")
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
  print_endline "12. Exit";
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
      let price1 =
        if int_of_string (read_line ()) >= 0 then int_of_string (read_line ())
        else raise (Invalid_argument "Negative Price")
      in
      print_string "Enter Quantity: ";
      let quantity1 =
        if int_of_string (read_line ()) >= 0 then int_of_string (read_line ())
        else raise (Invalid_argument "Negative Quantity")
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
      let price1 =
        if int_of_string (read_line ()) >= 0 then int_of_string (read_line ())
        else raise (Invalid_argument "Negative Price")
      in
      print_string "Enter Quantity: ";
      let quantity1 =
        if int_of_string (read_line ()) >= 0 then int_of_string (read_line ())
        else raise (Invalid_argument "Negative Quantity")
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
  | "3" -> print_endline (available_buys current_book)
  | "4" -> print_endline (t_to_string current_book)
  | "5" ->
      print_endline "Which Asset are you looking for?";
      let desired_asset = read_line () in
      print_endline "Buy or Sell Order?";
      let bs = read_line () in
      print_endline (asset_book_to_string current_book desired_asset bs)
  | "6" -> print_endline (user_to_string user_profile)
  | "7" -> print_endline name
  | "8" -> (
      match find_user name current_users with
      | None -> print_endline ""
      | Some us -> print_endline (t_to_string us.order_history))
  | "9" -> (
      match find_user name current_users with
      | None -> print_endline ""
      | Some us -> print_endline (t_to_string us.pending_orders))
  | "10" -> (
      match find_user name current_users with
      | None -> print_endline ""
      | Some us -> print_endline (string_of_int us.profit))
  | "11" -> print_endline (users_to_string current_users)
  | "12" ->
      print_endline "Thank you for using the Order Book System!";
      exit 0
  | _ ->
      print_endline "Invalid choice. Try again.";
      main_loop (book1, users1)

let () = main_loop (book1, users1)
