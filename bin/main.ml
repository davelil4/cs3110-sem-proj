open Order_book
let book1 = Order_book.empty
let rec main_loop book1  =
  print_endline "=================================";
  print_endline "Welcome to the Order Book System!";
  print_endline "1. Add Order";
  print_endline "2. View Order Book";
  print_endline "3. Exit";
  print_endline "=================================";
  print_string "Enter your choice: ";
  let choice = read_line () in
  match choice with
  | "1" ->
      print_string "Enter Order Type (Buy/Sell): ";
      let o_type1 = match read_line () with 
      | "Buy" -> B 
      | _ -> S
    in
      print_string "Enter Asset: ";
      let asset1 = Stock (read_line ()) in
      print_string "Enter Name: ";
      let user1 = read_line () in
      print_string "Enter Price: ";
      let price1 = int_of_string (read_line ()) in
      print_string "Enter Quantity: ";
      let quantity1 = int_of_string (read_line ()) in
      (*
      print_endline o_type1;
      print_endline user1;
      print_endline (string_of_int quantity1);
      print_endline (string_of_float price1);
      *)
      let new_book = add_order book1
      {o_type = o_type1; asset = asset1; price = price1; quantity = quantity1; 
      user = user1} in
      main_loop new_book
  | "2" ->
      (*
      print_endline "print orderbook test";
      print_book 
      *)
      (*
      let string_order_type ot = (match ot with
    | B -> "Buy"
    | S -> "Sell" )
  in 

    let string_of_asset a = (match a with 
    | Stock b -> b
    | Option b -> b )
  in 
      let print_order o = (Printf.printf "Order Type: %s, 
      Asset: %s, 
      Price: %d, 
      Quantity: %d, 
      User: %s\n" 
      (string_order_type o.o_type) (string_of_asset o.asset) o.price 
      o.quantity o.user) in 
      let rec print_book book = (match book with
    | [] -> Printf.printf "Hey"
    | h :: [] -> print_order h
    | h :: t -> print_order h; print_endline "--------------"; print_book t)
  in 
  *)
      print_book book1;
      main_loop book1
  | "3" ->
      print_endline "Thank you for using the Order Book System!";
      exit 0
  | _ ->
      print_endline "Invalid choice. Try again.";
      main_loop book1

let () = main_loop book1
