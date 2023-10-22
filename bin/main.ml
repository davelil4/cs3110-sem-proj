let rec main_loop () =
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
      let o_type = read_line () in
      print_string "Enter Price: ";
      let price = float_of_string (read_line ()) in
      print_string "Enter Quantity: ";
      let quantity = int_of_string (read_line ()) in
      add_order o_type price quantity;
      main_loop ()
  | "2" ->
      print_order_book ();
      main_loop ()
  | "3" ->
      print_endline "Thank you for using the Order Book System!";
      exit 0
  | _ ->
      print_endline "Invalid choice. Try again.";
      main_loop ()

let () = main_loop ()
