open Odyssey.PropEval
open ANSITerminal
open Unix

(* Helper function to clear the terminal screen *)
let clear_screen () =
  print_string [ Reset ]
    "\027[2J\027[H" (* ANSI escape codes to clear the screen *)

(* Helper function to simulate a loading screen *)
let loading_screen message =
  let dots = [ "."; ".."; "..." ] in
  print_string [ cyan ] message;
  List.iter
    (fun dot ->
      print_string [ cyan ] dot;
      flush Stdlib.stdout;
      sleepf 0.5)
    dots;
  print_endline ""

(* Help function to explain how to use the program *)
let print_help () =
  clear_screen ();
  print_endline "Welcome to the Proposition Evaluator!";
  print_endline "You can input a logical proposition to evaluate.";
  print_endline "Supported operators:";
  print_string [ green ] "  ~  : NOT\n";
  print_string [ magenta ] "  ^  : AND\n";
  print_string [ red ] "  v  : OR\n";
  print_string [ blue ] "  => : IMPLIES\n";
  print_endline "Parentheses can be used to group expressions.";
  print_endline "Examples:";
  print_string [ yellow ] "  (x ^ (~y))\n";
  print_string [ cyan ] "  (x => (y v z))\n";
  print_endline
    "If you want to evaluate with variable values, provide them in the format:";
  print_endline "  x true, y false, z true";
  print_endline "Type 'exit' to quit the program."

(* Function to colorize a proposition *)
let colorize_prop prop =
  let rec aux = function
    | Var x -> sprintf [ yellow ] "%s" x
    | Not p -> sprintf [ green ] "¬(%s)" (aux p)
    | And (p1, p2) -> sprintf [ magenta ] "(%s ∧ %s)" (aux p1) (aux p2)
    | Or (p1, p2) -> sprintf [ red ] "(%s ∨ %s)" (aux p1) (aux p2)
    | Implies (p1, p2) -> sprintf [ blue ] "(%s → %s)" (aux p1) (aux p2)
  in
  aux prop

(* Function to evaluate a proposition *)
let evaluate_proposition expr data =
  clear_screen ();
  print_string [ cyan ] "Proposition: ";
  print_endline expr;
  (match data with
  | None -> ()
  | Some data_list ->
      print_string [ cyan ] "Variable Assignments: ";
      print_endline (String.concat ", " data_list));
  loading_screen "Evaluating your proposition";
  try
    let parsed = parse_prop expr in
    let simplified = colorize_prop parsed in
    match data with
    | None ->
        (* If no data is provided, print the simplified proposition *)
        clear_screen ();
        print_string [ cyan ] "Simplified Proposition: ";
        print_endline simplified
    | Some data_list ->
        let data_table = create_data data_list in
        let result = eval_prop parsed data_table in
        clear_screen ();
        print_string [ cyan ] "Evaluation Result: ";
        print_string [ Bold; green ] (if result then "true\n" else "false\n")
  with
  | InvalidProposition ->
      clear_screen ();
      print_string [ red ]
        "Error: Invalid proposition. Please check your input.\n"
  | Not_found ->
      clear_screen ();
      print_string [ red ] "Error: Missing variable values for evaluation.\n"

(* Main REPL loop *)
let rec repl () =
  print_endline
    "\nEnter a proposition (or type 'help' for instructions, 'exit' to quit):";
  print_string [ Bold; yellow ] "> ";
  let input = read_line () in
  match String.trim input with
  | "exit" ->
      clear_screen ();
      print_string [ green ] "Goodbye!\n"
  | "help" ->
      print_help ();
      repl ()
  | _ ->
      print_endline
        "Enter variable values (e.g., 'x true, y false') or press Enter to \
         skip:";
      print_string [ Bold; yellow ] "> ";
      let vars = read_line () in
      let data =
        if String.trim vars = "" then None
        else Some (String.split_on_char ',' vars |> List.map String.trim)
      in
      evaluate_proposition input data;
      repl ()

(* Entry point *)
let () =
  print_help ();
  repl ()
