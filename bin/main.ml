open ANSITerminal

let startup_instructions =
  print_string [ cyan ] "Welcome to the Theorem Prover! \n";
  print_string [ cyan ]
    "In this Theorem Prover, you will be able to input propositional \
     statements such as 'x->y' and quantify your variables 'x true, y false \n";
  print_string [ cyan ]
    "Then, the magic happens! We will show you how we evaluate the proposition \
     step by step into a final truth value \n"

let rec prop_input () =
  print_string [ magenta ]
    "Please input a Valid Proposition. If you want instructions on how to do \
     so, type 'Help'. Otherwise, input your proposition \n";
  let user_input = read_line () in
  match user_input with
  | "Help" ->
      print_string [ magenta ]
        "To Input a Valid Proposition, Please type your proposition with the \
         following inputs:\n\
         NOT: ~\n\
         AND: ^ \n\
         OR: v\n\
         IMPLIES: ->\n\
         BICONDITIONAL: <->\n\
         '(x v y) -> z' is a valid Proposition \n\
         If no parentheses are specified then the conventional precedence of \
         operations is followed in mathematics. The order in which the \
         operators are displayed in the instructions is from highest priority \
         to lowest priority.\n\
         Special Commands in the Command Menu:\n\
         'SAT' - check if the proposition is satisfiable (has at least one \
         assignment that makes it true)\n\
         'Tautology' - check if the proposition is always true under all \
         assignments\n\
         'Equivalent' - check if your current proposition is logically \
         equivalent to another proposition you input\n\
         'CNF' - export the proposition to Conjunctive Normal Form (CNF)";
      prop_input ()
  | _ -> (
      try
        let prop = Odyssey.PropEval.parse_prop user_input in
        prop
      with Odyssey.PropEval.InvalidProposition ->
        print_string [ magenta ]
          "Invalid Proposition Given. Please Try Again \n";
        prop_input ())

let rec variable_input () =
  print_string [ blue ]
    "Please input your variables in exactly the format provided. NOTE: Make \
     sure to not put spaces between the variable name and comma! \n\
    \  'x true,y false,z true' \n";
  let user_input = read_line () in
  try
    let data_list = String.split_on_char ',' user_input in
    let data = Odyssey.PropEval.create_data data_list in
    data
  with Odyssey.PropEval.InvalidData ->
    print_string [ blue ]
      "Please input your variables and their values in the format exactly as \
       you see. No additional spacing! \n";
    variable_input ()

let rec variable_add data =
  print_string [ blue ]
    "Please input the variable and its value in exactly the format provided. \n\
    \  'x true' \n";
  let user_input = read_line () in
  let user_list = String.split_on_char ' ' user_input in
  match user_list with
  | [ var; var_value ] -> (
      try
        let data =
          Odyssey.PropEval.add_var (var, bool_of_string var_value) data
        in
        data
      with Odyssey.PropEval.InvalidData ->
        print_string [ blue ]
          "Invalid Input. Please follow the instructions exactly \n";
        variable_add data)
  | _ ->
      print_string [ blue ]
        "Invalid Input. Please follow the instructions exactly \n";
      variable_add data

let style_fromstr str =
  match str with
  | "yellow" -> yellow
  | "red" -> red
  | "green" -> green
  | "magneta" -> magenta
  | "blue" -> blue
  | "cyan" -> cyan
  | "black" -> black
  | "white" -> white
  | _ -> default

let rec print_evalprop_string lst =
  match lst with
  | [] -> ()
  | (color, str) :: t ->
      print_string [ style_fromstr color ] str;
      print_evalprop_string t

let eval_prop prop data =
  let unquantified_variables =
    Odyssey.PropEval.unquantified_variables data prop
  in
  let string_unquantified_variables =
    List.fold_left (fun x y -> x ^ " " ^ y) "" unquantified_variables
  in
  if List.length unquantified_variables = 0 then (
    let final_val = Odyssey.PropEval.eval_prop prop data in
    print_string [ red; Blink; Bold ]
      ("The final value of the proposition is " ^ string_of_bool final_val
     ^ ". \n\nHere is the evaluation proces:\n\n");
    print_evalprop_string (Odyssey.PropEval.eval_prop_string prop data);
    true)
  else (
    print_string [ white ]
      ("Missing variables: " ^ string_unquantified_variables ^ "\n");

    print_string [ yellow ]
      ("Here is the simplified version: "
      ^ Odyssey.PropEval.print_prop (Odyssey.PropEval.simplify_prop prop data)
      ^ "\n");

    print_string [ white ]
      "To fully evaluate this proposition, please quantify the remaining \
       variables in the command menu.\n";

    false)

let latex_of_eval_prop prop data =
  let unquantified_variables =
    Odyssey.PropEval.unquantified_variables data prop
  in
  let string_unquantified_variables =
    List.fold_left (fun x y -> x ^ " " ^ y) "" unquantified_variables
  in
  if List.length unquantified_variables = 0 then
    print_string [ white ] (Odyssey.PropEval.latex_of_eval_prop prop data)
  else
    print_string [ white ]
      ("Missing variables: " ^ string_unquantified_variables ^ "\n");
  true

let latex_document_export prop data =
  let unquantified_variables =
    Odyssey.PropEval.unquantified_variables data prop
  in
  let string_unquantified_variables =
    List.fold_left (fun x y -> x ^ " " ^ y) "" unquantified_variables
  in
  if List.length unquantified_variables = 0 then
    let _ = Odyssey.PropEval.latex_document_export prop data in
    print_string [ white ]
      "Your document has been exported to the working directory of the codebase"
  else
    print_string [ white ]
      ("Missing variables: " ^ string_unquantified_variables ^ "\n");
  true

let rec user_loop (prop : Odyssey.PropEval.t option)
    (data : Odyssey.PropEval.data option) =
  print_string [ green ]
    "Please look at the possible inputs below and type it in exactly as you \
     see it \n";
  print_string [ green ]
    "If you would like to input a proposition, then type 'Prop Input' \n";
  print_string [ green ]
    "If you would like to quantify the variables in your proposition, then \
     type 'Variable Input' \n";
  print_string [ green ]
    "If you would like to add variables to your data table, then type \
     'Variable Add' \n";
  print_string [ green ]
    "If you would like to evaluate your proposition, then type 'Evaluate Prop' \n";
  print_string [ green ]
    "If you want to simplify your proposition based on the variables you have \
     and save that proposition, then type 'Simplify Prop' \n";
  print_string [ green ]
    "If you like to export to LaTex via a copy-pastable string, then type \
     'LaTex Export' \n";
  print_string [ green ]
    ("If you would like to export the evaluation process in a bottom up \
      approach, then type 'LaTex Evaluate Export'" ^ "\n");
  print_string [ green ]
    "If you would like to export the proposition and evaluation process to a \
     LaTex Document, then type 'LaTex Document Export'\n";
  print_string [ green ]
    "If you would like to export to CNF form, then type 'CNF' \n";
  print_string [ green ]
    "If you would like to export to DIMACS format, then type 'DIMACS' \n";
  print_string [ green ]
    "If you would like to check satisfiability, then type 'SAT' \n";
  print_string [ green ]
    "If you would like to check for tautology, then type 'Tautology' \n";
  print_string [ green ]
    "If you would like to check equivalence, then type 'Equivalent' \n";
  print_string [ green ]
    "If you would like to find a valid proposition assignment, then type \
     'Valid Prop' \n";
  print_string [ green ] "If you would like to exit, type 'Exit' \n";
  (match (prop, data) with
  | Some p, Some d ->
      print_string [ red ]
        ("Current Prop: "
        ^ Odyssey.PropEval.print_prop p
        ^ " \nCurrent Data: "
        ^ Odyssey.PropEval.data_to_string d
        ^ " \n")
  | Some p, None ->
      print_string [ red ]
        ("Current Prop: "
        ^ Odyssey.PropEval.print_prop p
        ^ " \nCurrent Data: None" ^ " \n")
  | None, Some d ->
      print_string [ red ]
        ("Current Prop: None" ^ " \nCurrent Data: "
        ^ Odyssey.PropEval.data_to_string d
        ^ " \n")
  | None, None ->
      print_string [ red ] "Current Prop: None \nCurrent Data: None \n");
  let user_input = read_line () in
  match user_input with
  | "Prop Input" ->
      let p = prop_input () in
      user_loop (Some p) None
  | "Variable Input" ->
      let d = variable_input () in
      user_loop prop (Some d)
  | "Variable Add" -> (
      match data with
      | Some d ->
          let new_d = variable_add d in
          user_loop prop (Some new_d)
      | None ->
          print_string [ red ]
            "You currently do not have a data table. Please input one before \
             trying to add data \n";
          user_loop prop data)
  | "Evaluate Prop" -> (
      match (prop, data) with
      | Some p, Some d ->
          let _ = eval_prop p d in
          user_loop prop data
      | Some p, None ->
          (* Create an empty data table and run simplification *)
          let empty_data = Odyssey.PropEval.create_data [] in
          let unquantified =
            Odyssey.PropEval.unquantified_variables empty_data p
          in
          let simplified = Odyssey.PropEval.simplify_prop p empty_data in

          print_string [ white ] "No variables have been quantified yet.\n";
          print_string [ white ]
            ("Missing variables: " ^ String.concat " " unquantified ^ "\n");
          print_string [ yellow ]
            ("Here is the simplified version: "
            ^ Odyssey.PropEval.print_prop simplified
            ^ "\n");
          print_string [ white ]
            "Please quantify variables using 'Variable Input' to evaluate this \
             proposition.\n";

          user_loop prop data
      | None, _ ->
          print_string [ red ]
            "You don't have a proposition to evaluate. Please input one using \
             'Prop Input'.\n";
          user_loop prop data)
  | "LaTeX Export" -> (
      match prop with
      | Some p ->
          print_string [ red ]
            ("Copy Paste this into LaTeX: \n"
            ^ Odyssey.PropEval.latex_of_prop p
            ^ "\n");
          user_loop prop data
      | None ->
          print_string [ red ]
            "You do not have a proposition to export to LaTeX\n";
          user_loop prop data)
  | "LaTeX Evaluate Export" -> (
      match (prop, data) with
      | Some p, Some d ->
          let _ = latex_of_eval_prop p d in
          print_string [ white ] "\n";
          user_loop prop data
      | _ ->
          print_string [ red ]
            "Please make sure you have a proposition and variables before \
             using this feature!\n";
          user_loop prop data)
  | "LaTeX Document Export" -> (
      match (prop, data) with
      | Some p, Some d ->
          let _ = latex_document_export p d in
          print_string [ white ] "\n";
          user_loop prop data
      | _ ->
          print_string [ red ]
            "Please make sure you have a proposition and variables before \
             using this feature!\n";
          user_loop prop data)
  | "CNF" -> (
      match prop with
      | Some p ->
          let cnf = Odyssey.PropEval.cnf_of_prop p in
          print_string [ red ]
            ("CNF form: " ^ Odyssey.PropEval.print_prop cnf ^ "\n");
          user_loop prop data
      | None ->
          print_string [ red ] "no proposition to convert to CNF\n";
          user_loop prop data)
  | "DIMACS" -> (
      match prop with
      | Some p ->
          let dimacs_str = Odyssey.PropEval.dimacs_of_prop p in
          print_string [ red ] (dimacs_str ^ "\n");
          user_loop prop data
      | None ->
          print_string [ red ] "No Proposition to convert to DIMACS\n";
          user_loop prop data)
  | "SAT" -> (
      match prop with
      | Some p ->
          let sat = Odyssey.PropEval.is_satisfiable p in

          print_string [ red ]
            (if sat then "Satisfiable\n" else "Unsatisfiable\n");
          user_loop prop data
      | None ->
          print_string [ red ] "No Proposition to test\n";
          user_loop prop data)
  | "Valid Prop" -> (
      match prop with
      | Some p -> (
          match Odyssey.PropEval.find_assignment p with
          | Some assign ->
              List.iter
                (fun (v, b) ->
                  print_string [ red ] (v ^ "=" ^ string_of_bool b ^ ", "))
                assign;
              print_endline "";
              user_loop prop data
          | None ->
              print_string [ red ] "No Satisfying Assignment\n";
              user_loop prop data)
      | None ->
          print_string [ red ] "No Proposition to test\n";
          user_loop prop data)
  | "Tautology" -> (
      match prop with
      | Some p ->
          let taut = Odyssey.PropEval.is_tautology p in
          print_string [ red ]
            (if taut then "Tautology\n" else "Not a Tautology\n");
          user_loop prop data
      | None ->
          print_string [ red ] "No Proposition to test\n";
          user_loop prop data)
  | "Simplify Prop" -> (
      match (prop, data) with
      | Some p, Some d ->
          user_loop (Some (Odyssey.PropEval.simplify_prop p d)) data
      | _ -> user_loop prop data)
  | "Equivalent" -> (
      match prop with
      | Some p ->
          print_string [ magenta ]
            "Enter a Second Proposition for Equivalence:\n";
          let q = prop_input () in
          let equal = Odyssey.PropEval.equivalent p q in
          print_string [ red ]
            (if equal then "Equivalent\n" else "Not Equivalent\n");
          user_loop prop data
      | None ->
          print_string [ red ] "No Proposition to test\n";
          user_loop prop data)
  | "Exit" -> ()
  | _ ->
      print_string [ red ]
        "This Input is Unknown. Please Try again with commands exactly as \
         shown! \n";
      user_loop prop data

let () = user_loop None None
