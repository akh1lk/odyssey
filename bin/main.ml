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
         OR: v \n\
         AND: ^\n\
         IMPLIES: ->\n\
         NOT: ~\n\
         BICONDITIONAL: <->\n\
         Additionally, you must not wrap your entire proposition in Parenthesis:\n\
         '(x v y) -> z' is a valid Proposition \n\
         '((x v y)->z)' is NOT a valid proposition \n\
         If no parentheses are specified then the conventional precedence of \
         operations is followed in mathematics.";
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
    "Please input your variables in exactly the format provided. \n\
    \  'x true, y false, z true' \n";
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

let eval_prop prop data =
  let unquantified_variables =
    Odyssey.PropEval.unquantified_variables data prop
  in
  let string_unquantified_variables =
    List.fold_left (fun x y -> x ^ " " ^ y) "" unquantified_variables
  in
  if List.length unquantified_variables = 0 then
    Odyssey.PropEval.eval_prop prop data
  else (
    print_string [ white ]
      "You have not quantified all of your variables to evaluate the \
       proposition \n";
    print_string [ white ]
      ("Please Quantify these variables in the command menu: \n"
     ^ string_unquantified_variables ^ "\n");
    false)

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
      | _ ->
          print_string [ red ]
            "You do not have a proposition and data. Once you have both, you \
             can evaluate your proposition \n")
  | "Exit" -> ()
  | _ ->
      print_string [ red ]
        "This Input is Unknown. Please Try again with commands exactly as \
         shown! \n";
      user_loop prop data

let () = user_loop None None
