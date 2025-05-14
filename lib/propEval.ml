open ANSITerminal

type prop =
  | Var of string
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Implies of prop * prop
  | Biconditional of prop * prop

module StringHashtbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

type t = prop
type data = bool StringHashtbl.t

exception InvalidProposition
exception InvalidData

let print_prop prop =
  let rec aux = function
    | Var x -> x
    | Not p -> "¬(" ^ aux p ^ ")"
    | And (p1, p2) -> "(" ^ aux p1 ^ " ^ " ^ aux p2 ^ ")"
    | Or (p1, p2) -> "(" ^ aux p1 ^ " v " ^ aux p2 ^ ")"
    | Implies (p1, p2) -> "(" ^ aux p1 ^ " -> " ^ aux p2 ^ ")"
    | Biconditional (p1, p2) -> "(" ^ aux p1 ^ ") <-> (" ^ aux p2 ^ ")"
  in
  aux prop

let create_data (data_list : string list) : data =
  let table = StringHashtbl.create (List.length data_list) in
  (try
     List.iter
       (fun entry ->
         match String.split_on_char ' ' entry with
         | [ var; value ] ->
             StringHashtbl.add table var (String.equal value "true")
         | _ -> raise InvalidProposition)
       data_list
   with _ -> raise InvalidData);
  table

let add_var (var, var_value) data =
  try
    StringHashtbl.add data var var_value;
    data
  with _ -> raise InvalidData

let data_to_string data =
  StringHashtbl.fold
    (fun x y acc -> "(" ^ x ^ " : " ^ string_of_bool y ^ ") " ^ acc)
    data ""

let split_list_by_prop (expr_lst : string list) (find_symbol : string) =
  let rec split_expr_helper lst find_symbol left_list =
    match lst with
    | [] -> raise InvalidProposition
    | element :: rest ->
        if element = find_symbol then (List.rev left_list, rest)
        else split_expr_helper rest find_symbol (element :: left_list)
  in
  split_expr_helper expr_lst find_symbol []

let rec find_prop_aux lst prop =
  match lst with
  | [] -> false
  | h :: t -> if h = prop then true else find_prop_aux t prop

let rec find_prop lst prop_lst =
  match prop_lst with
  | [] -> raise InvalidProposition
  | h :: t -> if find_prop_aux lst h then h else find_prop lst t

let rec preprocess_lst lst =
  match lst with
  | [] -> []
  | h :: t ->
      if String.length h = 2 then
        Str.string_before h 1 :: Str.string_after h 1 :: preprocess_lst t
      else h :: preprocess_lst t

let rec t_aux acc = function
  | [] -> acc
  | "<->" :: t -> t_aux (acc @ [ "<->" ]) t
  | "->" :: t -> t_aux (acc @ [ "->" ]) t
  | s :: t ->
      t_aux (acc @ List.map (String.make 1) (List.of_seq (String.to_seq s))) t

(** [tokenizer s] returns s split into variables, parentheses and operators.
    Example: [tokenizer "(x-> y) <-> z"] returns
    ["("; "x"; "->"; "y"; ")"; "<->"; "z"]*)
let tokenizer s =
  Str.(full_split (regexp "\\(->\\|<->\\)") s)
  |> List.map (function Str.Text t | Str.Delim t -> t)
  |> t_aux []
  |> List.filter (fun s -> s <> " ")

let rec countparen lst count =
  match lst with
  | [] -> raise InvalidProposition
  | h :: [] -> count
  | h :: t ->
      if count >= 0 then
        if h = "(" then countparen t (count + 1)
        else if h = ")" then countparen t (count - 1)
        else countparen t count
      else -100

let extract_lst lst =
  let rec extract_helper lst =
    match lst with
    | h :: [] -> []
    | h :: t -> h :: extract_helper t
    | _ -> raise InvalidProposition
  in
  match lst with
  | h :: t -> extract_helper t
  | _ -> raise InvalidProposition

let clean_intial_parenthesis (lst : string list) =
  match lst with
  | [] -> raise InvalidProposition
  | h :: t ->
      if h = "(" then
        let parenpairs = countparen t 0 in
        if parenpairs = 0 then extract_lst lst else lst
      else lst

let preprocess_string str =
  let lst = tokenizer str in
  clean_intial_parenthesis lst

let split_string str =
  let expr_lst = preprocess_string str in
  let rec combine_with_parenthesis parenbool combinedelement lst parencount =
    match lst with
    | [] -> []
    | h :: t ->
        if parenbool then
          if h = ")" then
            if parencount = 0 then
              combinedelement :: combine_with_parenthesis false "" t 0
            else
              combine_with_parenthesis true
                (combinedelement ^ " " ^ h)
                t (parencount - 1)
          else if h = "(" then
            combine_with_parenthesis true
              (combinedelement ^ " " ^ h)
              t (parencount + 1)
          else
            combine_with_parenthesis true
              (if combinedelement = "" then h else combinedelement ^ " " ^ h)
              t parencount
        else if h = "(" then combine_with_parenthesis true "" t 0
        else h :: combine_with_parenthesis false "" t 0
  in
  combine_with_parenthesis false "" expr_lst 0

let process_string_list lst =
  if List.length lst = 1 then
    match lst with
    | h :: [] -> split_string h
    | _ -> raise InvalidProposition
  else lst

let parse_prop expr =
  let expression_lst = split_string expr in
  let rec parse_prop_helper expr_lst =
    try
      let proposition = find_prop expr_lst [ "<->"; "->"; "v"; "^"; "~" ] in
      let unprocessed_left, unprocessed_right =
        split_list_by_prop expr_lst proposition
      in
      let left = process_string_list unprocessed_left in
      let right = process_string_list unprocessed_right in
      match proposition with
      | "->" -> Implies (parse_prop_helper left, parse_prop_helper right)
      | "v" -> Or (parse_prop_helper left, parse_prop_helper right)
      | "^" -> And (parse_prop_helper left, parse_prop_helper right)
      | "~" -> Not (parse_prop_helper (left @ right))
      | "<->" -> Biconditional (parse_prop_helper left, parse_prop_helper right)
      | _ -> raise InvalidProposition
    with _ -> (
      match expr_lst with
      | element :: [] -> Var element
      | _ -> raise InvalidProposition)
  in
  parse_prop_helper expression_lst

let rec find_variables_helper (prop : t) =
  match prop with
  | Var x -> [ x ]
  | Not p -> find_variables_helper p
  | And (p1, p2) -> find_variables_helper p1 @ find_variables_helper p2
  | Or (p1, p2) -> find_variables_helper p1 @ find_variables_helper p2
  | Implies (p1, p2) -> find_variables_helper p1 @ find_variables_helper p2
  | Biconditional (p1, p2) ->
      find_variables_helper p1 @ find_variables_helper p2

let find_variables (prop : t) =
  List.sort_uniq String.compare (find_variables_helper prop)

let unquantified_variables data prop =
  let rec unquantified_variables_helper data lst =
    match lst with
    | [] -> []
    | h :: t -> (
        try
          let _ = StringHashtbl.find data h in
          unquantified_variables_helper data t
        with _ -> h :: unquantified_variables_helper data t)
  in
  unquantified_variables_helper data (find_variables prop)

let rec eval_prop (proposition : t) (info : data) =
  match proposition with
  | Var x -> StringHashtbl.find info x
  | Not p ->
      let final_value = eval_prop p info in
      not final_value
  | And (p1, p2) ->
      let prop1 = eval_prop p1 info in
      let prop2 = eval_prop p2 info in
      prop1 && prop2
  | Or (p1, p2) ->
      let prop1 = eval_prop p1 info in
      let prop2 = eval_prop p2 info in
      prop1 || prop2
  | Implies (p1, p2) ->
      let prop1 = eval_prop p1 info in
      let prop2 = eval_prop p2 info in
      (not prop1) || prop2
  | Biconditional (p1, p2) ->
      let prop1 = eval_prop (Implies (p1, p2)) info in
      let prop2 = eval_prop (Implies (p2, p1)) info in
      prop1 = prop2

let rec eval_prop_string proposition info =
  match proposition with
  | Var x ->
      let var_value = StringHashtbl.find info x in
      [
        ( "yellow",
          "Evaluating Variable: " ^ x ^ " to " ^ string_of_bool var_value
          ^ " \n \n" );
      ]
  | Not p ->
      let first_str =
        [ ("red", "Evaluating '~' Statement (" ^ print_prop p ^ "): \n\n") ]
      in
      let intermediate_str = eval_prop_string p info in
      let pval = eval_prop p info in
      let final_value = not pval in
      let final_str =
        if final_value then [ ("red", "~" ^ print_prop p ^ " is True \n\n") ]
        else [ ("red", "~" ^ print_prop p ^ " is False \n\n") ]
      in
      first_str @ intermediate_str @ final_str
  | And (p1, p2) ->
      let first_str =
        [
          ( "green",
            "Evaluating '^' Statement (" ^ print_prop p1 ^ ") ^ ("
            ^ print_prop p2 ^ "): \n\n" );
        ]
      in
      let intermediate_str1 = eval_prop_string p1 info in
      let intermediate_str2 = eval_prop_string p2 info in
      let intermediate_str = intermediate_str1 @ intermediate_str2 in
      let p1val = eval_prop p1 info in
      let p2val = eval_prop p2 info in
      let final_value = p1val && p2val in
      let final_str =
        if final_value then
          [ ("green", print_prop (And (p1, p2)) ^ " is True \n\n") ]
        else [ ("green", print_prop (And (p1, p2)) ^ " is False \n\n") ]
      in
      first_str @ intermediate_str @ final_str
  | Or (p1, p2) ->
      let first_str =
        [
          ( "magenta",
            "Evaluating 'v' Statement (" ^ print_prop p1 ^ ") v ("
            ^ print_prop p2 ^ "): \n\n" );
        ]
      in
      let intermediate_str1 = eval_prop_string p1 info in
      let intermediate_str2 = eval_prop_string p2 info in
      let intermediate_str = intermediate_str1 @ intermediate_str2 in
      let p1val = eval_prop p1 info in
      let p2val = eval_prop p2 info in
      let final_value = p1val || p2val in
      let final_str =
        if final_value then
          [ ("magenta", print_prop (Or (p1, p2)) ^ " is True \n\n") ]
        else [ ("magenta", print_prop (Or (p1, p2)) ^ " is False \n\n") ]
      in
      first_str @ intermediate_str @ final_str
  | Implies (p1, p2) ->
      let first_str =
        [
          ( "blue",
            "Evaluating '->' Statement: (" ^ print_prop p1 ^ ") -> ("
            ^ print_prop p2 ^ "): \n\n" );
        ]
      in
      let intermediate_str1 = eval_prop_string p1 info in
      let intermediate_str2 = eval_prop_string p2 info in
      let intermediate_str = intermediate_str1 @ intermediate_str2 in
      let p1val = eval_prop p1 info in
      let p2val = eval_prop p2 info in
      let final_value = (not p1val) || p2val in
      let final_str =
        if final_value then
          [ ("blue", print_prop (Implies (p1, p2)) ^ " is True \n\n") ]
        else [ ("blue", print_prop (Implies (p1, p2)) ^ " is False \n\n") ]
      in
      first_str @ intermediate_str @ final_str
  | Biconditional (p1, p2) ->
      let first_str =
        [
          ( "blue",
            "Evaluating '<->' Statement: "
            ^ print_prop (Biconditional (p1, p2))
            ^ " \n\n" );
        ]
      in
      let intermediate_str1 = eval_prop_string (Implies (p1, p2)) info in
      let intermediate_str2 = eval_prop_string (Implies (p2, p1)) info in
      let intermediate_str = intermediate_str1 @ intermediate_str2 in
      let p1val = eval_prop (Implies (p1, p2)) info in
      let p2val = eval_prop (Implies (p2, p1)) info in
      let final_value = p1val = p2val in
      let final_str =
        if final_value then
          [ ("blue", print_prop (Biconditional (p1, p2)) ^ " is True \n\n") ]
        else
          [ ("blue", print_prop (Biconditional (p1, p2)) ^ " is False \n\n") ]
      in
      first_str @ intermediate_str @ final_str

(* Hierarchy of prop (high->low): Not=3, And=2, Or=2, Implies=1 *)
let rec latex_of_prop_with_prec (p : prop) (prec : int) : string =
  let wrap needed s = if needed then "(" ^ s ^ ")" else s in
  match p with
  | Var x -> x
  | Not p1 ->
      let inner = latex_of_prop_with_prec p1 3 in
      "\\lnot " ^ inner
  | And (p1, p2) ->
      let s1 = latex_of_prop_with_prec p1 2 in
      let s2 = latex_of_prop_with_prec p2 2 in
      wrap (prec > 2) (s1 ^ " \\land " ^ s2)
  | Or (p1, p2) ->
      let s1 = latex_of_prop_with_prec p1 1 in
      let s2 = latex_of_prop_with_prec p2 1 in
      wrap (prec > 1) (s1 ^ " \\lor " ^ s2)
  | Implies (p1, p2) ->
      let s1 = latex_of_prop_with_prec p1 0 in
      let s2 = latex_of_prop_with_prec p2 0 in
      wrap (prec > 0) (s1 ^ " \\rightarrow " ^ s2)
  | Biconditional (p1, p2) ->
      let s1 = latex_of_prop_with_prec p1 0 in
      let s2 = latex_of_prop_with_prec p2 0 in
      wrap (prec > 0) (s1 ^ " \\leftrightarrow " ^ s2)

let latex_of_prop p = "$" ^ latex_of_prop_with_prec p 0 ^ "$"

let rec latex_of_eval_prop p info =
  match p with
  | Var x ->
      "\\text{Evaluating variable } $" ^ x ^ "$ = "
      ^ string_of_bool (StringHashtbl.find info x)
      ^ "\n"
  | Not p1 ->
      latex_of_eval_prop p1 info ^ "\n" ^ "\\text{Evaluating } $\\lnot("
      ^ latex_of_prop_with_prec p1 3
      ^ ")$ = "
      ^ string_of_bool (eval_prop p info)
      ^ "\n"
  | And (p1, p2) ->
      latex_of_eval_prop p1 info ^ "\n" ^ latex_of_eval_prop p2 info ^ "\n"
      ^ "\\text{Evaluating } $"
      ^ latex_of_prop_with_prec p 2
      ^ "$ = "
      ^ string_of_bool (eval_prop p info)
      ^ "\n"
  | Or (p1, p2) ->
      latex_of_eval_prop p1 info ^ "\n" ^ latex_of_eval_prop p2 info ^ "\n"
      ^ "\\text{Evaluating } $"
      ^ latex_of_prop_with_prec p 1
      ^ "$ = "
      ^ string_of_bool (eval_prop p info)
      ^ "\n"
  | Implies (p1, p2) ->
      latex_of_eval_prop p1 info ^ "\n" ^ latex_of_eval_prop p2 info ^ "\n"
      ^ "\\text{Evaluating } $"
      ^ latex_of_prop_with_prec p 0
      ^ "$ = "
      ^ string_of_bool (eval_prop p info)
      ^ " \n"
  | Biconditional (p1, p2) ->
      latex_of_eval_prop p1 info ^ "\n" ^ latex_of_eval_prop p2 info ^ "\n"
      ^ "\\text{Evaluating } $"
      ^ latex_of_prop_with_prec p 0
      ^ "$ = "
      ^ string_of_bool (eval_prop p info)
      ^ " \n"

(* Implementation of SAT Solver Below *)

(* CNF Form Converter (for real SAT export) *)

(** Step 1.1: change P->Q to ~P v Q *)
let rec eliminate_implies = function
  | Var x -> Var x
  | Not p -> Not (eliminate_implies p)
  | And (p1, p2) -> And (eliminate_implies p1, eliminate_implies p2)
  | Or (p1, p2) -> Or (eliminate_implies p1, eliminate_implies p2)
  | Implies (p1, p2) -> Or (Not (eliminate_implies p1), eliminate_implies p2)
  | Biconditional (p1, p2) ->
      let a = eliminate_implies p1 in
      let b = eliminate_implies p2 in
      And (Or (Not a, b), Or (Not b, a))

(** Step 1.2: change eliminated implies to nnf. Requires eliminate_implies is
    called before. Outputs a prop w/ only NOT, AND and OR*)
let rec to_nnf = function
  | Var x -> Var x
  | Not (Not p) -> to_nnf p
  | Not (And (p1, p2)) -> Or (to_nnf (Not p1), to_nnf (Not p2))
  | Not (Or (p1, p2)) -> And (to_nnf (Not p1), to_nnf (Not p2))
  | Not p -> Not (to_nnf p)
  | And (p1, p2) -> And (to_nnf p1, to_nnf p2)
  | Or (p1, p2) -> Or (to_nnf p1, to_nnf p2)
  (* assume no impls or biconds are here *)
  | p -> p

(** Step 1.3: NNF to CNF by distributing OR over AND *)
let rec distribute_or = function
  (* the main distributing work *)
  | Or (p, And (q, r)) ->
      And (distribute_or (Or (p, q)), distribute_or (Or (p, r)))
  | Or (And (q, r), p) ->
      And (distribute_or (Or (q, p)), distribute_or (Or (r, p)))
  (* keep rest the same *)
  | And (p1, p2) -> And (distribute_or p1, distribute_or p2)
  | Or (p1, p2) -> Or (distribute_or p1, distribute_or p2)
  | p -> p

(** Step 1.4: Takes in a proposition and converts it to CNF form *)
let cnf_of_prop p = p |> eliminate_implies |> to_nnf |> distribute_or

(* Step 2: Brute Force SAT (Satisfiability) Solver *)

(** Step 2.1: truth tables for all variable assignments *)
let rec truth_tables = function
  | [] -> [ [] ]
  | v :: vs ->
      let rest = truth_tables vs in
      List.concat
        [
          List.map (fun r -> (v, true) :: r) rest;
          List.map (fun r -> (v, false) :: r) rest;
        ]

(** Step 2.2: Converts a truthtable to data. *)
let data_of_truthtables truthtables =
  let tbl = StringHashtbl.create (List.length truthtables) in
  List.iter (fun (v, b) -> StringHashtbl.add tbl v b) truthtables;
  tbl

(** Step 2.3: Find a valid assignment for a prop *)
let find_assignment prop =
  (* generate all T/F combos of the vars in prop *)
  let vars = find_variables prop in
  let combos = truth_tables vars in
  (* find a valid assignment *)
  List.find_opt
    (fun assignment ->
      let tfdata = data_of_truthtables assignment in

      eval_prop prop tfdata)
    combos

(** Step 2.4: SAT Solver, returns whether a prop is satisfiable *)
let is_satisfiable prop =
  match find_assignment prop with
  | None -> false
  | Some x -> true

let is_tautology prop = not (is_satisfiable (Not prop))
let equivalent a b = is_satisfiable (Biconditional (a, b))

(** [simplify_prop prop data] simplifies a proposition by substituting
    quantified variables. *)
let rec simplify_prop prop data =
  let try_get_value var =
    try Some (StringHashtbl.find data var) with Not_found -> None
  in
  match prop with
  | Var x -> (
      match try_get_value x with
      | Some true -> Var x
      | Some false -> Var x
      | None -> Var x)
  | Not p -> (
      let simplified_p = simplify_prop p data in
      match simplified_p with
      | Var x -> (
          match try_get_value x with
          | Some true -> Not (Var x)
          | Some false -> Not (Var x)
          | None -> Not simplified_p)
      | Not inner -> inner
      | _ -> Not simplified_p)
  | And (p1, p2) -> (
      let simplified_p1 = simplify_prop p1 data in
      let simplified_p2 = simplify_prop p2 data in
      match (simplified_p1, simplified_p2) with
      | Var x, Var y when x = y -> Var x
      | Var x, _ when try_get_value x = Some false -> Var x
      | _, Var y when try_get_value y = Some false -> Var y
      | Var x, p when try_get_value x = Some true -> p
      | p, Var y when try_get_value y = Some true -> p
      | p1, p2 -> And (p1, p2))
  | Or (p1, p2) -> (
      let simplified_p1 = simplify_prop p1 data in
      let simplified_p2 = simplify_prop p2 data in
      match (simplified_p1, simplified_p2) with
      | Var x, Var y when x = y -> Var x
      | Var x, _ when try_get_value x = Some true -> Var x
      | _, Var y when try_get_value y = Some true -> Var y
      | Var x, p when try_get_value x = Some false -> p
      | p, Var y when try_get_value y = Some false -> p
      | p1, p2 -> Or (p1, p2))
  | Implies (p1, p2) -> (
      let simplified_p1 = simplify_prop p1 data in
      let simplified_p2 = simplify_prop p2 data in
      match (simplified_p1, simplified_p2) with
      | Var x, _ when try_get_value x = Some false -> Var "true"
      | _, Var y when try_get_value y = Some true -> Var "true"
      | Var x, p when try_get_value x = Some true -> p
      | p, Var y when try_get_value y = Some false -> Not p
      | p1, p2 when p1 = p2 -> Var "true"
      | p1, p2 -> Implies (p1, p2))
  | Biconditional (p1, p2) -> (
      let simplified_p1 = simplify_prop p1 data in
      let simplified_p2 = simplify_prop p2 data in
      match (simplified_p1, simplified_p2) with
      | p1, p2 when p1 = p2 -> Var "true"
      | Var x, Var y
        when try_get_value x = Some true && try_get_value y = Some true ->
          Var "true"
      | Var x, Var y
        when try_get_value x = Some false && try_get_value y = Some false ->
          Var "true"
      | Var x, Var y
        when (try_get_value x = Some true && try_get_value y = Some false)
             || (try_get_value x = Some false && try_get_value y = Some true) ->
          Var "false"
      | Var x, p when try_get_value x = Some true -> p
      | p, Var y when try_get_value y = Some true -> p
      | Var x, p when try_get_value x = Some false -> Not p
      | p, Var y when try_get_value y = Some false -> Not p
      | p1, p2 -> Biconditional (p1, p2))
