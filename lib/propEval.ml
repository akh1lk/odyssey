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
(** AF: [t] is a concrete tree representing an abstract propositional logic
    formula. Each constructor models a logical connective or variable; For ex,
    [And (Var "x", Var "y")] represents "x ∧ y". RI: all constructors are
    combined in syntactically valid ways. *)

type data = bool StringHashtbl.t
(** AF: [data] maps variable names (strings) to boolean truth values. RI: each
    variable appears max once with value [true] or [false]. *)

exception InvalidProposition
exception InvalidData

let rec print_prop = function
  | Var x -> x
  | Not p -> "~(" ^ print_prop p ^ ")"
  | And (p1, p2) -> "(" ^ print_prop p1 ^ " ^ " ^ print_prop p2 ^ ")"
  | Or (p1, p2) -> "(" ^ print_prop p1 ^ " v " ^ print_prop p2 ^ ")"
  | Implies (p1, p2) -> "(" ^ print_prop p1 ^ " -> " ^ print_prop p2 ^ ")"
  | Biconditional (p1, p2) ->
      "(" ^ print_prop p1 ^ ") <-> (" ^ print_prop p2 ^ ")"

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

let rec split_expr_helper lst find_symbol left_list =
  match lst with
  | [] -> raise InvalidProposition
  | element :: rest ->
      if element = find_symbol then (List.rev left_list, rest)
      else split_expr_helper rest find_symbol (element :: left_list)

let split_list_by_prop (expr_lst : string list) (find_symbol : string) =
  split_expr_helper expr_lst find_symbol []

let rec find_prop_aux lst prop =
  match lst with
  | [] -> false
  | h :: t -> if h = prop then true else find_prop_aux t prop

let rec find_prop lst prop_lst =
  match prop_lst with
  | [] -> raise InvalidProposition
  | h :: t -> if find_prop_aux lst h then h else find_prop lst t

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

let rec extract_helper lst =
  match lst with
  | h :: [] -> []
  | h :: t -> h :: extract_helper t
  | _ -> raise InvalidProposition

let extract_lst lst =
  match lst with
  | h :: t -> extract_helper t
  | _ -> raise InvalidProposition

let clean_initial_parenthesis (lst : string list) =
  match lst with
  | [] -> raise InvalidProposition
  | h :: t ->
      if h = "(" then
        let parenpairs = countparen t 0 in
        if parenpairs = 0 then extract_lst lst else lst
      else lst

let preprocess_string str =
  let lst = tokenizer str in
  clean_initial_parenthesis lst

let rec combine_w_paren parenbool combinedelement lst parencount =
  match lst with
  | [] -> []
  | h :: t ->
      if parenbool then
        if h = ")" then
          if parencount = 0 then combinedelement :: combine_w_paren false "" t 0
          else
            combine_w_paren true (combinedelement ^ " " ^ h) t (parencount - 1)
        else if h = "(" then
          combine_w_paren true (combinedelement ^ " " ^ h) t (parencount + 1)
        else
          combine_w_paren true
            (if combinedelement = "" then h else combinedelement ^ " " ^ h)
            t parencount
      else if h = "(" then combine_w_paren true "" t 0
      else h :: combine_w_paren false "" t 0

let split_string str =
  let expr_lst = preprocess_string str in
  combine_w_paren false "" expr_lst 0

let process_string_list lst =
  if List.length lst = 1 then
    match lst with
    | h :: [] -> split_string h
    | _ -> raise InvalidProposition
  else lst

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

let parse_prop expr = parse_prop_helper (split_string expr)

let rec find_var_helper (prop : t) =
  match prop with
  | Var x -> [ x ]
  | Not p -> find_var_helper p
  | And (p1, p2) -> find_var_helper p1 @ find_var_helper p2
  | Or (p1, p2) -> find_var_helper p1 @ find_var_helper p2
  | Implies (p1, p2) -> find_var_helper p1 @ find_var_helper p2
  | Biconditional (p1, p2) -> find_var_helper p1 @ find_var_helper p2

let find_variables (prop : t) =
  List.sort_uniq String.compare (find_var_helper prop)

let rec unquantified_variables_helper data lst =
  match lst with
  | [] -> []
  | h :: t -> (
      try
        let _ = StringHashtbl.find data h in
        unquantified_variables_helper data t
      with _ -> h :: unquantified_variables_helper data t)

let unquantified_variables data prop =
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
      "Evaluating variable $" ^ x ^ "$ = "
      ^ string_of_bool (StringHashtbl.find info x)
      ^ "\\\\"
  | Not p1 ->
      latex_of_eval_prop p1 info ^ "\n" ^ "Evaluating $\\lnot("
      ^ latex_of_prop_with_prec p1 3
      ^ ")$ = "
      ^ string_of_bool (eval_prop p info)
      ^ "\\\\"
  | And (p1, p2) ->
      latex_of_eval_prop p1 info ^ "\n" ^ latex_of_eval_prop p2 info ^ "\n"
      ^ "Evaluating $"
      ^ latex_of_prop_with_prec p 2
      ^ "$ = "
      ^ string_of_bool (eval_prop p info)
      ^ "\\\\"
  | Or (p1, p2) ->
      latex_of_eval_prop p1 info ^ "\n" ^ latex_of_eval_prop p2 info ^ "\n"
      ^ "Evaluating $"
      ^ latex_of_prop_with_prec p 1
      ^ "$ = "
      ^ string_of_bool (eval_prop p info)
      ^ "\\\\"
  | Implies (p1, p2) ->
      latex_of_eval_prop p1 info ^ "\n" ^ latex_of_eval_prop p2 info ^ "\n"
      ^ "Evaluating $"
      ^ latex_of_prop_with_prec p 0
      ^ "$ = "
      ^ string_of_bool (eval_prop p info)
      ^ " \\\\"
  | Biconditional (p1, p2) ->
      latex_of_eval_prop p1 info ^ "\n" ^ latex_of_eval_prop p2 info ^ "\n"
      ^ "Evaluating $"
      ^ latex_of_prop_with_prec p 0
      ^ "$ = "
      ^ string_of_bool (eval_prop p info)
      ^ "\\\\"

let latex_document_export prop info =
  (*Accessed https://ocaml.org/docs/file-manipulation to learn how to output our
    string to a file. 5/14/2025 (note citation is also in AUTHORS.md) *)
  let file = "PropositionLatex.tex" in
  let output =
    "\\documentclass{article}\n\n\
     \\usepackage{graphicx}\n\n\
     \\usepackage{amsmath}\n\n\
     \\title{Proposition Document}\n\n\n\n\
     \\begin{document}\n\n\n\
     \\maketitle\n\n\n\
     \\section{Proposition}\n" ^ latex_of_prop prop
    ^ "\n\\section{Evaluation}\n"
    ^ latex_of_eval_prop prop info
    ^ "\n\\end{document}"
  in
  let () =
    let oc = open_out file in
    Printf.fprintf oc "%s\n" output;
    close_out oc
  in
  ()

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

(* Helper: distribute Or over And until no Or is above And anywhere *)

(** [distribute prop] fully distributes Or over And until no Or is above And
    anywhere. *)
let rec distribute prop =
  let rec changed p =
    match p with
    | Or (p1, And (q, r)) ->
        let left = changed (Or (p1, q)) in
        let right = changed (Or (p1, r)) in
        changed (And (left, right))
    | Or (And (q, r), p2) ->
        let left = changed (Or (q, p2)) in
        let right = changed (Or (r, p2)) in
        changed (And (left, right))
    | And (p1, p2) -> And (changed p1, changed p2)
    | Or (p1, p2) -> Or (changed p1, changed p2)
    | p -> p
  in
  let rec fix f x =
    let x' = f x in
    if x' = x then x else fix f x'
  in
  fix changed prop

(** Flatten nested Ors into a list of literals *)
let rec flatten_or = function
  | Or (p1, p2) -> flatten_or p1 @ flatten_or p2
  | p -> [ p ]

(** Flatten nested Ands into a list of conjuncts *)
let rec flatten_and = function
  | And (p1, p2) -> flatten_and p1 @ flatten_and p2
  | p -> [ p ]

(** Step 1.4: Takes in a proposition and converts it to fully flattened CNF form
*)
let cnf_of_prop p =
  let after_dist = p |> eliminate_implies |> to_nnf |> distribute in
  (* 1. flatten to list of conjuncts *)
  let clauses = flatten_and after_dist in
  (* 2. for each clause, flatten Ors and rebuild as flat Or tree *)
  let flat_clauses =
    List.map
      (fun clause ->
        match flatten_or clause with
        | [] -> failwith "Empty CNF clause"
        | hd :: tl -> List.fold_left (fun acc c -> Or (acc, c)) hd tl)
      clauses
  in
  (* 3. rebuild CNF as a binary And tree of flat clauses *)
  match flat_clauses with
  | [] -> failwith "Empty CNF"
  | hd :: tl -> List.fold_left (fun acc c -> And (acc, c)) hd tl

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
let equivalent a b = is_tautology (Biconditional (a, b))

(* Step 3: Convert to DIMACs for External SAT Solver *)

(* signed var for clause representation in dimacs *)
type svar =
  | Pos of string
  | Neg of string

(* Flatten a CNF clause into a list of signed literals, ensuring flat
   disjunctions *)
let rec flatten_or_literals = function
  | Or (p1, p2) -> flatten_or_literals p1 @ flatten_or_literals p2
  | Not (Var x) -> [ Neg x ]
  | Var x -> [ Pos x ]
  | p -> failwith ("Invalid literal in CNF clause: " ^ print_prop p)

let rec clauses_of = function
  | And (p, q) -> clauses_of p @ clauses_of q
  | p ->
      let clause = flatten_or_literals p in
      if clause = [] then failwith "Invalid clause structure for DIMACS"
      else [ clause ]

let cnf_clauses prop = prop |> cnf_of_prop |> clauses_of

(* converts cnf clause to a dimacs string *)
let dimacs_of_clauses clauses : string =
  (* var name to a DIMACS int id *)
  let tbl = Hashtbl.create 16 in
  let nxt_id = ref 1 in

  (* helper to lookup or create new id *)
  let get_id v =
    match Hashtbl.find_opt tbl v with
    | Some i -> i
    | None ->
        (* add new var to hashtable *)
        let ind = !nxt_id in
        Hashtbl.add tbl v ind;
        incr nxt_id;
        ind
  in
  (* get all var names from clauses *)
  let _ =
    List.iter
      (fun clause ->
        List.iter
          (function
            | Pos v -> ignore (get_id v)
            | Neg v -> ignore (get_id v))
          clause)
      clauses
  in

  (* header *)
  let numvars = Hashtbl.length tbl in
  let numclauses = List.length clauses in
  let header =
    "p cnf " ^ string_of_int numvars ^ " " ^ string_of_int numclauses
  in

  (* helper for converting a clause to cnf style *)
  let clause_to_dimacs clause =
    clause
    |> List.map (function
         | Pos v -> string_of_int (get_id v)
         | Neg v -> "-" ^ string_of_int (get_id v))
    |> fun vs -> String.concat " " vs ^ " 0"
  in

  (* convert all clauses *)
  let clause_lines = List.map clause_to_dimacs clauses in
  String.concat "\n" (header :: clause_lines)

let dimacs_of_prop prop = prop |> cnf_clauses |> dimacs_of_clauses

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
