type prop =
  | Var of string
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Implies of prop * prop

module StringHashtbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

type t = prop
type data = bool StringHashtbl.t

exception InvalidProposition

let print_prop prop =
  let rec aux = function
    | Var x -> x
    | Not p -> "¬" ^ aux p ^ ""
    | And (p1, p2) -> "(" ^ aux p1 ^ " ∧ " ^ aux p2 ^ ")"
    | Or (p1, p2) -> "(" ^ aux p1 ^ " ∨ " ^ aux p2 ^ ")"
    | Implies (p1, p2) -> "(" ^ aux p1 ^ " → " ^ aux p2 ^ ")"
  in
  aux prop

let create_data (data_list : string list) : data =
  let table = StringHashtbl.create (List.length data_list) in
  List.iter
    (fun entry ->
      match String.split_on_char ' ' entry with
      | [ var; value ] ->
          StringHashtbl.add table var (String.equal value "true")
      | _ -> raise InvalidProposition)
    data_list;
  table

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

let split_string str =
  let expr_lst = Str.split (Str.regexp " ") str in
  let rec combine_with_parenthesis parenbool combinedelement lst =
    match lst with
    | [] -> []
    | h :: t ->
        if parenbool then
          if h = ")" then combinedelement :: combine_with_parenthesis false "" t
          else
            combine_with_parenthesis true
              (if combinedelement = "" then h else combinedelement ^ " " ^ h)
              t
        else if h = "(" then combine_with_parenthesis true "" t
        else h :: combine_with_parenthesis false "" t
  in
  combine_with_parenthesis false "" expr_lst

let process_string_list lst =
  if List.length lst = 1 then
    match lst with
    | h :: [] -> Str.split (Str.regexp " ") h
    | _ -> failwith "Nah"
  else lst

let parse_prop expr =
  let expression_lst = split_string expr in
  let rec parse_prop_helper expr_lst =
    try
      let proposition = find_prop expr_lst [ "->"; "v"; "^"; "~" ] in
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
      | _ -> raise InvalidProposition
    with _ -> (
      match expr_lst with
      | element :: [] -> Var element
      | _ -> raise InvalidProposition)
  in
  parse_prop_helper expression_lst

let rec eval_prop (proposition : t) (info : data) =
  match proposition with
  | Var x ->
      Printf.printf "Evaluating variable: %s\n" x;
      StringHashtbl.find info x
  | Not p ->
      Printf.printf "Evaluating NOT\n";
      not (eval_prop p info)
  | And (p1, p2) ->
      Printf.printf "Evaluating AND\n";
      eval_prop p1 info && eval_prop p2 info
  | Or (p1, p2) ->
      Printf.printf "Evaluating OR\n";
      eval_prop p1 info || eval_prop p2 info
  | Implies (p1, p2) ->
      Printf.printf "Evaluating IMPLIES\n";
      (not (eval_prop p1 info)) || eval_prop p2 info
