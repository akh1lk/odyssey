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
    | Not p ->
        "¬(" ^ aux p
        ^ ")" (* Always enclose the operand of Not in parentheses *)
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

let parse_prop (expr : string) : t =
  (* Preprocess the input string to add spaces around operators and
     parentheses *)
  let preprocess str =
    str
    |> Str.global_replace (Str.regexp "~") " ~ "
    |> Str.global_replace (Str.regexp "\\^") " ^ "
    |> Str.global_replace (Str.regexp "v") " v "
    |> Str.global_replace (Str.regexp "=>") " => "
    |> Str.global_replace (Str.regexp "(") " ( "
    |> Str.global_replace (Str.regexp ")") " ) "
  in
  let tokens = Str.split (Str.regexp "[ \t]+") (preprocess expr) in
  Printf.printf "Tokens: %s\n" (String.concat " " tokens);
  (* Debug: Print tokens *)
  let rec parse = function
    | [] ->
        Printf.printf "Error: Empty token list\n";
        (* Debug: Empty token list *)
        raise InvalidProposition
    | "~" :: rest ->
        Printf.printf "Parsing NOT operator\n";
        (* Debug: Parsing NOT *)
        let p, rest' = parse rest in
        (Not p, rest')
    | "(" :: rest -> (
        Printf.printf "Parsing expression inside parentheses\n";
        (* Debug: Parentheses *)
        let p1, rest' = parse rest in
        match rest' with
        | "^" :: rest'' -> (
            Printf.printf "Parsing AND operator\n";
            (* Debug: Parsing AND *)
            let p2, rest''' = parse rest'' in
            match rest''' with
            | ")" :: rest'''' -> (And (p1, p2), rest'''')
            | _ ->
                Printf.printf "Error: Missing closing parenthesis after AND\n";
                (* Debug *)
                raise InvalidProposition)
        | "v" :: rest'' -> (
            Printf.printf "Parsing OR operator\n";
            (* Debug: Parsing OR *)
            let p2, rest''' = parse rest'' in
            match rest''' with
            | ")" :: rest'''' -> (Or (p1, p2), rest'''')
            | _ ->
                Printf.printf "Error: Missing closing parenthesis after OR\n";
                (* Debug *)
                raise InvalidProposition)
        | "=>" :: rest'' -> (
            Printf.printf "Parsing IMPLIES operator\n";
            (* Debug: Parsing IMPLIES *)
            let p2, rest''' = parse rest'' in
            match rest''' with
            | ")" :: rest'''' -> (Implies (p1, p2), rest'''')
            | _ ->
                Printf.printf
                  "Error: Missing closing parenthesis after IMPLIES\n";
                (* Debug *)
                raise InvalidProposition)
        | ")" :: rest'' ->
            Printf.printf "Closing parenthesis found\n";
            (* Debug: Closing parenthesis *)
            (p1, rest'')
        | _ ->
            Printf.printf "Error: Invalid token inside parentheses\n";
            (* Debug *)
            raise InvalidProposition)
    | var :: rest ->
        Printf.printf "Parsing variable: %s\n" var;
        (* Debug: Parsing variable *)
        (Var var, rest)
  in
  let prop, remaining = parse tokens in
  if remaining = [] then (
    Printf.printf "Parsing successful: %s\n" (print_prop prop);
    (* Debug: Success *)
    prop)
  else (
    Printf.printf "Error: Unprocessed tokens remaining: %s\n"
      (String.concat " " remaining);
    (* Debug *)
    raise InvalidProposition)

let rec eval_prop (proposition : t) (info : data) =
  match proposition with
  | Var x ->
      Printf.printf "Evaluating variable: %s\n" x;
      (* Debug: Evaluating variable *)
      StringHashtbl.find info x
  | Not p ->
      Printf.printf "Evaluating NOT\n";
      (* Debug: Evaluating NOT *)
      not (eval_prop p info)
  | And (p1, p2) ->
      Printf.printf "Evaluating AND\n";
      (* Debug: Evaluating AND *)
      eval_prop p1 info && eval_prop p2 info
  | Or (p1, p2) ->
      Printf.printf "Evaluating OR\n";
      (* Debug: Evaluating OR *)
      eval_prop p1 info || eval_prop p2 info
  | Implies (p1, p2) ->
      Printf.printf "Evaluating IMPLIES\n";
      (* Debug: Evaluating IMPLIES *)
      (not (eval_prop p1 info)) || eval_prop p2 info
