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
(* TODO: change this [data] to a hash table that takes in a string and returns
   true or false (to map from variables to truth values)*)

exception InvalidProposition

(* NOTE: do not document these fucntions, documentation already provided in
   propEval. TODO: Implement these functions according to the specification in
   mli*)
let print_prop prop = ()
let create_data (data_list : string list) : data = StringHashtbl.create 1
let parse_prop (expr : string) : t = Var ""

let rec eval_prop (proposition : t) (info : data) =
  match proposition with
  | Var x -> StringHashtbl.find info x
  | Not p -> not (eval_prop p info)
  | And (p1, p2) -> eval_prop p1 info && eval_prop p2 info
  | Or (p1, p2) -> eval_prop p1 info || eval_prop p2 info
  | Implies (p1, p2) -> (not (eval_prop p1 info)) || eval_prop p2 info
