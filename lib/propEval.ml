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

(* TODO: Implement these helper functions: *)

(** [nnf prop] converts [prop] to Negation Normal Form. *)
let nnf prop = Var ""

(** [cnf prop] converts [prop] to Conjunctive Normal Form. *)
let cnf prop = Var ""

(* NOTE: do not document these fucntions, documentation already provided in
   propEval. TODO: Implement these functions according to the specification in
   mli*)
let print_prop prop = ()
let create_data (data_list : string list) : data = StringHashtbl.create 1
let parse_prop (var_data : data) (expr : string) : t = Var ""
let eval_prop (proposition : t) = true
