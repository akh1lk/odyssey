(** abstract data type for propositions *)
type t

(** stores truth values assigned to a given variable *)
type data

(** Raised when a proposition is invalid.  *)
exception InvalidProposition

(* Functionalities we need to implement: parser, printer, evaluator!.  *)

(** [create_data str_list] str_list is [["var1 true/false"; "var2 true"; ... ]] 
returns [data] with truth values assigned to variables. *)
val create_data: string list -> data

(** [parse_prop data str] Parses [str] and returns a proposition. 
Raises: [InvalidProposition] if [prop] is an invalid proposition.*)
val parse_prop: data -> string -> t

(** [eval_prop prop] evaluates [prop] and prints intermediate steps. 
Requires: [prop] is a valid proposition *)
val eval_prop: t -> bool

