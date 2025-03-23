type t
(** abstract data type for propositions *)

type data
(** stores truth values assigned to a given variable *)

exception InvalidProposition
(** Raised when a proposition is invalid. *)

(* Functionalities we need to implement: parser, printer, evaluator!. *)

val create_data : string list -> data
(** [create_data str_list] str_list is [["var1 true/false"; "var2 true"; ... ]]
    returns [data] with truth values assigned to variables. *)

val parse_prop : string -> t
(** [parse_prop data str] Parses [str] and returns a proposition. Raises:
    [InvalidProposition] if [prop] is an invalid proposition.*)

val eval_prop : t -> data -> bool
(** [eval_prop prop] evaluates [prop] and prints intermediate steps. Requires:
    [prop] is a valid proposition *)
