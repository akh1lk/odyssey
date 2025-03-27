

type t 
(** Abstract data type for propositions. *)

type data
(** Stores truth values assigned to a given variable. *)

exception InvalidProposition
(** Raised when a proposition is invalid. *)

exception InvalidData

val print_prop : t -> string
(** [print_prop prop] converts the proposition [prop] into a human-readable
    string. *)

val create_data : string list -> data
(** [create_data str_list] takes a list of strings in the format
    [["var1 true/false"; "var2 true"; ...]] and returns a [data] hash table with
    truth values assigned to variables. *)
val add_var: string*bool -> data -> data

val unquantified_variables : data -> t -> string list

val parse_prop : string -> t
(** [parse_prop expr] parses the string [expr] and returns a proposition of type
    [t]. Raises: [InvalidProposition] if [expr] is an invalid proposition. *)

val eval_prop : t -> data -> bool
(** [eval_prop prop data] evaluates the proposition [prop] using the truth
    values provided in [data]. Requires: No unquantified variables*)

(*TODO: Add to_string method for DATA!*)
