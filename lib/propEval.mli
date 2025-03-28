type t 
(** Abstract data type for propositions. *)

type data
(** Stores truth values assigned to a given variable. *)

exception InvalidProposition
(** Raised when a proposition is invalid. *)

exception InvalidData
(**Raised when a data input is invalid.*)

val print_prop : t -> string
(** [print_prop prop] converts the proposition [prop] into a human-readable
    string. *)

val create_data : string list -> data
(** [create_data str_list] takes a list of strings in the format
    [["var1 true/false"; "var2 true"; ...]] and returns a [data] hash table with
    truth values assigned to variables. *)
val add_var: string*bool -> data -> data


val data_to_string: data -> string

val unquantified_variables : data -> t -> string list
(**[unquantified_variables data prop] returns a list of the variables in the proposition that do not have a quantification*)


val parse_prop : string -> t
(** [parse_prop expr] parses the string [expr] and returns a proposition of type
    [t]. Raises: [InvalidProposition] if [expr] is an invalid proposition. *)

val eval_prop : t -> data -> bool
(** [eval_prop prop data] evaluates the proposition [prop] using the truth
    values provided in [data]. Requires: No unquantified variables*)

val latex_of_prop : t -> string
(** [latex_of_prop p] returns a string of LaTeX-interpretable code of the
    proposition. Note that output has two backslashes, so it must be printed or
    parsed when written to a file. *)