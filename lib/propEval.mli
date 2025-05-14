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
(** [add_var (var, var_value) data] adds a variable and its truth value to the data table. *)

val data_to_string: data -> string
(** [data_to_string data] converts the data table to a string representation. *)

val unquantified_variables : data -> t -> string list
(**[unquantified_variables data prop] returns a list of the variables in the proposition that do not have a quantification*)

val simplify_prop : t -> data -> t
(** [simplify_prop prop data] simplifies the proposition [prop] using the truth values in [data], 
    keeping unquantified variables as they are. *)

val parse_prop : string -> t
(** [parse_prop expr] parses the string [expr] and returns a proposition of type
    [t]. Raises: [InvalidProposition] if [expr] is an invalid proposition. *)

val eval_prop : t -> data -> bool
(** [eval_prop prop data] evaluates the proposition [prop] using the truth
    values provided in [data]. Requires: No unquantified variables*)

val eval_prop_string: t -> data -> (string * string) list
(** [eval_prop_string prop data] returns a list of (color, message) pairs
    that show each step in evaluating [prop] using [data]. *)

val latex_of_prop : t -> string
(** [latex_of_prop p] returns a string of LaTeX-interpretable code of the
    proposition. Note that output has two backslashes, so it must be printed or
    parsed when written to a file. *)

val latex_of_eval_prop: t -> data -> string
(** [latex_of_eval_prop prop data] returns a LaTeX string that shows the
    evaluation steps of [prop] using [data]. *)

val latex_document_export: t -> data -> unit
(** [latex_document_export prop data] writes a LaTeX file with the proposition
    and its evaluation using [data]. *)

val find_variables : t -> string list
(** [find_variables prop] returns a list of variables present in the proposition [prop]. *)

val find_assignment : t -> (string * bool) list option 
(** [find_assignment prop] returns [Some string * bool list] of variables and
truth values in order to make a proposition satisfiable. If there is no
combinations of T/F variable assigments, it returns [None] *)

val is_satisfiable : t -> bool 
(** [is_satisfiable prop] returns a boolean that says whether a proposition
is satisfiable or not, specifically relating to the boolean satisfiablity
problem. *)

val is_tautology : t -> bool 
(** [is_tautology prop] returns a boolean that says whether a proposition
is a tautology (i.e. always true). *)

val equivalent: t-> t-> bool
(** [equivalent prop] returns a boolean that says whether 2 propositions 
are equivalent *)

val cnf_of_prop : t -> t
(** [cnf_of_prop prop] returns a proposition logically equivalent to [prop] 
    in Conjunctive Normal Form (CNF). *)

val dimacs_of_prop  : t -> string
(** [dimacs_of_prop prop] returns a string representation of the CNF form
of a proposition in DIMACS format, a format recognized by popular SAT Solvers *)