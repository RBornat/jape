(* $Id$ *)

val currentenv : unit -> string list
val getenv : string -> string -> string (* search acquired environment       *)
val reset : unit -> unit                (* reset to the original environment *)
val setenv : string * string -> unit    (* augment acquired environment      *)
val variables : unit -> string list     (* domain of the environment         *)
