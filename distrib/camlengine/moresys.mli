(* $Id$ *)

type process_id

exception Interrupt

val execute : string -> string list -> process_id * in_channel * out_channel
val execute_in_env : string -> string list -> string list -> process_id * in_channel * out_channel
val onInterrupt : (int -> unit) -> (unit -> 'a) -> unit
val reap : process_id -> unit