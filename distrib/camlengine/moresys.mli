exception Interrupt
val onInterrupt : (unit -> unit) -> (unit -> 'a) -> unit
val execute : string -> string list -> int * in_channel * out_channel
val execute_in_env : string -> string list -> string list -> int * in_channel * out_channel