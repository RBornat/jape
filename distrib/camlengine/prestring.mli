(* $Id$ *)

type prestring =
  Prestr of string | Prestrs of string list | Prepres of prestring list

val pre_List : ('a -> prestring) -> 'a list -> prestring
val pre_Set : ('a -> prestring) -> 'a list -> prestring
val pre_Tuple : ('a -> prestring) -> 'a list -> prestring
val pre__comma : prestring
val pre__nil : prestring
val pre__space : prestring
val pre_app : (string -> unit) -> prestring -> unit
val pre_array : ('a -> prestring) -> 'a array -> prestring
val pre_bool : bool -> prestring
val pre_implode : prestring -> string
val pre_int : int -> prestring
val pre_list : ('a -> prestring) -> 'a list -> prestring
val pre_option : ('a -> prestring) -> 'a option -> prestring
val pre_real : float -> prestring
val pre_string : string -> prestring
val pre_unit : unit -> prestring
val pre_vector : ('a -> prestring) -> 'a array -> prestring
