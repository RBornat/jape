(* $Id$ *)

val (<*>) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c (* compose -- can't do without it *)

val chars_of_string : string -> char list
val explode : string -> string list
val fSome : 'a ->'a option
val fst_of_3 : ('a * 'b * 'c) -> 'a
val fst_of_6 : ('a * 'b * 'c * 'd * 'e * 'f) -> 'a
val fst_of_7 : ('a * 'b * 'c * 'd * 'e * 'f * 'g) -> 'a
val implode : string list -> string
val nj_fold : ('b * 'a -> 'a) -> 'b list -> 'a -> 'a
val nj_revfold : ('b * 'a -> 'a) -> 'b list -> 'a -> 'a
val null : 'a list -> bool
val ord : string -> int
val ordof : string -> int -> int
val revapp : ('a -> unit) -> 'a list -> unit
val snd_of_3 : ('a * 'b * 'c) -> 'b
val string_of_chars : char list -> string
val thrd : ('a * 'b * 'c) -> 'c

exception OrdOf_ of string * int
