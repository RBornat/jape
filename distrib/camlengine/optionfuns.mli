(* $Id$ *)

val (&~) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option  (* andthen  *)
val (&~~) : 'a option -> ('a -> 'b option) -> 'b option               (* andthenr *)
val (|~) : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option  (* ortry    *)
val (|~~) : 'a option -> (unit -> 'a option) -> 'a option             (* ortryr   *)

val _The : 'a option -> 'a                              (* _The None raises None_ *)
val anyway : ('a -> 'a option) -> 'a -> 'a
val failpt : ('a -> 'a option) -> 'a -> 'a
val findbest : ('a -> 'b option) -> ('b -> 'b -> 'b) -> 'a list -> 'b option
val findfirst : ('a -> 'b option) -> 'a list -> 'b option
val opt2bool : 'a option -> bool
val optioncompose : ('b -> 'c) * ('a -> 'b option) -> 'a -> 'c option
val optionfilter : ('a -> 'b option) -> 'a list -> 'b list
val optionfold : ('a * 'b -> 'b option) -> 'a list -> 'b -> 'b option
val optionmap : ('a -> 'b option) -> 'a list -> 'b list option
val optordefault : 'a option * 'a -> 'a
val somef : ('a -> 'a option) -> 'a -> 'a option
val stripoption : 'a option option -> 'a option
val try__ : ('a -> 'b) -> 'a option -> 'b option

(* save space when rewriting structures *)
val option_rewrite2 : ('a -> 'a option) -> ('b -> 'b option) 
                   -> 'a * 'b -> ('a * 'b) option
val option_rewrite3 : ('a -> 'a option) -> ('b -> 'b option) 
                   -> ('c -> 'c option) -> 'a * 'b * 'c -> ('a * 'b * 'c) option
val option_rewritelist : ('a -> 'a option) -> 'a list -> 'a list option

val catelim_optionstring : ('a -> string list -> string list) 
                        -> 'a option -> string list -> string list
val optionstring : ('a -> string) -> 'a option -> string

exception None_
