(* $Id$ *)

type ('a, 'b) mapping

(* infixr   9   at
   infixr   8   |->  
   infixr   7   ++
   infixr   7   --
 *)

val ( ++ ) : ('a, 'b) mapping * ('a, 'b) mapping -> ('a, 'b) mapping
val ( -- ) : ('a, 'b) mapping * 'a list -> ('a, 'b) mapping
val ( |-> ) : 'a * 'b -> ('a, 'b) mapping
val aslist : ('a, 'b) mapping -> ('a * 'b) list
val at : ('a, 'b) mapping * 'a -> 'b option
val dom : ('a, 'b) mapping -> 'a list
val empty : ('a, 'b) mapping
val formappingpairs : ('a * 'b -> unit) * ('a, 'b) mapping -> unit
val isempty : ('a, 'b) mapping -> bool
val lfold : (('a * 'b) * 'c -> 'c) -> 'c -> ('a, 'b) mapping -> 'c
val mapped : ('a * 'a -> bool) -> ('a, 'b) mapping -> 'a -> 'b option
val mkmap : ('a * 'b) list -> ('a, 'b) mapping
val ran : ('a, 'b) mapping -> 'b list
val rawaslist : ('a, 'b) mapping -> ('a * 'b) list
val rawdom : ('a, 'b) mapping -> 'a list
val rawran : ('a, 'b) mapping -> 'b list
val remapping : ('a * 'b -> 'c * 'd) * ('a, 'b) mapping -> ('c, 'd) mapping

val catelim_mappingstring :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) -> string -> ('a, 'b) mapping ->
    string list -> string list
val mappingstring :
  ('a -> string) -> ('b -> string) -> ('a, 'b) mapping -> string
