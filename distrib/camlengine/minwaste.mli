(* $Id$ *)

val minwaste : ('a -> int) -> int -> 'a list -> 'a list list
val minwastedebug : bool ref
val resetminwcache : unit -> unit
