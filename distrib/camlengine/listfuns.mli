(* $Id$ *)

(* nonfix _All
   infixr 7 <|
   infixr 6 <*;
   infix  5 </ //;
   infixr 5 doubleslosh />;
   infix  4 ||| slosh;
   infix  0 nonmember member subset;
 *)

val ( <| ) : ('a -> bool) -> 'a list -> 'a list     (* filter *)
val ( <* ) : ('a -> 'b) -> 'a list -> 'b list       (* map *)
val doubleslosh : ('a -> 'b) * ('a -> bool) -> 'a list -> 'b list
val ( </ ) : ('a * 'b -> 'b) * 'b -> 'a list -> 'b
val ( /> ) : 'a * ('a * 'b -> 'a) -> 'b list -> 'a
val ( // ) : ('a * 'a -> 'a) * 'a list -> 'a
exception Reduce
val ( ||| ) : 'a list -> 'b list -> ('a * 'b) list  (* zip = combine *)
exception Zip_ (* unequal lengths *)

val _All : ('a -> bool) -> 'a list -> bool
val member : 'a * 'a list -> bool
val subset : 'a list * 'a list -> bool
val _INTER : 'a list -> 'a list -> 'a list
val nonmember : 'a * 'a list -> bool
val slosh : 'a list * 'a list -> 'a list
val set : 'a list -> 'a list
val seteq : ('a -> 'a -> bool) -> 'a list -> 'a list
val last : 'a list -> 'a
exception Last_
val take : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val zip : 'a list * 'b list -> ('a * 'b) list
(* Bird-Meertens zip, uneven lists allowed *)
val takewhile : ('a -> bool) -> 'a list -> 'a list
val dropwhile : ('a -> bool) -> 'a list -> 'a list
(* get ready for change to proper fold semantics *)
val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val foldl : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
val isprefix : ('a * 'a -> bool) -> 'a list -> 'a list -> bool
val extract : ('a -> bool) -> 'a list -> 'a * 'a list
exception Extract_
val replacenth : 'a list -> int -> 'a -> 'a list
val interpolate : 'a -> 'a list -> 'a list
val catelim_interpolate :
  ('a -> 'b list -> 'b list) -> 'b -> 'a list -> 'b list -> 'b list
val split : ('a -> bool) -> 'a list -> 'a list * 'a list (* yess, nos *)
val sort : ('a -> 'a -> bool) -> 'a list -> 'a list (* given op<, sorts in < order *)
val sortandcombine : ('a -> 'a -> bool) -> ('a * 'a -> 'a) -> 'a list -> 'a list
val remdups : 'a list -> 'a list
val earlierlist : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val sortunique : ('a -> 'a -> bool) -> 'a list -> 'a list
val sorteddiff : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val sortedsame : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val sortedmergeandcombine :
  ('a -> 'a -> bool) -> ('a -> 'a -> 'a) -> 'a list -> 'a list -> 'a list
val sortedmerge : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val sortedlistsub : ('a * 'b -> bool) -> 'a list -> 'b list -> 'a list
val matchbag : ('a -> 'b option) -> 'a list -> ('a * 'b * 'a list) list
val ( >< ) : 'a list -> 'b list -> ('a * 'b) list
val allpairs : 'a list -> ('a * 'a) list
val listsub : ('a * 'b -> bool) -> 'a list -> 'b list -> 'a list
val eqlists : ('a * 'a -> bool) -> 'a list * 'a list -> bool
val eqbags : ('a * 'a -> bool) -> 'a list * 'a list -> bool
val numbered : 'a list -> (int * 'a) list
val toposort : 'a list -> ('a -> 'a list) -> 'a list * 'a list list
(* roots -> dependencies
 * -> (topological sort (roots first), list of cycles) 
 *)

val liststring : ('a -> string) -> string -> 'a list -> string
val liststring2 : ('a -> string) -> string -> string -> 'a list -> string
val bracketedliststring : ('a -> string) -> string -> 'a list -> string
val catelim_liststring :
  ('a -> string list -> string list) -> string -> 'a list ->
    string list -> string list
val catelim_liststring2 :
  ('a -> string list -> string list) -> string -> string -> 'a list ->
    string list -> string list
val catelim_bracketedliststring :
  ('a -> string list -> string list) -> string -> 'a list ->
    string list -> string list
val catelim2stringfn : ('a -> string list -> string list) -> 'a -> string
val stringfn2catelim : ('a -> string) -> 'a -> string list -> string list

