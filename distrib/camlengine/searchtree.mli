(* $Id$ *)

type ('c, 'r) searchtree and ('c, 'r) fsm

type ('r, 's) searchresult = Found of ('r * 's) | NotFound of 's

val addtotree       : ('r * 'r -> bool) -> ('c, 'r) searchtree -> 'c list * 'r * bool
				   -> ('c, 'r) searchtree
val deletefromtree  : ('r * 'r -> bool) -> ('c, 'r) searchtree -> 'c list * 'r * bool
                   -> ('c, 'r) searchtree
val emptysearchtree : (('c * ('c, 'r) fsm) list -> ('c, 'r) fsm -> 'c -> ('c, 'r) fsm)
				   -> ('c, 'r) searchtree
val summarisetree   : ('c, 'r) searchtree -> ('c list * 'r * bool) list

val rootfsm         : ('c, 'r) searchtree ref -> ('c, 'r) fsm
val fsmpos          : ('c, 'r) fsm -> 'c list -> ('c, 'r) fsm option

val scanfsm         : (unit -> 'c) -> ('c, 'r) fsm -> 'c list -> 'c 
                   -> ('r, 'c list) searchresult
val scanstatefsm    : ('a -> 'c) -> ('a -> 'a) -> ('c, 'r) fsm -> 'a 
                   -> ('r, 'a) searchresult
val searchfsm       : ('c, 'r) fsm -> 'c list -> ('r, 'c list) searchresult

val catelim_fsmstring : ('c -> string list -> string list)
					 -> ('r -> string list -> string list) -> ('c, 'r) fsm -> string list
					 -> string list

exception DeleteFromTree_
