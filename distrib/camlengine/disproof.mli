(* $Id$ *)

type element = Term.Funs.element
and facts = Facts.facts
and forcedef = Forcedef.forcedef
and model = Forcedef.model
and path = Prooftree.Tree.Fmttree.path
and prooftree = Prooftree.Tree.Fmttree.prooftree
and seq = Seqdraw.M.seq
and term = Term.Funs.term
and universe

val catelim_universestring : string -> universe -> string list -> string list
val universestring : string -> universe -> string

val emptyworld : unit -> universe
val isemptyworld : universe -> bool
val addworldlabel : universe -> int * int -> term -> universe option
val deleteworldlabel : universe -> int * int -> term -> universe option

(* semantics *)
val addforcedef : term * forcedef -> unit
val clearforcedefs : unit -> unit
val hasforcedefs : unit -> bool
(* and when I can work out how to fix termstring ...
   val analyse : term -> world -> string
   val seq_analyse : seq -> world -> string list * string * string list
 *)

(* states of the interaction *)
type disproofstate
val catelim_disproofstatestring :
  disproofstate -> string list -> string list
val disproofstatestring : disproofstate -> string
val disproofstate_seq : disproofstate -> seq
val disproofstate_universe : disproofstate -> universe
val disproofstate_selected : disproofstate -> (int * int) list
val disproofstate_conclusive : disproofstate -> bool
val disproofstate_countermodel : disproofstate -> bool

(* because of the need for facts when evaluating, these functions don't evaluate.
 * So you should use evaldisproofstate once you've set it up
 *)
val withdisproofuniverse : disproofstate -> universe -> disproofstate
val newtile : disproofstate -> term -> disproofstate option
val deleteworld : disproofstate -> int * int -> disproofstate option
val worldselect :
  disproofstate -> (int * int) list -> disproofstate option
val addchild : universe -> int * int -> int * int -> universe option
val moveworld :
  disproofstate -> int * int -> int * int -> disproofstate option
val deletelink : universe -> int * int -> int * int -> universe option
val evaldisproofstate :
  facts -> prooftree -> disproofstate -> disproofstate
val disproof_start :
  facts -> prooftree -> path option -> element list ->
    disproofstate
val disproof_minimal : disproofstate option -> bool
(* models and disproofstates *)
val disproofstate2model : disproofstate option -> (seq * model) option
val model2disproofstate :
  facts -> prooftree -> (seq * model) option ->
    disproofstate option
val checkdisproof :
  facts -> prooftree -> (seq * model) option -> bool
exception Disproof_ of string list
(* stuff to display disproofs *)
val showdisproof : disproofstate -> unit
val cleardisproof : unit -> unit
val disproofdebug : bool ref
