(* $Id$ *)

type term = Term.Type.term
and seq = Sequent.Type.seq
and rewinf = Prooftree.Tree.rewinf
and element = Term.Type.element
and cxt = Context.Cxt.cxt
and prooftree = Prooftree.Tree.Fmttree.prooftree
and possmatch
and resnum = Term.Type.resnum
and visproviso = Proviso.M.visproviso
and prooftree_step = Prooftree.Tree.prooftree_step
and name = Name.M.name

val applydebug : int ref
val beforeOfferingDo : (unit -> unit) -> unit
val failOfferingDo : (unit -> unit) -> unit
val succeedOfferingDo : (unit -> unit) -> unit

(* apply is now the only matcher:  
 * args are checker, filter, taker, selhyps, selconcs, name, stuff, reason, conjecture, cxt
 *)
val apply :
  (term * term -> cxt -> cxt list) -> (possmatch -> possmatch option) ->
    (possmatch -> 'a option) -> element list -> element list ->
    string * (bool * bool) * prooftree_step * term list *
      (resnum list * resnum list) * seq list * seq * visproviso list ->
    string -> cxt -> seq * rewinf -> 'a option
 (* reason    cxt    problem      *)

(* filters *)
val nofilter : possmatch -> possmatch option
val bymatch : possmatch -> possmatch option
val sameprovisos : possmatch -> possmatch option

(* discriminators *)
val takefirst : possmatch -> (cxt * prooftree) option
val takeonlyone : possmatch -> (cxt * prooftree) option
val offerChoice : possmatch -> (cxt * prooftree) option

(* and one more *)
val takethelot : possmatch -> (cxt * prooftree) list
