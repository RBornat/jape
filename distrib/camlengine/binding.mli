(* $Id$ *)

type term = Term.Funs.term

val bindingstructure :
  term -> ((term list * term list * term list) * (term * (int * int)) list * term) option

val addbindingdirective : term list * term list * term list * term -> unit
val clearbindingdirectives : unit -> unit
val bindingdebug : bool ref
