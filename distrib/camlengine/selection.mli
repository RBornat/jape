(* $Id$ *)

open Context.Cxt
open Term.Funs

val selection2Subst : bool -> string list -> cxt -> cxt * term
val subterm2subst :
  (term * term -> cxt -> cxt option) -> cxt -> term -> term -> (cxt * term) option

exception Selection_ of string list
