(* $Id$ *)

open Tactictype
open Mappingfuns
open Name
open Term.Funs

type tactic = Tactictype.tactic

val tacname : term -> name
(* or raise ParseError_ *)
val transTactic : term -> tactic
val explodeForExecute : term -> name * term list
val tacticstring : tactic -> string
(* the simple, unvarnished string *)
val tacticstringwithNLs : tactic -> string
(* guess what this one does *)
val catelim_tacticstring : tactic -> string list -> string list
val catelim_tacticstringwithNLs : tactic -> string list -> string list
val remaptactic : (term, term) mapping -> tactic -> tactic
val isguard : tactic -> bool
val showargasint : (term -> int) option ref
val readintasarg : term array option ref

