(* $Id$ *)

open Box
open Displayclass
open Draw
open Sequent.Funs
open Term.Funs

type planclass =
  ElementClass of (element * displayclass) | PunctClass | ReasonClass

val makeelementplan :
  (element -> string) -> displayclass -> element -> pos -> planclass plan
val makeseqplan :
  (element -> string) -> bool -> pos -> seq ->
	planclass plan list * textbox
val planclass2displayclass : planclass -> displayclass
val planclassstring : planclass -> string
val seqdraw : pos -> textbox -> planclass plan list -> unit
val seqelementpos : pos -> textbox -> planclass plan -> pos
