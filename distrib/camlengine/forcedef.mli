(* $Id$ *)

open Sequent.Type
open Term.Type

type forcedef = ForcePrim of term
              | ForceBracket of forcedef
              | ForceAnd of (forcedef * forcedef)
              | ForceOr of (forcedef * forcedef)
              | ForceImplies of (forcedef * forcedef)
              | ForceEverywhere of forcedef
              | ForceNowhere of forcedef
              | ForceAll of (term * term list * forcedef)
              | ForceSome of (term * term list * forcedef)

val catelim_forcedefstring : forcedef -> string list -> string list
val forcedefstring : forcedef -> string

val existsinforcedef : (term -> bool) -> forcedef -> bool
val findinforcedef : (term -> 'a option) -> forcedef -> 'a option
val mapforcedefterms : (term -> term option) -> forcedef -> forcedef
val parseForceDef : unit -> forcedef

type coordinate = Coord of (int * int)

and world = World of (coordinate * coordinate list * term list)

and model = Model of world list

val catelim_modelstring : (seq * model) option -> string list -> string list
val parsemodel : unit -> (seq * model) option
