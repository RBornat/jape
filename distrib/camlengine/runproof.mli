(* $Id$ *)

open Context.Cxt
open Forcedef
open Japeenv
open Name
open Proofstage
open Proofstate
open Proviso
open Sequent.Funs
open Tactic

val proofsdone : bool ref
val mkstate : visproviso list -> seq list -> prooftree -> proofstate
val startstate : japeenv -> visproviso list -> seq list -> seq -> proofstate
val addproof :
  (string list -> unit) ->
    (string list * string * string * int -> bool) -> name -> bool ->
    proofstate -> (seq * model) option -> bool
val doProof :
  (string list -> unit) ->
    (string list * string * string * int -> bool) -> japeenv -> name ->
    proofstage -> seq -> seq list * proviso list * tactic ->
    (seq * model) option ->
    (name * proofstate * (seq * model) option) option
