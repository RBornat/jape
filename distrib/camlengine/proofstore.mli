(* $Id *)

open Context.Cxt
open Forcedef 
open Name
open Proofstage
open Prooftree.Tree.Fmttree
open Proviso
open Sequent.Funs
open Thing

val saveable : unit -> bool
val saved : unit -> bool
val freezesaved : unit -> unit
val thawsaved : unit -> unit
val saveproof :
  out_channel -> name -> proofstage -> prooftree ->
    proviso list -> seq list -> (seq * model) option -> unit
val saveproofs : out_channel -> unit
val proved : name -> bool
val disproved : name -> bool
val provedordisproved : name -> bool option
val proofnamed :
  name ->
    (bool * prooftree * (bool * proviso) list * seq list * (seq * model) option) option
val addproof :
  (string list -> unit) ->
    (string list * string * string * int -> bool) -> name -> bool ->
    prooftree -> seq list -> cxt -> (seq * model) option -> bool
val clearproofs : unit -> unit
val proofnames : unit -> name list
val thingswithproofs : bool -> name list
val namedthingswithproofs : bool -> name list -> name list

(* bad naming here *)
val needsProof : name -> thing -> bool
val lacksProof : name -> bool
val thmLacksProof : name -> bool
