(* $Id$ *)

open Prooftree.Tree.Fmttree
open Proofstate
open Tactic.Funs
open Japeenv
open Displaystate
open Name
open Term.Funs
open Hit

val forceUnify : term list -> proofstate -> proofstate option
val doDropUnify :
  element list -> element list -> proofstate -> proofstate option
val autoStep : bool -> name list -> proofstate -> proofstate option
val selections :
  (path * (element * side option) option * element list *
     ((element * side option) * string list) list *
     (element * string list) list * string list)
    option ref
val applyLiteralTactic :
  displaystate option -> japeenv -> string -> proofstate -> proofstate option
val applyTactic :
  displaystate option -> japeenv -> tactic -> proofstate -> proofstate option
val autoTactics :
  displaystate option -> japeenv -> (bool * tactic) list -> proofstate -> proofstate
val interruptTactic : unit -> unit
val resetcaches : unit -> unit
val explain : string -> string list

val _FINDdebug : bool ref
val _FOLDdebug : bool ref
val proving : name ref
val tacticresult : string ref
val tactictracing : bool ref
val timestotry : int ref
val tryresolution : bool ref
