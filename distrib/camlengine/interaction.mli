(* $Id$*)

open Prooftree.Tree.Fmttree
open Proofstate.M
open Hit
open Context.Type
open Displaystate
open Term.Type
open Sequent.Type

type command =
    TextCommand of string list
  | HitCommand of (prooftree * path hit * path sel)

val commandstring : command -> string
val startServer : string * string list -> unit
val abandonServer : unit -> unit
val killServer : unit -> unit
val deadServer : string list -> unit
val runningServer : unit -> bool
val setdisplaystyle : string -> unit
val getdisplaystyle : unit -> string
val showProof :
  displaystate -> path option -> path option -> cxt ->
    prooftree -> bool -> displaystate
val showFocussedProof :
  path option -> cxt -> prooftree -> bool -> displaystate
val refreshProof : displaystate -> unit
val setProvisos : cxt -> unit
val setGivens : seq list -> unit
val showallprovisos : bool ref
val getCommand : displaystate option -> command
val findSelection : displaystate -> path sel option
val findLayoutSelection : displaystate -> hitkind -> path option
(* Drag n drop is moribund, as currently implemented.  Will be redone! *)
val dropsource : element list ref
val droptarget : element list ref
val setComment : string list -> unit
val showState : displaystate -> proofstate -> bool -> displaystate
val printState : Pervasives.out_channel -> proofstate -> bool -> unit
val alterTip :
  displaystate -> cxt -> path -> prooftree ->
    (prooftree * path) option ->
    (bool * path * element) * string list ->
    cxt * element * prooftree
