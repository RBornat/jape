(* $Id$ *)

open Term.Funs
open Sequent.Funs
open Context.Cxt
open Tactic
open Prooftree.Tree.Fmttree
 
type prooftree = Prooftree.Tree.Fmttree.prooftree
 and path = Prooftree.Tree.Fmttree.path
 
(* should this be public? RB *)
type proofstate = Proofstate of staterec
 and staterec = { cxt : cxt; tree : prooftree; givens : seq list;
                  goal : path option; target : path option;
                  root : (prooftree * path) option }

val addautorule : bool * tactic -> unit
val autorules : unit -> (bool * tactic) list
val clearautorules : unit -> unit
val isproven : proofstate -> bool
val nextGoal : bool -> proofstate -> proofstate
val proofstate_cxt : proofstate -> cxt
val proofstate_givens : proofstate -> seq list
val proofstate_goal : proofstate -> path option
val proofstate_root : proofstate -> (prooftree * path) option
val proofstate_target : proofstate -> path option
val proofstate_tree : proofstate -> prooftree
val proofstatestring : bool -> proofstate -> string
val proofstep : cxt -> prooftree -> proofstate -> proofstate option
val prunestate : path -> proofstate -> proofstate
val rewriteproofstate : proofstate -> proofstate
val withcxt : proofstate -> cxt -> proofstate
val withgivens : proofstate -> seq list -> proofstate
val withgoal : proofstate -> path option -> proofstate
val withroot : proofstate -> (prooftree * path) option -> proofstate
val withtarget : proofstate -> path option -> proofstate
val withtree : proofstate -> prooftree -> proofstate
