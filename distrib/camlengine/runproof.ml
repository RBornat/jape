(*
	$Id$

    This file is part of the jape proof engine, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*)

open Context.Cxt 
open Context.ExteriorFuns
open Name 
open Proofstage 
open Proofstate
open Sml

exception Use_ = Paragraph.Use_
exception Tacastrophe_ = Miscellaneous.Tacastrophe_

let addproof = Proofstore.addproof
let ( &~~ ) = Optionfuns.( &~~ )
let applyLiteralTactic = Tacticfuns.applyLiteralTactic None
let applyconjectures = Miscellaneous.applyconjectures
let applyTactic = Tacticfuns.applyTactic None

let checkdisproof cxt =
  Disproof.checkdisproof (Facts.facts (Context.Cxt.provisos cxt) cxt)

let compiletoprove = Thing.compiletoprove
let consolereport = Miscellaneous.consolereport
let empty = Mappingfuns.empty
let eqbags = Listfuns.eqbags
let explain = Tacticfuns.explain
let getReason = Reason.getReason
let liststring = Listfuns.liststring
let ( <* ) = Listfuns.( <* )
let maxprovisoresnum = Proviso.maxprovisoresnum
let maxtreeresnum = Prooftree.Tree.Fmttree.maxtreeresnum
let mkReplayTac v = Tactictype.ReplayTac v

let mkTip cxt seq =
  Prooftree.Tree.mkTip cxt seq Treeformat.Fmt.neutralformat

let mkvisproviso = Proviso.mkvisproviso
let proofstage2word = Proofstage.proofstage2word
let proving = Tacticfuns.proving
let provisoactual = Proviso.provisoactual
let provisostring = Proviso.provisostring
let provisovisible = Proviso.provisovisible
let rewriteproofstate = Proofstate.rewriteproofstate
let rewriteProoftree = Prooftree.Tree.rewriteProoftree
let rewriteseq = Rewrite.Funs.rewriteseq
let rootPath = Prooftree.Tree.Fmttree.rootPath
let sequent = Prooftree.Tree.Fmttree.sequent
let seqstring = Sequent.Funs.seqstring
let tacticstring = Tactic.tacticstring
let takethelot = Applyrule.takethelot
let uncurry2 = Miscellaneous.uncurry2
let ( <| ) = Listfuns.( <| )

let proofsdone = ref false

let rec doBEGINPROOF env state =
  match applyLiteralTactic env "IF BEGINPROOF" state with
    Some state' -> state'
  | None -> state

let rec mkstate provisos givens tree =
  let (cxt, tree, uvs) =
    rewriteProoftree givens false
      (withexterior
         (withprovisos newcxt provisos) (givens, sequent tree))
      tree
  in
  Proofstate
    {cxt = withresnum
             (withusedVIDs cxt uvs)
             (nj_fold (uncurry2 max)
                (maxtreeresnum tree ::
                     ((maxprovisoresnum <.> provisoactual) <* provisos)) 1 + 1);
     givens = givens; tree = tree; goal = None; target = None; root = None}

let rec startstate env provisos givens seq =
  let (Proofstate {tree = tree} as state) =
    mkstate provisos givens (mkTip newcxt (rewriteseq newcxt seq))
  in
  (* rewrite necessary - see comment on definition of mkTip *)
  doBEGINPROOF env (withgoal state (Some (rootPath tree)))

let realaddproof = addproof

let rec addproof report query name proved =
  fun (Proofstate {cxt = cxt; tree = tree; givens = givens})
    disproofopt ->
    realaddproof report query name proved tree givens cxt disproofopt
(* at present a proof is recorded in the proof store as a proof tree, and in a 
 * file as a SEQ tactic wich will rebuild that tree.  This works for the time
 * being but is deficient, for a number of reasons:
 *
 * 1. We don't record the stages of the proof.  This could be remedied quite
 *    easily - and at the same time we could record the tactic used to make
 *    each stage-step.
 *
 * 2. It is sensitive to the names used in the rule definitions, and those 
 *    automatically added (like extraBag and the auto variables and ...)
 *
 * RB April 1995
 *)
(* this function gives back Some(name,state,disproof option) just when a proof is in 
   progress.  
   It stores the proof if the proofstage is Complete.
 *)
exception NoProof_ of string list
(* moved out for OCaml *)

let rec doProof report query env name stage seq (givens, pros, tac) disproofopt =
  (* ReplayTac now does all the work of setting up the parameters for a replay *)
  let tac = mkReplayTac tac in
  let (pros', givens, seq) = compiletoprove (pros, givens, seq) in
  let cxt = withprovisos newcxt (mkvisproviso <* pros') in
  let oldapply = !applyconjectures in
  let oldproving = !proving in
  let rec checkfinalprovisos cxt =
    let newpros = (provisoactual <* (provisovisible <| provisos cxt)) in
    let rec showpros pros =
      if null pros then "no provisos"
      else liststring provisostring " AND " pros
    in
    eqbags (uncurry2 (=)) (pros, newpros) ||
    query
      (["The proof of theorem "; namestring name;
        " didn't generate the provisos that were expected: ";
        " the proof originally had "; showpros pros;
        "; when replayed it had "; showpros newpros;
        "  Accept this proof?"],
       "Accept", "Reject", 1)
  in
  let initialstate = startstate env (provisos cxt) givens seq in
  let rec cleanup () =
    applyconjectures := oldapply; proving := oldproving
  in
  let rec label () = proofstage2word stage in
  let rec complain ss =
    report (label () :: " " :: namestring name :: " -- " :: ss)
  in
  let rec checkproof () =
    try
      applyconjectures := true;
      proving := namefrom "";
      proofsdone := true;
      match
        applyLiteralTactic env (parseablenamestring name) initialstate
      with
        None ->
          raise
            (NoProof_
               ("doesn't seem to be the same theorem (the base sequent "
                  ::
                  seqstring seq :: " doesn't match) -- " :: getReason ()))
      | Some st ->
          (* it seems to be a statement of the theorem *)
          match applyTactic env tac initialstate with
            Some (Proofstate {cxt = cxt} as st) ->
              let st = rewriteproofstate st in
              cleanup ();
              Some (st, isproven st && checkfinalprovisos cxt)
          | None -> raise (NoProof_ ("proof fails -- " :: explain ""))
    with
      NoProof_ ss -> cleanup (); complain ss; None
    | Tacastrophe_ ss ->
        cleanup ();
        report ("Error in tactic during " :: label () :: " " :: namestring name :: " -- " :: ss);
        None
    | exn -> cleanup (); raise exn
  in
  (checkproof () &~~
     (fun (state, proved) ->
        let disproved = checkdisproof (proofstate_cxt state) (proofstate_tree state) disproofopt in
        let stage =
	  match stage, proved, disproved with
	    Complete, false, false -> 
	      complain ["is recorded as complete but is neither proved nor disproved"];
	      InProgress
	  | _, _, _ -> stage
	in
        if stage = Complete then
          begin
            let _ = (addproof report query name proved state disproved disproofopt : bool) in
            None
          end
        else Some (name, state, disproofopt)))
