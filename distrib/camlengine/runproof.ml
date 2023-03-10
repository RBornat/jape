(*
        Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

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

open Cxtfuns 
open Cxtexterior
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
  Disproof.checkdisproof (Facts.facts (Cxtfuns.provisos cxt) cxt)

let compiletoprove = Thing.compiletoprove
let empty = Mappingfuns.empty
let eqbags = Listfuns.eqbags
let explain = Tacticfuns.explain
let getReason = Reason.getReason
let string_of_list = Listfuns.string_of_list
let ( <* ) = Listfuns.( <* )
let maxprovisoresnum = Proviso.maxprovisoresnum
let maxtreeresnum = Prooftree.Tree.Fmttree.maxtreeresnum
let mkReplayTac v = Tactictype.ReplayTac v

let mkTip cxt seq =
  Prooftree.Tree.mkTip cxt seq Treeformat.Fmt.neutralformat

let mkvisproviso = Proviso.mkvisproviso
let word_of_proofstage = Proofstage.word_of_proofstage
let proving = Tacticfuns.proving
let provisoactual = Proviso.provisoactual
let string_of_proviso = Proviso.string_of_proviso
let provisovisible = Proviso.provisovisible
let rewriteproofstate = Proofstate.rewriteproofstate
let rewriteProoftree = Prooftree.Tree.rewriteProoftree
let rewriteseq = Rewrite.rewriteseq
let rootPath = Prooftree.Tree.Fmttree.rootPath
let sequent = Prooftree.Tree.Fmttree.sequent
let string_of_seq = Sequent.string_of_seq
let string_of_tactic = Tactic.string_of_tactic
let takethelot = Applyrule.takethelot
let uncurry2 = Miscellaneous.uncurry2
let ( <| ) = Listfuns.( <| )

let proofsdone = ref false

let runprooftracing = ref false
let consolereport = Miscellaneous.consolereport

let rec doBEGINPROOF env state =
  match applyLiteralTactic env "IF BEGINPROOF" state with
  | Some state' -> state'
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
(* doProof gives back Some(name,state,disproof option) just when a proof is in 
   progress.  
   It stores the proof if the proofstage is Complete.
 *)
exception NoProof_ of string list
(* moved out for OCaml *)

let doProof report query env name stage seq (params, givens, pros, tac) disproofopt =
  (* ReplayTac now does all the work of setting up the parameters for a replay *)
  let tac = mkReplayTac tac in
  let (pros', givens, seq) = compiletoprove (params, pros, givens, seq) in
  let cxt = withprovisos newcxt (mkvisproviso <* pros') in
  let oldapply = !applyconjectures in
  let oldproving = !proving in
  let rec checkfinalprovisos cxt =
    let newpros = (provisoactual <* (provisovisible <| provisos cxt)) in
    let rec showpros pros =
      if null pros then "no provisos"
      else string_of_list string_of_proviso " AND " pros
    in
    eqbags (uncurry2 (=)) (pros, newpros) ||
    query
      (["The proof of theorem "; string_of_name name;
        " didn't generate the provisos that were expected: ";
        " the proof originally had "; showpros pros;
        "; when replayed it had "; showpros newpros;
        "  Accept this proof?"],
       "Accept", "Reject", 1)
  in
  let initialstate = startstate env (provisos cxt) givens seq in
  let rec cleanup () =
    Japeenv.stringset env (name_of_string "applyconjectures") oldapply;
    proving := oldproving
  in
  let rec label () = word_of_proofstage stage in
  let rec complain ss =
    report (label () :: " " :: string_of_name name :: " -- " :: ss)
  in
  (* When we get a proof of a conjecture (or a conjectured rule) then there a number of things we must do.
     
     1. If there are any proofs, stored or otherwise (ooh er!) which depend on a conjecture with the same
        name, then the stored conjecture must be textually the same as the new conjecture. Otherwise those
        proofs have to be deleted.
        
     2. If that test is passed -- this is a new conjecture, or an unused conjecture, or the same as the
        conjecture stored under its name -- then we check the proof.
        
     The check previously carried out -- that the new conjecture was proved by the stored conjecture --
     was wrong because it was backwards, and even then wouldn't be enough, because dependent proofs 
     would have to be remade.
     
     Unstored proofs mess this up, and I can see no way in which they can be communicated to doProof, so
     for the time being, only proofs of currently stored conjectures or novel conjectures will be allowed.
   *)
  let rec checkproof () =
    try
      Japeenv.stringset env (name_of_string "applyconjectures") "all";
      proving := name_of_string "";
      proofsdone := true;
      (* Do we have a version of this rule already? We must have -- see paragraphfuns.ml *)
      let thingkind givens = if givens=[] then "rule" else "theorem" in
      let samething (t_params, t_bpros, t_givens, t_seq) =
        let pk () = ["loaded proof of "; thingkind givens; " "; string_of_name name] in
        let pkdiff s = pk () @ [" has different "; s; " to stored version"] in
        if not (eqbags Sequent.eqseqs (givens, t_givens)) then 
          (if givens=[] || t_givens=[] then 
             report ["loaded proof of "; string_of_name name; " defines a "; thingkind givens; 
                     "; stored version is a "; thingkind t_givens]
           else
             report (pkdiff "premises/GIVENs")
          );
        if not (params=t_params) then
          report (pkdiff "parameters");
        if not (eqbags (uncurry2 (=)) (List.map snd t_bpros, pros)) then
          report (pkdiff "provisos");
        if not (Sequent.eqseqs (t_seq,seq)) then
          report (pkdiff "conclusion");
        true
      in
      let identity_ok =
        match Thing.freshThingtoprove name with
       | Some (Rule ((t_params, t_bpros, t_givens, t_seq as stuff), t_ax)) -> 
           if !runprooftracing then consolereport ["it's a Rule"];
           samething stuff
       | Some (Theorem (t_params, t_bpros, t_seq))   -> 
           if !runprooftracing then consolereport ["it's a Theorem"];
           samething (t_params, t_bpros, [], t_seq)
       | Some (Tactic _)    
       | Some (Macro _)     
       | None               -> true (* if it happens *)
      in
      if identity_ok then
        if !runprooftracing then 
          consolereport ["runproof.checkproof identity ok -- trying proof"];
        (* it seems to be a statement of the theorem *)
        match applyTactic env tac initialstate with
        | Some (Proofstate {cxt = cxt} as st) ->
            let st = rewriteproofstate st in
            cleanup ();
            Some (st, isproven st && checkfinalprovisos cxt)
        | None -> raise (NoProof_ ("proof fails -- " :: explain ""))
    with
    | NoProof_ ss -> cleanup (); complain ss; None
    | Tacastrophe_ ss ->
        cleanup ();
        report ("Error in tactic during " :: label () :: " " :: string_of_name name :: " -- " :: ss);
        None
    | exn -> cleanup (); raise exn
  in
  (checkproof () &~~
     (fun (state, proved) ->
        let disproved = checkdisproof (proofstate_cxt state) (proofstate_tree state) disproofopt in
        let stage =
          match stage, proved, disproved with
          | Complete, false, false -> 
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
