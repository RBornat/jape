(* $Id$ *)

module type T =
  sig
    type cxt
    and japeenv
    and model
    and name
    and proofstate
    and 'a prooftree
    and seq
    and tactic
    and treeformat
    and proofstage
    and proviso
    and visproviso
    val proofsdone : bool ref
    val mkstate :
      visproviso list -> seq list -> treeformat prooftree -> proofstate
    val startstate :
      japeenv -> visproviso list -> seq list -> seq -> proofstate
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
  end
(* $Id$ *)

module M : T =
  struct
    open Context open Name open Proofstage open Proofstate

           type model = Forcedef.M.model
and possmatch = Applyrule.M.possmatch
and japeenv = Japeenv.M.japeenv
           
           exception Use_ = Paragraph.M.Use_
exception Tacastrophe_ = Miscellaneous.M.Tacastrophe_
           
           let addproof = Proofstore.M.addproof
           let ( &~~ ) = Optionfuns.M.( &~~ )
           let applyLiteralTactic = Tacticfuns.M.applyLiteralTactic None
           let applyconjectures = Miscellaneous.M.applyconjectures
           let applyTactic = Tacticfuns.M.applyTactic None
           let rec checkdisproof cxt =
  Disproof.M.checkdisproof (Facts.M.facts (Context.Cxt.provisos cxt) cxt)
           let compiletoprove = Thing.M.compiletoprove
           let consolereport = Miscellaneous.M.consolereport
           let empty = Mappingfuns.M.empty
           let eqbags = Listfuns.M.eqbags
           let explain = Tacticfuns.M.explain
           let getReason = Reason.M.getReason
           let liststring = Listfuns.M.liststring
           let ( <* ) = Listfuns.M.( <* )
           let maxprovisoresnum = Proviso.M.maxprovisoresnum
           let maxtreeresnum = Prooftree.Tree.Fmttree.maxtreeresnum
           let mkReplayTac = Tactic.Funs.ReplayTac
           let mkSimpleApplyTac = Tactic.Funs.SimpleApplyTac
           
           let rec mkTip cxt seq =
  Prooftree.Tree.mkTip cxt seq Treeformat.M.neutralformat
           
           let mkUniqueTac = Tactic.Funs.UniqueTac
           let mkvisproviso = Proviso.M.mkvisproviso
           let proofstage2word = Proofstage.M.proofstage2word
           let proving = Tacticfuns.M.proving
           let provisoactual = Proviso.M.provisoactual
           let provisostring = Proviso.M.provisostring
           let provisovisible = Proviso.M.provisovisible
           let rewriteproofstate = Proofstate.M.rewriteproofstate
           let rewriteProoftree = Prooftree.Tree.rewriteProoftree
           let rewriteseq = Rewrite.Funs.rewriteseq
           let rootPath = Prooftree.Tree.Fmttree.rootPath
           let sequent = Prooftree.Tree.Fmttree.sequent
           let seqstring = Sequent.Funs.seqstring
           let tacticstring = Tactic.Funs.tacticstring
           let takethelot = Applyrule.M.takethelot
           let uncurry2 = Miscellaneous.M.uncurry2
           let ( <| ) = Listfuns.M.( <| )
    
    let proofsdone = ref false
    let rec doBEGINPROOF env state =
      match applyLiteralTactic env "IF BEGINPROOF" state with
        Some state' -> state'
      | None -> state
    let rec mkstate provisos givens tree =
      let (cxt, tree, uvs) =
        rewriteProoftree givens false
          (withexterior
             (withprovisos (newcxt, provisos), (givens, sequent tree)))
          tree
      in
      Proofstate
        (let module M =
           struct
             class a =
               object
                 val cxt =
                   withresnum
                     (withusedVIDs (cxt, uvs),
                      nj_fold (uncurry2 max)
                        (maxtreeresnum tree ::
                             ((
                                 maxprovisoresnum <*> provisoactual) <*
                              provisos))
                        1 +
                        1)
                 val givens = givens
                 val tree = tree
                 val goal = None
                 val target = None
                 val root = None
                 method cxt = cxt
                 method givens = givens
                 method tree = tree
                 method goal = goal
                 method target = target
                 method root = root
               end
           end
         in
         new M.a)
    let rec startstate env provisos givens seq =
      let (Proofstate {tree = tree} as state) =
        mkstate provisos givens (mkTip newcxt (rewriteseq newcxt seq))
      in
      (* rewrite necessary - see comment on definition of mkTip *)
      doBEGINPROOF env (withgoal (state, Some (rootPath tree)))
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
       It stores the proof if the proofstage is Proved or Disproved.
     *)
    exception NoProof_ of string list
    (* moved out for OCaml *)

    let rec doProof
      report query env name stage seq (givens, pros, tac) disproofopt =
      let tac = mkReplayTac (mkUniqueTac (mkSimpleApplyTac tac)) in
      let (pros', givens, seq) = compiletoprove (pros, givens, seq) in
      let cxt = withprovisos (newcxt, (mkvisproviso <* pros')) in
      let oldapply = !applyconjectures in
      let oldproving = !proving in
      let rec checkfinalprovisos cxt =
        let newpros =
          (provisoactual <* (provisovisible <| provisos cxt))
        in
        let rec showpros pros =
          if null pros then "no provisos"
          else liststring provisostring " AND " pros
        in
        eqbags (fun (x, y) -> x = y : proviso * proviso -> bool)
          (pros, newpros) ||
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
                  Some
                    ((match stage, isproven st with
                        Disproved, true ->
                          complain
                            ["is a complete proof, but is recorded as a disproof."];
                          InProgress
                      | Proved, false ->
                          complain
                            ["doesn't seem to be a statement of the theorem ";
                             "(the base sequent "; seqstring seq;
                             " matches but it isn't a complete proof)"];
                          InProgress
                      | _ ->
                          if checkfinalprovisos cxt then stage
                          else InProgress),
                     st)
              | None -> raise (NoProof_ ("proof fails -- " :: explain ""))
        with
          NoProof_ ss -> cleanup (); complain ss; None
        | Tacastrophe_ ss ->
            cleanup ();
            report
              ("Error in tactic during " :: label () :: " " ::
                 namestring name :: " -- " :: ss);
            None
        | exn -> cleanup (); raise exn
      in
      (checkproof () &~~
         (fun (stage, state) ->
            let stage =
              match
                stage,
                checkdisproof (proofstate_cxt state) (proofstate_tree state)
                  disproofopt
              with
                Proved, true ->
                  complain ["is recorded as a proof but is disproved"];
                  InProgress
              | Disproved, false ->
                  complain ["is recorded as a disproof but isn't disproved"];
                  InProgress
              | _ -> stage
            in
            if stage <> InProgress then
              begin
                addproof report query name (stage = Proved) state disproofopt;
                None
              end
            else Some (name, state, disproofopt)))
  end
