(* $Id$ *)

module type Runproof =
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

module
  Runproof
  (AAA :
    sig
      module context : Context
      module name : Name
      module proofstage : Proofstage
      module proofstate : Proofstate
      type japeenv and model and possmatch
      exception Use_ exception Tacastrophe_ of string list
      val ( <| ) : ('a -> bool) * 'a list -> 'a list
      val andthenr : 'a option * ('a -> 'b option) -> 'b option
      val addproof :
        (string list -> unit) ->
          (string list * string * string * int -> bool) -> name.name ->
          bool -> proofstate.treeformat proofstate.prooftree ->
          proofstate.seq list -> context.cxt ->
          (proofstate.seq * model) option -> bool
      val applyconjectures : bool ref
      val applyLiteralTactic :
        japeenv -> string -> proofstate.proofstate ->
          proofstate.proofstate option
      val applyTactic :
        japeenv -> proofstate.tactic -> proofstate.proofstate ->
          proofstate.proofstate option
      val checkdisproof :
        context.cxt -> proofstate.treeformat proofstate.prooftree ->
          (context.seq * model) option -> bool
      val compiletoprove :
        context.proviso list * context.seq list * context.seq ->
          (bool * context.proviso) list * context.seq list * context.seq
      val eqbags : ('a * 'a -> bool) -> 'a list * 'a list -> bool
      val explain : string -> string list
      val getReason : unit -> string list
      val liststring : ('a -> string) -> string -> 'a list -> string
      val MAP : ('a -> 'b) * 'a list -> 'b list
      val maxprovisoresnum : context.proviso -> int
      val maxtreeresnum : proofstate.treeformat proofstate.prooftree -> int
      val mkReplayTac : proofstate.tactic -> proofstate.tactic
      val mkSimpleApplyTac : proofstate.tactic -> proofstate.tactic
      val mkTip :
        context.cxt -> context.seq ->
          proofstate.treeformat proofstate.prooftree
      val mkUniqueTac : proofstate.tactic -> proofstate.tactic
      val mkvisproviso : bool * context.proviso -> context.visproviso
      val proving : name.name ref
      val provisoactual : context.visproviso -> context.proviso
      val provisostring : context.proviso -> string
      val provisovisible : context.visproviso -> bool
      val rewriteproofstate : proofstate.proofstate -> proofstate.proofstate
      val rewriteProoftree :
        context.seq list -> bool -> context.cxt ->
          proofstate.treeformat proofstate.prooftree ->
          context.cxt * proofstate.treeformat proofstate.prooftree *
            context.vid list
      val rewriteseq : context.cxt -> context.seq -> context.seq
      val rootPath :
        proofstate.treeformat proofstate.prooftree -> proofstate.fmtpath
      val sequent : proofstate.treeformat proofstate.prooftree -> context.seq
      val seqstring : context.seq -> string
      val tacticstring : proofstate.tactic -> string
      val takethelot :
        possmatch ->
          (context.cxt * proofstate.treeformat proofstate.prooftree) list
      val uncurry2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
      
    end)
  :
  Runproof =
  struct
    open AAA
    open context open name open proofstage open proofstate
    type model = model and japeenv = japeenv
    
    
    
    
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
                      fold Integer.max
                        (maxtreeresnum tree ::
                           MAP
                             ((fun ooo ->
                                 maxprovisoresnum (provisoactual ooo)),
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
      let cxt = withprovisos (newcxt, MAP (mkvisproviso, pros')) in
      let oldapply = !applyconjectures in
      let oldproving = !proving in
      let rec checkfinalprovisos cxt =
        let newpros =
          MAP (provisoactual, ( <| ) (provisovisible, provisos cxt))
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
      andthenr
        (checkproof (),
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
