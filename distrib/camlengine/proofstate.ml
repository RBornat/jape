(* $Id$ *)

module type Proofstate =
  sig
    type term and seq
    type cxt and tactic and 'a prooftree and treeformat and fmtpath
    type proofstate =
        Proofstate of
          < cxt : cxt; tree : treeformat prooftree; givens : seq list;
            goal : fmtpath option; target : fmtpath option;
            root : (treeformat prooftree * fmtpath) option >
    val isproven : proofstate -> bool
    val prunestate : fmtpath -> proofstate -> proofstate
    val proofstep :
      cxt -> treeformat prooftree -> proofstate -> proofstate option
    val nextGoal : bool -> proofstate -> proofstate
    val rewriteproofstate : proofstate -> proofstate
    val autorules : unit -> (bool * tactic) list
    val addautorule : bool * tactic -> unit
    val clearautorules : unit -> unit
    val proofstatestring : bool -> proofstate -> string
    val proofstate_cxt : proofstate -> cxt
    val proofstate_tree : proofstate -> treeformat prooftree
    val proofstate_givens : proofstate -> seq list
    val proofstate_goal : proofstate -> fmtpath option
    val proofstate_target : proofstate -> fmtpath option
    val proofstate_root :
      proofstate -> (treeformat prooftree * fmtpath) option
    val withcxt : proofstate * cxt -> proofstate
    val withtree : proofstate * treeformat prooftree -> proofstate
    val withgivens : proofstate * seq list -> proofstate
    val withgoal : proofstate * fmtpath option -> proofstate
    val withtarget : proofstate * fmtpath option -> proofstate
    val withroot :
      proofstate * (treeformat prooftree * fmtpath) option -> proofstate
  end  
(* $Id$ *)

module
  Proofstate
  (AAA :
    sig
      module listfuns : Listfuns
      module stringfuns : Stringfuns
      module optionfuns : Optionfuns
      module term : Term
      module sequent : Sequent
      module context : Context
      module thing : Thing
      module prooftree : Prooftree
      module treeformat : TreeFormat
      val empty : ('a, 'b) context.mapping
      val provisoactual : context.visproviso -> context.proviso
      val provisovars : context.proviso -> term.term list
      val rewritecxt : context.cxt -> context.cxt
      val rewriteseq : context.cxt -> sequent.seq -> sequent.seq
      val uncurry2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
      
    end)
  :
  Proofstate =
  struct
    open AAA
    open listfuns
    open stringfuns
    open optionfuns
    open term
    open sequent
    open context
    open thing
    open prooftree
    open prooftree.fmtprooftree
    open treeformat
    (* from listfuns.sig.sml *)
    
    
    (* from context.sig.sml *)
    
    (* from optionfuns.sig.sml *)
    
    type proofstate =
        Proofstate of
          < cxt : cxt; tree : treeformat prooftree; givens : seq list;
            goal : fmtpath option; target : fmtpath option;
            root : (treeformat prooftree * fmtpath) option >
    let rec proofstate_cxt = fun (Proofstate {cxt = cxt}) -> cxt
    let rec proofstate_tree = fun (Proofstate {tree = tree}) -> tree
    let rec proofstate_givens = fun (Proofstate {givens = givens}) -> givens
    let rec proofstate_goal = fun (Proofstate {goal = goal}) -> goal
    let rec proofstate_target = fun (Proofstate {target = target}) -> target
    let rec proofstate_root = fun (Proofstate {root = root}) -> root
    
    let rec withcxt =
      fun
        (Proofstate
           {tree = tree;
            givens = givens;
            goal = goal;
            target = target;
            root = root}, cxt) ->
        Proofstate
          (let module M =
             struct
               class a =
                 object
                   val cxt = cxt
                   val tree = tree
                   val givens = givens
                   val goal = goal
                   val target = target
                   val root = root
                   method cxt = cxt
                   method tree = tree
                   method givens = givens
                   method goal = goal
                   method target = target
                   method root = root
                 end
             end
           in
           new M.a)
    let rec withtree =
      fun
        (Proofstate
           {cxt = cxt;
            givens = givens;
            goal = goal;
            target = target;
            root = root}, tree) ->
        Proofstate
          (let module M =
             struct
               class a =
                 object
                   val cxt = cxt
                   val tree = tree
                   val givens = givens
                   val goal = goal
                   val target = target
                   val root = root
                   method cxt = cxt
                   method tree = tree
                   method givens = givens
                   method goal = goal
                   method target = target
                   method root = root
                 end
             end
           in
           new M.a)
    let rec withgivens =
      fun
        (Proofstate
           {cxt = cxt;
            tree = tree;
            goal = goal;
            target = target;
            root = root}, givens) ->
        Proofstate
          (let module M =
             struct
               class a =
                 object
                   val cxt = cxt
                   val tree = tree
                   val givens = givens
                   val goal = goal
                   val target = target
                   val root = root
                   method cxt = cxt
                   method tree = tree
                   method givens = givens
                   method goal = goal
                   method target = target
                   method root = root
                 end
             end
           in
           new M.a)
    let rec withgoal =
      fun
        (Proofstate
           {cxt = cxt;
            tree = tree;
            givens = givens;
            target = target;
            root = root}, goal) ->
        Proofstate
          (let module M =
             struct
               class a =
                 object
                   val cxt = cxt
                   val tree = tree
                   val givens = givens
                   val goal = goal
                   val target = target
                   val root = root
                   method cxt = cxt
                   method tree = tree
                   method givens = givens
                   method goal = goal
                   method target = target
                   method root = root
                 end
             end
           in
           new M.a)
    let rec withtarget =
      fun
        (Proofstate
           {cxt = cxt;
            tree = tree;
            givens = givens;
            goal = goal;
            root = root}, target) ->
        Proofstate
          (let module M =
             struct
               class a =
                 object
                   val cxt = cxt
                   val tree = tree
                   val givens = givens
                   val goal = goal
                   val target = target
                   val root = root
                   method cxt = cxt
                   method tree = tree
                   method givens = givens
                   method goal = goal
                   method target = target
                   method root = root
                 end
             end
           in
           new M.a)
    let rec withroot =
      fun
        (Proofstate
           {cxt = cxt;
            tree = tree;
            givens = givens;
            goal = goal;
            target = target}, root) ->
        Proofstate
          (let module M =
             struct
               class a =
                 object
                   val cxt = cxt
                   val tree = tree
                   val givens = givens
                   val goal = goal
                   val target = target
                   val root = root
                   method cxt = cxt
                   method tree = tree
                   method givens = givens
                   method goal = goal
                   method target = target
                   method root = root
                 end
             end
           in
           new M.a)
    let rec proofstatestring all =
      fun
        (Proofstate
           {cxt = cxt;
            tree = tree;
            givens = givens;
            goal = goal;
            target = target;
            root = root}) ->
        let rec showsubtree pathopt tree =
          match all, pathopt with
            true, Some path -> prooftreestring (followPath tree path)
          | _ -> "..."
        in
        implode
          ["Proofstate{cxt = "; cxtstring cxt; ",\nproof = ";
           showsubtree (Some (rootPath tree)) tree; ",\ngivens = ";
           liststring seqstring " AND " givens; ",\ngoal = ";
           optionstring fmtpathstring goal; " = "; showsubtree goal tree;
           ",\ntarget = "; optionstring fmtpathstring target; " = ";
           showsubtree target tree; ",\nroot = ";
           optionstring
             (fun (rp, _ as r) ->
                pairstring (showsubtree (Some (rootPath rp))) fmtpathstring
                  "," r)
             root;
           "}"]
    let rec prunestate goal =
      fun (Proofstate {cxt = cxt; tree = tree} as state) ->
        let (goal', tree') = truncateprooftree cxt goal tree in
        withtree
          (withtarget (withgoal (state, Some goal'), Some goal'), tree')
    let rec nextusefulgoal skip t path =
      ortryr (findRightwardsGoal skip t path, (fun _ -> findAnyGoal t))
    let rec proofstep a1 a2 a3 =
      match a1, a2, a3 with
        cxt, subtree,
        (Proofstate {tree = tree; goal = Some gpath} as state) ->
          let (gpath', tree') = insertprooftree cxt gpath tree subtree in
          Some
            (withcxt
               (withgoal
                  (withtree (state, tree'),
                   nextusefulgoal false tree' gpath'),
                cxt))
      | _, _, _ -> None
    let rec nextGoal a1 a2 =
      match a1, a2 with
        skip, (Proofstate {tree = tree; goal = Some gpath} as state) ->
          withtarget
            (withgoal (state, nextusefulgoal skip tree gpath), Some gpath)
      | skip, state -> state
    let rec isproven =
      fun (Proofstate {tree = tree; cxt = cxt}) ->
        not (hasTip tree) &&
        not
          (List.exists isUnknown
             (nj_fold (sortedmerge earliervar)
                (m_a_p
                   ((fun ooo -> provisovars (provisoactual ooo)),
                    provisos (rewritecxt cxt)))
                []))
    let rec rewriteproofstate =
      fun (Proofstate {cxt = cxt; tree = tree; givens = givens} as state) ->
        let (cxt, tree, _) = rewriteProoftree givens true cxt tree in
        withtree (withcxt (state, cxt), tree)
    let autorulelist : (bool * tactic) list ref = ref []
    let rec autorules () = !autorulelist
    let rec addautorule (sense, tac as rule) =
      autorulelist :=
        rule :: ( <| ) ((fun (_, tac') -> tac <> tac'), !autorulelist)
    let rec clearautorules () = autorulelist := []
  end

