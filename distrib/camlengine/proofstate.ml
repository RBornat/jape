(* $Id$ *)

module type T =
  sig
    type term and seq and cxt and tactic and prooftree and path
    
    (* should this be public? RB *)
    type staterec = { cxt : cxt; tree : prooftree; givens : seq list;
					  goal : path option; target : path option;
					  root : (prooftree * path) option }
    type proofstate = Proofstate of staterec

    val isproven : proofstate -> bool
    val prunestate : path -> proofstate -> proofstate
    val proofstep :
      cxt -> prooftree -> proofstate -> proofstate option
    val nextGoal : bool -> proofstate -> proofstate
    val rewriteproofstate : proofstate -> proofstate
    val autorules : unit -> (bool * tactic) list
    val addautorule : bool * tactic -> unit
    val clearautorules : unit -> unit
    val proofstatestring : bool -> proofstate -> string
    val proofstate_cxt : proofstate -> cxt
    val proofstate_tree : proofstate -> prooftree
    val proofstate_givens : proofstate -> seq list
    val proofstate_goal : proofstate -> path option
    val proofstate_target : proofstate -> path option
    val proofstate_root :
      proofstate -> (prooftree * path) option
    val withcxt : proofstate -> cxt -> proofstate
    val withtree : proofstate -> prooftree -> proofstate
    val withgivens : proofstate -> seq list -> proofstate
    val withgoal : proofstate -> path option -> proofstate
    val withtarget : proofstate -> path option -> proofstate
    val withroot : proofstate -> (prooftree * path) option -> proofstate
  end  

module M : T with type term = Term.Funs.term
			  and type seq = Sequent.Funs.seq
			  and type cxt = Context.Cxt.cxt
			  and type tactic = Tactic.Funs.tactic
			  and type prooftree = Prooftree.Tree.Fmttree.prooftree
			  and type path = Prooftree.Tree.Fmttree.path
=
  struct
    open Context.Cxt
    open Context.Cxtstring
    open Listfuns.M
    open Miscellaneous.M
    open Optionfuns.M
    open Prooftree.Tree
    open Prooftree.Tree.Fmttree
    open Proviso.M
    open Rewrite.Funs
    open Sequent.Funs
    open SML.M
    open Stringfuns.M
    open Term.Funs
    open Thing.M
    open Treeformat.Fmt
    
    type term = Term.Funs.term
     and seq = Sequent.Funs.seq
     and cxt = Context.Cxt.cxt
     and tactic = Tactic.Funs.tactic
     and prooftree = Prooftree.Tree.Fmttree.prooftree
     and path = Prooftree.Tree.Fmttree.path
     
	let provisovars = provisovars termvars tmerge
	
    type staterec = { cxt : cxt; tree : prooftree; givens : seq list;
					  goal : path option; target : path option;
					  root : (prooftree * path) option }
    type proofstate = Proofstate of staterec
    
    let proofstate_cxt = fun (Proofstate {cxt = cxt}) -> cxt
    let proofstate_tree = fun (Proofstate {tree = tree}) -> tree
    let proofstate_givens = fun (Proofstate {givens = givens}) -> givens
    let proofstate_goal = fun (Proofstate {goal = goal}) -> goal
    let proofstate_target = fun (Proofstate {target = target}) -> target
    let proofstate_root = fun (Proofstate {root = root}) -> root
    
    let withcxt (Proofstate s) cxt = Proofstate {s with cxt = cxt}
    let withtree (Proofstate s) tree = Proofstate {s with tree = tree}
    let withgivens (Proofstate s) givens = Proofstate {s with givens = givens}
    let withgoal (Proofstate s) goal = Proofstate {s with goal = goal}
    let withtarget (Proofstate s) target = Proofstate {s with target = target}
    let withroot (Proofstate s) root = Proofstate {s with root = root}

    let proofstatestring all =
      fun
        (Proofstate
           {cxt = cxt;
            tree = tree;
            givens = givens;
            goal = goal;
            target = target;
            root = root}) ->
        let showsubtree pathopt tree =
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
    let prunestate goal =
      fun (Proofstate {cxt = cxt; tree = tree} as state) ->
        let (goal', tree') = truncateprooftree cxt goal tree in
        withtree (withtarget (withgoal state (Some goal')) (Some goal')) tree'
    let nextusefulgoal skip t path =
      (findRightwardsGoal skip t path |~~ (fun _ -> findAnyGoal t))
    let proofstep a1 a2 a3 =
      match a1, a2, a3 with
        cxt, subtree,
        (Proofstate {tree = tree; goal = Some gpath} as state) ->
          let (gpath', tree') = insertprooftree cxt gpath tree subtree in
          Some
            (withcxt
               (withgoal (withtree state tree') (nextusefulgoal false tree' gpath'))
               cxt)
      | _, _, _ -> None
    let nextGoal a1 a2 =
      match a1, a2 with
        skip, (Proofstate {tree = tree; goal = Some gpath} as state) ->
          withtarget (withgoal state (nextusefulgoal skip tree gpath)) (Some gpath)
      | skip, state -> state
    let isproven =
      fun (Proofstate {tree = tree; cxt = cxt}) ->
        not (hasTip tree) &&
        not (List.exists isUnknown
			   (nj_fold (uncurry2 (sortedmerge earliervar))
				  ((provisovars <*> provisoactual) <* provisos (rewritecxt cxt))
				  []))
    let rewriteproofstate =
      fun (Proofstate {cxt = cxt; tree = tree; givens = givens} as state) ->
        let (cxt, tree, _) = rewriteProoftree givens true cxt tree in
        withtree (withcxt state cxt) tree
    let autorulelist : (bool * tactic) list ref = ref []
    let autorules () = !autorulelist
    let addautorule (sense, tac as rule) =
      autorulelist :=
        rule :: ((fun (_, tac') -> tac <> tac') <| !autorulelist)
    let clearautorules () = autorulelist := []
  end

