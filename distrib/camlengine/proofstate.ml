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
open Context.Cxtstring
open Listfuns
open Miscellaneous
open Optionfuns
open Prooftree.Tree
open Prooftree.Tree.Fmttree
open Proviso
open Rewrite.Funs
open Sequent.Funs
open Sml
open Stringfuns
open Term.Funs
open Thing
open Treeformat.Fmt

type prooftree = Prooftree.Tree.Fmttree.prooftree
 and path = Prooftree.Tree.Fmttree.path
 
let provisovars = provisovars termvars tmerge

type proofstate = Proofstate of staterec
 and staterec = { cxt : cxt; tree : prooftree; givens : seq list;
                  goal : path option; target : path option;
                  root : (prooftree * path) option }

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
let autorulelist : (bool * Tactic.tactic) list ref = ref []
let autorules () = !autorulelist
let addautorule (sense, tac as rule) =
  autorulelist :=
	rule :: ((fun (_, tac') -> tac <> tac') <| !autorulelist)
let clearautorules () = autorulelist := []

