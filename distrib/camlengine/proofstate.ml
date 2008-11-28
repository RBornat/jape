(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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
open Cxtstring
open Listfuns
open Miscellaneous
open Optionfuns
open Prooftree.Tree
open Prooftree.Tree.Fmttree
open Proviso
open Rewrite
open Sequent
open Sml
open Stringfuns
open Termfuns
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

let string_of_subtree pathopt tree =
  match pathopt with
    Some path -> string_of_prooftree (followPath tree path)
  | _         -> "..."

let string_of_proofstate all (Proofstate {cxt = cxt; tree = tree; givens = givens;
                                          goal = goal; target = target; root = root}) =
  let string_of_subtree pathopt =
    string_of_subtree (if all then pathopt else None)
  in
  implode
    ["Proofstate{cxt = "; string_of_cxt cxt; ",\nproof = ";
     string_of_subtree (Some (rootPath tree)) tree; ",\ngivens = ";
     string_of_list string_of_seq " AND " givens; ",\ngoal = ";
     string_of_option string_of_fmtpath goal; " = "; string_of_subtree goal tree;
     ",\ntarget = "; string_of_option string_of_fmtpath target; " = ";
     string_of_subtree target tree; ",\nroot = ";
     string_of_option
       (fun (rp, _ as r) ->
          string_of_pair (string_of_subtree (Some (rootPath rp))) string_of_fmtpath
            "," r)
       root;
     "}"]

let prunestate goal =
  fun (Proofstate {cxt = cxt; tree = tree} as state) ->
    let (goal', tree') = truncateprooftree cxt goal tree in
    withtree (withtarget (withgoal state (Some goal')) (Some goal')) 
             (set_prooftree_fmt tree' goal' neutralformat)

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

let isproven (Proofstate {tree = tree; cxt = cxt}) =
  not (hasTip tree) &&
  not (List.exists isUnknown
     (nj_fold (uncurry2 (sortedmerge earliervar))
        ((provisovars <.> provisoactual) <* provisos (rewritecxt cxt))
        []))

let rewriteproofstate (Proofstate {cxt = cxt; tree = tree; givens = givens} as state) =
  let (cxt, tree, _) = rewriteProoftree givens true cxt tree in
  withtree (withcxt state cxt) tree

let autorulelist : (bool * Tactictype.tactic) list ref = ref []

let autorules () = !autorulelist

let addautorule (sense, tac as rule) =
  autorulelist :=
    rule :: ((fun (_, tac') -> tac <> tac') <| !autorulelist)

let clearautorules () = autorulelist := []

