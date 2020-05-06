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

open Listfuns
open Miscellaneous
open Sml

(* updatable syntax search trees, without backtracking. 
 * The trees have to allow for 'prefix' searching, where there is
 * an answer as soon as a particular point is passed.
 *)

type ('c, 'r) fsm =
  | Wrong
  | Answer of 'r
  | Prefix of ('r * ('c, 'r) fsm)
  | Shift of ('c, 'r) fsm
  | Alt of ('c -> ('c, 'r) fsm)
  | Eq of ('c * ('c, 'r) fsm * ('c, 'r) fsm)

(* cheapo alt *)

type ('c, 'r) status =
  | Unbuilt of ('c, 'r) mkalt
  | Built of (('c, 'r) mkalt * ('c, 'r) fsm)

and ('c, 'r) mkalt =
  ('c * ('c, 'r) fsm) list -> ('c, 'r) fsm -> 'c -> ('c, 'r) fsm

type ('c, 'r) searchtree =
  | SearchTree of (('c list * 'r * bool) list * ('c, 'r) status)

(* chars  result isprefix     tree            *)

let emptysearchtree f = SearchTree ([], Unbuilt f)

let mkalt = function Built (f, _) -> f | Unbuilt f -> f

let same eqr (cs, r, isprefix) (cs', r', b) =
  (cs = cs' && eqr (r, r')) && isprefix = b

let diff cs (cs', _, _) = cs <> cs'

(* given a tree, a list of cs and a result, make an augmented tree *)
(* for heaven's sake, slow lookup doesn't matter here? *)

let addtotree eqr (SearchTree (csrbs, status) as t) ((cs, r, isprefix) as info)
    =
  if List.exists (same eqr info) csrbs then t
  else SearchTree (info :: (diff cs <| csrbs), Unbuilt (mkalt status))

(* delete stuff from trees; explode if it isn't there *)

exception DeleteFromTree_

let deletefromtree eqr (SearchTree (csrbs, status) (* as t *))
    ((cs, r, isprefix) as info) =
  if List.exists (same eqr info) csrbs then
    SearchTree (diff cs <| csrbs, Unbuilt (mkalt status))
  else raise DeleteFromTree_

(* give list of items in tree and what they index *)

let summarisetree (SearchTree (csrbs, _)) = csrbs

let rec catelim_string_of_fsm cf rf t ss =
  match t with
  | Wrong -> "Wrong" :: ss
  | Answer r -> "Answer(" :: rf r (")" :: ss)
  | Prefix (r, t') ->
      "Prefix(" :: rf r ("," :: catelim_string_of_fsm cf rf t' (")" :: ss))
  | Shift t' -> "Shift(" :: catelim_string_of_fsm cf rf t' (")" :: ss)
  | Alt _ -> "Alt ..." :: ss
  | Eq (c, t1, t2) ->
      "Eq("
      :: cf c
           ( ","
           :: catelim_string_of_fsm cf rf t1
                ("," :: catelim_string_of_fsm cf rf t2 (")" :: ss)) )

let rootfsm rt =
  match !rt with
  | SearchTree (_, Built (_, t)) -> t
  | SearchTree (csrbs, Unbuilt mkalt) ->
      let rec doit csrbs =
        match split (fun (cs, _, _) -> null cs) csrbs with
        | (_, r, true) :: _, qs -> Prefix (r, doit qs)
        | (_, r, _) :: _, [] ->
            (* because of add/delete above, there is only one r *)
            Answer r
        | [], [] ->
            (* because of add/delete above, there is only one r *)
            Wrong
        | (_, r, _) :: _, qs -> doalt [] qs (Answer r)
        | _, qs -> doalt [] qs Wrong
      and doalt a1 a2 a3 =
        match (a1, a2, a3) with
        | cqs, [], def -> (
            let cts = (fun (c, csrbs) -> (c, doit csrbs)) <* cqs in
            match cts with
            | [ (c, t) ] -> Eq (c, t, def)
            | [] -> def
            | _ -> Alt (mkalt ((fun (c, t) -> (c, Shift t)) <* cts) def) )
        | cqs, qs, def ->
            let c = List.hd ((fun (xs, _, _) -> xs) (List.hd qs)) in
            let sames, diffs = split (fun (cs, _, _) -> List.hd cs = c) qs in
            doalt
              ((c, (fun (cs, r, b) -> (List.tl cs, r, b)) <* sames) :: cqs)
              diffs def
      in
      let t = doit csrbs in
      rt := SearchTree (csrbs, Built (mkalt, t));
      t

type ('r, 's) searchresult = Found of ('r * 's) | NotFound of 's

(* given a tree and a list of things, find the tree to which those things take you 
 * -- takes no notice at all of Prefix
 *)

let rec fsmpos t cs =
  match (t, cs) with
  | _, [] -> Some t
  | Prefix (_, t'), _ -> fsmpos t' cs
  | Shift t', c :: cs' -> fsmpos t' cs'
  | Alt f, c :: _ -> fsmpos (f c) cs
  | Eq (c', y, n), c :: cs' -> if c = c' then fsmpos y cs' else fsmpos n cs
  | _ -> None

(* a scan function *)
(* result includes reverse of ''cs scanned *)

let scanfsm next t rcs c =
  let rec scan t rcs c =
    match t with
    | Answer r -> Found (r, rcs)
    | Wrong -> NotFound rcs
    | Prefix (r, t') -> (
        match scan t' rcs c with NotFound _ -> Found (r, rcs) | res -> res )
    | Shift t' -> scan t' (c :: rcs) (next ())
    | Alt f -> scan (f c) rcs c
    | Eq (c', y, n) ->
        if c = c' then scan y (c :: rcs) (next ()) else scan n rcs c
  in
  scan t rcs c

(* this is duplicated, but that's because the one above has to be fast *)
(* no idea if the Prefix stuff is useful *)
(* this result includes state *)

let scanstatefsm curr move t state =
  let rec scan t state =
    match t with
    | Answer r -> Found (r, state)
    | Wrong -> NotFound state
    | Prefix (r, t') -> (
        match scan t' state with NotFound s -> Found (r, s) | res -> res )
    | Shift t' -> scan t' (move state)
    | Alt f -> scan (f (curr state)) state
    | Eq (c', y, n) ->
        if curr state = c' then scan y (move state) else scan n state
  in
  scan t state

(* a search function -- notices Prefix *)

let searchfsm t cs =
  let rec search t rcs cs =
    match (t, cs) with
    | Answer r, [] -> Found (r, rcs)
    | Prefix (r, t'), _ -> (
        match search t' rcs cs with NotFound _ -> Found (r, rcs) | res -> res )
    | Shift t', c :: cs' -> search t' (c :: rcs) cs'
    | Alt f, c :: _ -> search (f c) rcs cs
    | Eq (c', y, n), c :: cs' ->
        if c = c' then search y (c :: rcs) cs' else search n rcs cs
    | _ -> NotFound rcs
  in
  search t [] cs
