(*
	$Id$

    Copyright (C) 2003 Richard Bornat & Bernard Sufrin
     
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

open Sml
open Termfuns
open Termstore
open Termstring
open Termtype

let bracketedliststring = Listfuns.bracketedliststring
let consolereport = Miscellaneous.consolereport
let enQuote = Stringfuns.enQuote

let rec freshvar object__ cxt =
  let class__ = Idclass.VariableClass in
  let prefix = Symbol.autoID class__ "subst" in
  let (cxt', vid) = Cxtfuns.freshVID cxt class__ (vid_of_string prefix) in
  cxt',
  (if object__ then registerId else registerUnknown)
    (vid, class__)

let rec indistinct cxt =
 not <.> Answer.qDEFNOT <.>
 Miscellaneous.uncurry2
      (Facts.substeqvarsq (Facts.facts (Cxtfuns.provisos cxt) cxt))

let interpolate = Listfuns.interpolate
let ( <* ) = Listfuns.( <* )
let member = Listfuns.member
let fNotinProviso v = Provisotype.NotinProviso v
let plusvisibleprovisos = Cxtfuns.plusvisibleprovisos
let rewrite = Rewrite.rewrite
let simplifySubstAnyway = Substmapfuns.simplifySubstAnyway
let sort = Listfuns.sortunique earliervar
let term_of_string = Termparse.term_of_string
let ( <| ) = Listfuns.( <| )

exception Catastrophe_ = Miscellaneous.Catastrophe_
exception ParseError_ = Miscellaneous.ParseError_

(* convert a text selection into a non-reducible substitution *)
exception Selection_ of string list
let rec selection2Subst object__ sels cxt =
  let original = implode sels in
  let origterm =
    try term_of_string original with
      ParseError_ s ->
        raise
          (Catastrophe_ (["selection2Subst can't parse original: "; original; " -- "] @ s))
  in
  let (cxt', v) = freshvar object__ cxt in
  let rec splitup =
    function
      [s] -> [s], []
    | t1 :: s1 :: ts ->
        let (ts', ss') = splitup ts in t1 :: ts', s1 :: ss'
    | _ ->
        raise
          (Catastrophe_
             ["selection2Subst given even number of strings: ";
              bracketedliststring enQuote "," sels])
  in
  let (ts, ss) = splitup sels in
  let rec bad s =
    raise
      (Selection_
         (s :: " - you split the formula up thus: " ::
            interpolate "; " ((enQuote <* sels))))
  in
  let rec badsub () =
    bad
      (if List.length ss = 1 then "the selection you made wasn't a subformula"
       else "the selections you made weren't all subformulae")
  in
  let ss' =
    try (term_of_string <* ss) with
      ParseError_ _ -> bad "your selection(s) didn't parse"
  in
  let _ =
    if List.exists (fun t -> not (eqterms (t, List.hd ss'))) (List.tl ss') then
      bad "your selections weren't all the same"
  in
  let _P =
    try
      term_of_string (implode (interpolate ((" " ^ termstring v) ^ " ") ts))
    with
      ParseError_ _ -> badsub ()
  in
  let m = [v, List.hd ss'] in
  let res = registerSubst (false, _P, m) in
  let pvs = ((fun v' -> v, v') <* termvars origterm) in
  let extraps = (fNotinProviso <* (indistinct cxt <| pvs)) in
  let cxt'' = plusvisibleprovisos cxt' extraps in
  let rec check t =
    let _E = List.exists (fun b -> v = b) in
    let rec bb () =
      bad "one of your selections was a binding instance of a variable"
    in
    if t = v then Some (List.hd ss')
    else
      match t with
        Binding (_, (bs, ss, us), env, pat) ->
          if _E bs then bb () else None
      | Subst (_, r, _P, vts) ->
          if _E ((fst <* vts)) then bb () else None
      | _ -> None
  in
  if eqterms (rewrite cxt'' res, origterm) then cxt'', res
  else if eqterms (mapterm check _P, origterm) then
    bad
      (("you can't make the substitution you want because " ^
          (if List.length ss = 1 then "your selection"
           else "one of your selections")) ^
         " was inside a binding, and involved a possible bound variable capture")
  else badsub ()
let rec subterm2subst unify cxt pat t =
  let (cxt', v) = freshvar true cxt in
  let rec f hole subt =
    match unify (pat, subt) cxt' with
      Some cxt'' ->
        (* we have found a match, but there may be binding problems *)
        let t' = registerSubst (false, hole v, [v, rewrite cxt'' subt]) in
        if eqterms (rewrite cxt'' t', t) then Some (cxt'', t') else None
    | None -> None
  in
  findhole f t
