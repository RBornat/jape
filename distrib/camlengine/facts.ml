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

open Answer 
open Cxttype
open Cxtstring
open Cxtexterior 
open Listfuns
open Mappingfuns
open Miscellaneous
open Provisotype (* ok. RB *)
open Proviso
open Rewinf
open Stringfuns
open Symbol
open Termfuns 
open Termstring
open Termtype

let factsdebug = ref false

type facts = proviso list * exterior

let factsstring =
  pairstring (bracketedliststring provisostring " AND ") exteriorstring ","

let rec facts provisos cxt =
  (provisoactual <* provisos), getexterior cxt

let rec expandfacts (oldps, ext) ps = ps @ oldps, ext
(* the names of the functions are intended to indicate that the function tells us
   only when a fact is definitely known to be true.
   
   I use eqterms, but it might be a bit inefficient ...
 *)

(* Strictly, a NOTIN b doesn't imply b NOTIN a, but since a is always a variable in
   our system, a NOTIN b means also b NOTIN a.
 *)

let rec knownNOTIN (ps, _) (v, t) =
  List.exists
    (function
       NotinProviso (v', t') ->
         eqterms (v, v') && eqterms (t, t') ||
         eqterms (v, t') && eqterms (t, v')
     | _ -> false)
    ps
let rec knownproofvar facts v =
  match v, facts with
    Id _, (_, Exterior (_, Some ri, _)) ->
      not (member (v, rewinf_vars ri)) &&
      _All
         (fun u -> knownNOTIN facts (v, u))
         (isUnknown <| rewinf_vars ri)
  | _ -> false

(* This function is deciding whether a variable v can be made equal to some term t
   by unification and/or instatiation of parameters.  It is used in simplifySubst.
   
   Provisos, read as facts that we can depend on, make it much more possible to
   distinguish what is and isn't equal in substitutions.
   If we know that x is NOTIN v or v is NOTIN x then v and x can't be made equal.
   
   The new 'class' feature of names is more help still.  
   Information about bindings, stored in the Exterior component of the context,
   is yet further help.
 *)

let rec showvarsq facts v1 v2 name r =
  if !factsdebug then
    begin
      let (ps, sb) = facts in
      consolereport
        [name; " ("; bracketedliststring provisostring "," ps; ",";
         exteriorstring sb; ") "; smltermstring v1; " "; smltermstring v2;
         " => "; answerstring r]
    end;
  r

(* this function takes no notice of NOTIN and idclass, but it does look at FRESH ... 
   boy, I hope I get this right! 
 *)

let rec exterioreqvarsq facts v1 v2 =
  let v1 = debracket v1 in
  let v2 = debracket v2 in
  let r =
    if v1 = v2 then Yes
    else
      match v1, v2 with
        Id (_, vid1, _), Id (_, vid2, _) ->
          if not (isextensibleID (string_of_vid vid1)) && 
             not (isextensibleID (string_of_vid vid2)) 
          then No
          else
            begin match facts with
              ps, Exterior (_, _, Some {fvs=fvs; vmap=vmap; bhfvs=bhfvs; bcfvs=bcfvs}) ->
                let rec checkfresh () =
                  if List.exists
                       (function
                          FreshProviso (h, g, _, v') ->
                            let rec ok v =
                              h && member (v, bhfvs) ||
                              g && member (v, bcfvs)
                            in
                            v1 = v' && ok v2 || v2 = v' && ok v1
                        | _ -> false)
                       ps
                  then
                    No
                  else Maybe
                in
                if member (v1, fvs) && member (v2, fvs) then checkfresh ()
                else if
                  (match (vmap <@> v1) with
                     Some vs -> member (v2, vs)
                   | _ -> false) ||
                  (match (vmap <@> v2) with
                     Some vs -> member (v1, vs)
                   | _ -> false)
                then
                  checkfresh ()
                else No
            | _ ->
                if knownproofvar facts v1 || knownproofvar facts v2 then
                  No
                else Maybe
            end
      | _ -> Maybe
  in
  showvarsq facts v1 v2 "exterioreqvarsq" r

(* this function uses NOTIN and idclass *)

let rec substeqvarsq facts v1 v2 =
  let v1 = debracket v1 in
  let v2 = debracket v2 in
  let c1 = idclass v1 in
  let c2 = idclass v2 in
  let r =
    if v1 = v2 then Yes
    else if not (specialisesto (c1, c2) || specialisesto (c2, c1)) then No
    else if(* that is wrong, in general.  What is needed is a test
     * that c1 and c2 have a common ancestor.
     *) 
     knownNOTIN facts (v1, v2) then No
    else exterioreqvarsq facts v1 v2
  in
  showvarsq facts v1 v2 "substeqvarsq" r

let rec eqlistsq a1 a2 a3 =
  match a1, a2, a3 with
    _Q, [], [] -> Yes
  | _Q, t1 :: t1s, t2 :: t2s ->
      andalsoq (_Q t1 t2) (fun () -> eqlistsq _Q t1s t2s)
  | _Q, _, _ -> No
(* What this function is deciding is whether there is any substitution for metavariables
      which could make equal ground terms.  It is only used in abstraction (see unify.sml).
      So we can treat Paramids as Conids without fear, and no need for parameterisation any more.
      RB 31/i/93
      
      I had believed that termoccursin was sound for use in this function, as it is 
      if it doesn't go into substitutions.  
      It doesn't seem to make a significant difference to execution times, but I haven't 
      checked that very carefully.
      
      This function will perform
      reasonably given *fully simplified* substitutions: otherwise it is dramatically 
      over-cautious.
      RB 20/i/93
      
      And it *isn't* uncurried any more.  Hooray!
    *)

let rec unifyeqtermsq facts t1 t2 =
  let (t1, t2) = debracket t1, debracket t2 in
  match t1, t2 with
    Unknown (_, _, c1), Unknown (_, _, c2) ->
      if t1 = t2 then Yes
      else if specialisesto (c1, c2) || specialisesto (c2, c1) then Maybe
      else No
  | Unknown (_, _, c1), t2 ->
      if not (specialisesto (c1, idclass t2)) then No
      else if termoccursin t1 t2 then No
      else substeqvarsq facts t1 t2
  | t1, Unknown _ -> unifyeqtermsq facts t2 t1
  | Id _, Id _ -> if t1 = t2 then Yes else No
  | App (_, f1, a1), App (_, f2, a2) ->
      andalsoq (unifyeqtermsq facts f1 f2)
        (fun () -> unifyeqtermsq facts a1 a2)
  | Literal (_, k1), Literal (_, k2) -> if k1 = k2 then Yes else No
  | Tup (_, s1, t1s), Tup (_, s2, t2s) ->
      if s1 = s2 then unifyeqtlistsq facts t1s t2s else No
  | Fixapp (_, s1s, t1s), Fixapp (_, s2s, t2s) ->
      if s1s = s2s then unifyeqtlistsq facts t1s t2s else No
  | Binding (_, (bs, ss, us), _, pat),
    Binding (_, (bs', ss', us'), _, pat') ->
      if pat = pat' then
        andalsoq (unifyeqtlistsq facts bs bs')
          (fun _ ->
             andalsoq (unifyeqtlistsq facts ss ss')
               (fun _ -> unifyeqtlistsq facts us us'))
      else No
  | Subst (_, _, _P1, m1), Subst (_, _, _P2, m2) ->
      (* Substitutions are a bit dodgy. If they are the same, then they are the same.
         But if they are different, who knows?  There are lots of instances of the problem.
         For example, P[_a\y], _Q[_b\x] where P and _Q are different terms are apparently different
         - but perhaps they could be made the same.
         So a fully simplified substitution is at least Maybe the same as another substitution 
         or another term, always.
         Classes make this a bit less likely to happen ...
       *)
      if not (simterms (t1, t2)) then No
      else
        takeYes
          (andalsoq (unifyeqtermsq facts _P1 _P2)
             (fun () ->
                unifyeqmapsq facts (canonicalsubstmap m1)
                  (canonicalsubstmap m2)))
  | Subst (_, _, _P1, m1), _ ->
      if not (simterms (t1, t2)) then No
      else if termoccursin t1 t2 then No
      else Maybe
  | t1, Subst _ -> unifyeqtermsq facts t2 t1
  | _ ->(* but otherwise I'm sure, I think *)
     No

and unifyeqtlistsq facts = eqlistsq (unifyeqtermsq facts)

and unifyeqmapsq facts vts vts' =
  eqlistsq
    (fun (v, t) (v', t') ->
       andalsoq (unifyeqtermsq facts v v')
         (fun () -> unifyeqtermsq facts t t'))
    vts vts'
