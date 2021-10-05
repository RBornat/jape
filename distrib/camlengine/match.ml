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

open Miscellaneous
open Listfuns
open Mappingfuns
open Optionfuns 
open Termtype
open Termfuns
open Termstring
open Termstore 
open Idclass
open Sml

let consolereport = Miscellaneous.consolereport

type ('a, 'b) mapping = ('a, 'b) Mappingfuns.mapping
type term = Termtype.term

let matchdebug = ref false

(* because of matching in provisos, we have to use a discrimination between 
 * certainty and uncertainty
 *)
type matchresult =
  Certain of (term, term) mapping | Uncertain of (term, term) mapping

let rec string_of_matchresult =
  function
  | Certain e -> "Certain " ^ string_of_mapping string_of_term string_of_term e
  | Uncertain e -> "Uncertain " ^ string_of_mapping string_of_term string_of_term e

let rec env =
  function
  | Certain e -> e
  | Uncertain e -> e

let rec ( +++ ) =
  function
  | Certain e, e' -> Certain ((e ++ e'))
  | Uncertain e, e' -> Uncertain ((e ++ e'))

(* because of bag matching this is now mr list -> mr list *)
let rec matchtermvars matchbra ispatvar pat term mrs =
  let matchterm = matchtermvars matchbra ispatvar in
  let rec res' s p t rs =
    let rec mm es = bracketed_string_of_list string_of_matchresult "," es in
    if !matchdebug then
      consolereport
        [s; "matchterm "; string_of_termarg p; " "; string_of_termarg t; " "; mm mrs;
         " => "; mm rs];
    rs
  in
  let res = res' "" pat term in
  let rec eqt (t1, t2) mrs =
    if eqterms (t1, t2) then mrs
    else if simterms (t1, t2) then
         (function
          | Certain e -> Uncertain e
          | umr -> umr
         ) <* mrs
    else []
  in
  let rec bindvar v t mr =
    match (env mr <@> v) with
      None ->
        if specialisesto (idclass v, idclass t) then
          Some (( +++ ) (mr, (v |-> t)))
        else None
    | Some t ->
        match eqt (term, t) [mr] with
          [mr'] -> Some mr'
        | _ -> None
  in
  let rec matchlist f pats objs mrs =
    try nj_fold (uncurry2 (uncurry2 f)) ((pats ||| objs)) mrs with
      Zip_ -> []
  in
  let listmatch = matchlist matchterm in
  let matchvts =
    let rec f (vpat, tpat) (vterm, tterm) =
      matchterm vpat vterm <.> matchterm tpat tterm
    in
    matchlist f
  in
  (* doesn't match resource numbers *)
  let rec matchelement epat eterm mrs =
    match epat, eterm with
      Segvar (_, pspat, vpat), Segvar (_, psterm, vterm) ->
        if pspat = psterm then matchterm vpat vterm mrs else []
    | Element (_, _, tpat), Element (_, _, tterm) ->
        matchterm tpat tterm mrs
    | _ -> []
  in
  (* let rec tc k es = string_of_term (Collection (None, k, es)) in *)
  (* only temporary *)
                
  let rec bagmatch epats eterms mrs =
    let k = BagClass FormulaClass in
    let rec res rs =
      res' "(bagmatch) " (Collection (None, k, epats))
        (Collection (None, k, eterms)) rs
    in
    (* both temporary *)
    match epats, eterms with
      [], [] -> res mrs
    | epat :: epats, _ :: _ ->
        let rec f t =
          match matchelement epat t mrs with
            [] -> None
          | mrs -> Some mrs
        in
        let rec g (_, mrs, terms) = bagmatch epats terms mrs in
        res (List.concat ((g <* matchbag f eterms)))
    | _ -> res []
  in
  if null mrs then []
  else
    match
      (if matchbra then pat else debracket pat),
      (if matchbra then term else debracket term)
    with
    | (Id _ as v), _ ->
        res (if ispatvar v then optionfilter (bindvar v term) mrs
             else eqt (v, term) mrs
            )
    | (Unknown _ as v), _ -> res (optionfilter (bindvar v term) mrs)
    | App (_, f, a), App (_, f', a') ->
        res (matchterm a a' (matchterm f f' mrs))
    | Tup (_, s, ts), Tup (_, s', ts') ->
        res (if s = s' then listmatch ts ts' mrs else [])
    | Literal (_, k), Literal (_, k') -> res (if k = k' then mrs else [])
    | Fixapp (_, ms, ts), Fixapp (_, ms', ts') ->
        res (if ms = ms' then listmatch ts ts' mrs else [])
    | Subst (_, _, _P, vts), Subst (_, _, _P', vts') ->
        res (matchterm _P _P' (matchvts vts vts' mrs))
    | Binding (_, (bs, ss, us), _, pat),
      Binding (_, (bs', ss', us'), _, pat') ->
        res
          (if pat = pat' then
             listmatch bs bs' (listmatch ss ss' (listmatch us us' mrs))
           else [])
    | Collection (_, k, es), Collection (_, k', es') ->
        begin match k, k' with
          ListClass k, ListClass k' ->
            if k = k' then
              res' "(ListClass) " pat term
                (matchlist matchelement es es' mrs)
            else res' "?? (1) " pat term []
        | BagClass k, BagClass k' ->
            if k = k' then bagmatch es es' mrs
            else res' "?? (2) " pat term []
        | _ -> res' "?? (3) " pat term []
        end
    | _ -> res' "?? " pat term []

(* Ok, this is the point at which I really begin to see why we ought to 
 * distinguish Id/Unknown from everything else.  We use remapterm to alter
 * names (or we might do so :-)) and also to change a pattern into an instance.
 * These two can't be distinguished, hence the hoohah when remapping Collections.
 * RB 8/x/96
 *)
let rec option_remapterm env term =
  let _Rvar v = match (env <@> v) with
                | None   -> v
                | Some t -> t
  in
  let rec _R =
    function
    | Id _ as v             -> (env <@> v)
    | Unknown _ as v        -> (env <@> v)
    | Collection (_, k, es) ->
        let _RR = mapterm _R in
        let rec f =
          function
          | Segvar (_, ps, v), es ->
              let ps' = (_RR <* ps) in
              (match (env <@> v) with
               | Some t ->
                   (match debracket t with
                    | Collection (_, k', es') ->
                        if k = k' then es' @ es
                        else raise (Catastrophe_ ["remapterm Collection 1"])
                    | Id _ -> registerSegvar (ps', t) :: es
                    | Unknown _ -> registerSegvar (ps', t) :: es
                    | _ -> raise (Catastrophe_ ["remapterm Collection 2"])
                   )
               | None -> registerSegvar (ps', v) :: es
              )
          | Element (_, r, t), es -> registerElement (r, _RR t) :: es
        in
        Some (registerCollection (k, nj_fold f es []))
    | _ -> None
  in
  option_mapterm _R term

let rec remapterm env = anyway (option_remapterm env)

(* convert a pattern into the most general form that will match it with the coarsest
 * punctuation - e.g. convert forall (x,y) . x+y into forall _P . Q
 * This is used in bindingstructure to check if a term provided by a user could be 
 * a failed attempt to make a binding structure.
 *)

let rec simplepat term =
  let idnum = ref 0 in
  let rec mkid c = 
    let r = registerId (vid_of_string (string_of_int !idnum), c) in 
    incr idnum; r 
  in
  let rec _S t =
    match t with
      Id _ -> mkid FormulaClass
    | Unknown _ -> mkid FormulaClass
    | App _ -> registerApp (mkid FormulaClass, mkid FormulaClass)
    | Tup (_, s, ts) -> registerTup (s, (_S <* ts))
    | Literal _ -> t
    | Fixapp (_, ss, ts) -> registerFixapp (ss, (_S <* ts))
    | Subst (_, r, _P, vts) ->
        registerSubst
          (r, mkid FormulaClass, (fun (v, t) -> _S v, _S t) <* vts)
    | Binding (_, vs, _, pat) -> _S pat
    | Collection (_, k, es) -> registerCollection (k, (_E <* es))
  and _E e =
    match e with
      Segvar (_, ps, v) -> registerSegvar (ps, _S v)
    | Element (_, r, t) -> registerElement (r, _S t)
  in
  _S term

(* for convenience, and backwards compatibility (:-)), 
 * a function which gives you the first good match of pat with term.
 *)
let rec matchvars matchbra ispatvar pat term mr =
  let rec best mrs =
    match mrs with
    | (Certain e as mr) :: _ -> Some mr
    | mr :: mrs -> (best mrs |~~ (fun _ -> Some mr))
    | [] -> None
  in
  best (matchtermvars matchbra ispatvar pat term [mr])

(********************** export section *****************************)

let match3vars = matchvars
let rec match3 matchbra = match3vars matchbra ismetav
let rec matchvars matchbra ispatvar pat t env =
    (match3vars matchbra ispatvar pat t (Certain env) &~~
     (function
        Certain e -> Some e
      | _ -> None))
let rec match__ matchbra = matchvars matchbra ismetav
let match3termvars = matchtermvars
let rec match3term matchbra = match3termvars matchbra ismetav
let rec matchtermvars matchbra ispatvar pat t envs =
  nj_fold
    (function
     | Certain e, es -> e :: es
     | Uncertain e, es -> es
    )
    (match3termvars matchbra ispatvar pat t (List.map (fun v->Certain v) envs)) []

let rec matchterm matchbra = matchtermvars matchbra ismetav
