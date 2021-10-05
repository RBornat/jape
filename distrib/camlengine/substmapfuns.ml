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
open Mappingfuns
open Optionfuns
open Answer
open Termfuns
open Proviso
open Facts
open Sml
open Miscellaneous
open Termstring
open Termtype
open Termstore

let consolereport = Miscellaneous.consolereport
let _NotinProviso = Provisotype._NotinProviso

let substdebug = ref false
let rec substmapdom (vts : (term * term) list) = (fst <* vts)
let rec substmapran (vts : (term * term) list) = (snd <* vts)

(* varmappedbyq doesn't always take a VAR as its first parameter *)
(* is that true any more? RB May 95 *)

let rec varmappedbyq facts v vts = varboundbyq facts v (substmapdom vts)

(* the vars in varboundbyq are all VARs, aren't they? But if the proof doesn't
   say so, we don't know so.
 *)
(* Now we do, because of VariableClass and its friends *)

and varboundbyq facts v vars = existsq (substeqvarsq facts v) vars
(* varmappedto is None if we can't say, Some v if it doesn't map, Some term if it does *)

let rec varmappedto facts v vts =
  let rec check a1 a2 =
    match a1, a2 with
      def, (v', t') :: vts ->
        begin match substeqvarsq facts v v' with
          Yes -> Some t'
        | Maybe -> check None vts
        | No -> check def vts
        end
    | def, [] -> def
  in
  check (Some v) vts

let rec vtmetacount vts =
  nj_fold (fun (x, y) -> x + y)
    ((fun (v, _) -> if isUnknown v then 1 else 0) <* vts) 0

let rec vtmaps facts vts (v, _) =
  List.exists (fun (v', _) -> qDEF (substeqvarsq facts v v')) vts

let rec vtminus facts vts1 vts2 =
  (not <.> vtmaps facts vts2) <| vts1

let rec vtsplit facts vts bs =
  let rec _S ((v, t), (ys, ns, ms)) =
    let rs = (substeqvarsq facts v <* bs) in
    if List.exists qDEF rs then (v, t) :: ys, ns, ms
    else if List.exists qUNSURE rs then ys, ns, (v, t) :: ms
    else ys, (v, t) :: ns, ms
  in
  nj_fold _S vts ([], [], [])

exception Whoops_ (* moved outside for OCaml *)
       
(* Moving a map through another map. If this works you get a new map *)
let rec plussubstmap facts vtout vtin =
  let vsin = substmapdom vtin in
  match vtsplit facts vtout vsin with
    ys, ns, [] ->
      Some (
           ((fun (v, t) -> v, simplifySubstAnyway facts vtout t) <* vtin) @
           ns)
  | _ -> None
(* Moving a map through a binder.  If this works you get a new map *)

and restrictsubstmap facts vts bs ss =
  let rec res f r =
    if !substdebug then
      consolereport
        ["restrictsubstmap "; string_of_vts vts; " "; string_of_termlist bs; " ";
         string_of_termlist ss; " => "; f ()];
    r
  in
  let (ys, ns, ms) = vtsplit facts vts bs in
  let rec newfacts v =
    expandfacts facts ((fun b -> _NotinProviso (v, b)) <* bs)
  in
  let rec foundinside v =
    List.exists
         (not <.> qDEFNOT <.> varoccursinq (newfacts v) v)
      ss
  in
  if not (List.exists foundinside (substmapdom ms)) then
    if not
         (List.exists
            (fun b ->
               List.exists
                    (not <.> qDEFNOT <.> varoccursinq facts b)
                 (substmapran ns))
            bs)
    then
      res (fun _ -> string_of_vts ns) (Some ns)
    else res (fun _ -> "failure cos of unsafeity (sic)") None
  else res (fun _ -> "failure cos of uncertainty") None

and varoccursinq facts v =
  fun _P ->
    let _EF = exterioreqvarsq facts in
    let rec _EFv v' =
      if canoccurfreein (idclass v, idclass v') then _EF v v' else No
    in
    let _OF = varoccursinq facts in
    let _OFv = _OF v in
    let _BFv = varboundbyq facts v in
    let _MFv = varmappedbyq facts v in
    let rec res r =
      fun _P ->
        if !substdebug then
          begin
            consolereport
              ["varoccursinq "; string_of_facts facts; " "; string_of_term v; " ";
               string_of_termarg _P; " => "; string_of_answer r];
            r
          end
        else r
    in
    let rec insidebinding bs ss =
      match _BFv bs with
        Yes -> No
      | No -> existsq (varoccursinq facts v) ss
      | Maybe ->
          let newfacts =
            expandfacts facts ((fun b -> _NotinProviso (v, b)) <* bs)
          in
          existsq (varoccursinq newfacts v) ss
    in
    (* search function to be folded across terms *)
    let rec search =
      fun _P ->
        if knownNOTIN facts (v, _P) then Some (res No _P)
        else
          match _P with
            Id _ -> Some (res (_EFv _P) _P)
          | Unknown _ -> Some (res (_EFv _P) _P)
          | Literal _ -> Some (res No _P)
          | Subst (_, _, _P, vts) ->
              Some
                (res
                   (orelseq
                      (existsq
                         (fun (v', t') ->
                            andalsoq (_OFv t') (fun _ -> _OF v' _P))
                         vts)
                      (fun _ -> insidebinding (substmapdom vts) [_P]))
                   _P)
          | Binding (_, (bs, ss, us), _, _) ->
              Some
                (res
                   (orelseq (existsq _OFv us)
                      (fun _ -> insidebinding bs ss))
                   _P)
          | _ -> None
    in
    let rec foldsearch =
      fun (_P, sofar) ->
        match sofar with
          Yes -> Some Yes
        | No -> search _P
        | Maybe ->
            match search _P with
              None -> None
            | Some v -> Some (orq (sofar, v))
    in
    foldterm foldsearch No _P

and simplifySubstAnyway facts vts =
  fun _P ->
    match simplifySubst facts vts _P with
      Some t -> t
    | None -> registerSubst (true, _P, vts)
(*
   simplifySubst and simplifysubstmap are partial functions.
   simplifySubst won't deliver a reducible substitution.
*)

and simpres vts =
  fun _P r ->
    consolereport
      ["simplifySubst "; " "; string_of_term (Subst (None, true, _P, vts));
       " => "; string_of_option string_of_term r]

and simplifySubst a1 a2 a3 =
  match a1, a2, a3 with
    facts, [], (Subst (_, _, _P', vts') as _P) ->
      let r = Some (simplifySubstAnyway facts vts' _P') in
      if !substdebug then simpres [] _P r; r
  | facts, [], _P ->
      let r = Some _P in if !substdebug then simpres [] _P r; r
  | facts, vts, _P ->
      let rec res r = if !substdebug then simpres vts _P r; r in
      let _S = simplifySubstAnyway facts in
      let rec more vts' = fun _P' -> Some (_S vts' _P') in
      let rec _Svar v =
        match varmappedto facts v vts with
          Some (Subst (_, _, _P', vts')) -> res (more vts' _P')
        | Some t -> res (Some t)
        | None -> fail v
      and fail =
        fun _P ->
          match simplifysubstmap facts _P vts with
            Some vts' -> res (more vts' _P)
          | None -> res None
      in
      match _P with
        Id _ -> _Svar _P
      | Unknown _ -> _Svar _P
      | App (_, f, a) -> res (Some (registerApp (_S vts f, _S vts a)))
      | Tup (_, sep, ts) ->
          res (Some (registerTup (sep, (_S vts <* ts))))
      | Literal k -> res (Some _P)
      | Fixapp (_, ss, ts) ->
          res (Some (registerFixapp (ss, (_S vts <* ts))))
      | Subst (_, _, _P', vts') ->
          begin match plussubstmap facts vts vts' with
            Some vts'' -> res (more vts'' _P')
          | None -> fail _P
          end
      | Binding (_, (bs, ss, us), env, pat) ->
          begin match restrictsubstmap facts vts bs ss with
            Some vts' ->
              res
                (Some
                   (registerBinding
                      ((bs, (_S vts' <* ss), (_S vts <* us)), env,
                       pat)))
          | None -> fail _P
          end
      | Collection (_, k, es) ->
          let rec se =
            function
              Element (_, _, t) -> registerElement (Nonum, _S vts t)
            | _ -> raise Whoops_
          in
          res
            (try Some (registerCollection (k, (se <* es))) with
               Whoops_ -> None)

and simplifysubstmap facts =
  fun _P vts ->
    (* _W detects substitutions in substitutions and takes them out if poss *)
    (* _V detects v slosh v and y slosh E where y NOTIN _P *)
    (* _U takes out those elements in a very silly way *)
    let rec _W =
      function
        v, Subst (_, r, _P, m) ->
          begin match simplifySubst facts m _P with
            Some t -> Some (v, t)
          | None -> None
          end
      | _ -> None
    in
    let rec _V (v, t) =
      eqterms (v, t) || qDEFNOT (varoccursinq facts v _P)
    in
    let rec _U =
      function
        [] -> None
      | vt :: vts ->
          if _V vt then Some (anyway _U vts)
          else
            match _W vt with
              Some vt -> Some (anyway _U (vt :: vts))
            | None ->
                match _U vts with
                  Some vts -> Some (vt :: vts)
                | None -> None
    in
    let r = _U vts in
    if !substdebug then
      consolereport
        ["simplifysubstmap "; " ";
         string_of_term (Subst (None, true, _P, vts)); " => ";
         string_of_option string_of_vts r];
    r
