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
open Match
open Miscellaneous
open Optionfuns
open Stringfuns
open Termfuns
open Termstring

type term = Termtype.term

let consolereport = Miscellaneous.consolereport

let bindingdebug = ref false

type bindingdirective = term list * term list * term list * term

let bindingdirectives = Array.make (termkindmax + 1) ([] : bindingdirective list)

let badbindings = Array.make (termkindmax + 1) ([] : term list)

let findbadbinding t bbs = findfirst (fun bb -> match__ false bb t empty) bbs

let string_of_bindingdirective =
  string_of_quadruple
    (bracketed_string_of_list string_of_term "; ")
    (bracketed_string_of_list string_of_term "; ")
    (bracketed_string_of_list string_of_term "; ")
    string_of_term ", "

let samedirective ((bs1, ss1, us1, pat1) as binding1)
    ((bs2, ss2, us2, pat2) as binding2) =
  (* Basically, do they match. Takes a deal of ingenuity this way, but it's so pretty
     I couldn't resist it.
  *)
  let r =
    try
      bool_of_opt
        (option_foldl
           (option_foldr (uncurry2 (match__ false)))
           empty
           [ bs1 ||| bs2; ss1 ||| ss2; us1 ||| us2; [ (pat1, pat2) ] ])
    with Zip_ -> false
  in
  if !bindingdebug then
    consolereport
      [
        (if r then "equal " else "unequal ");
        string_of_bindingdirective binding1;
        " ";
        string_of_bindingdirective binding2;
      ];
  r

let addbindingdirective ((bs, ss, us, pat) as directive) =
  if !bindingdebug then
    consolereport
      [ "Binding.addbindingdirective "; string_of_bindingdirective directive ];
  let k = termkind pat in
  let bds = bindingdirectives.(k) in
  let bbs = badbindings.(k) in
  (* test could be refined, but works for multiple re-loads of same syntax *)
  if List.exists (samedirective directive) bds then ()
  else (
    bindingdirectives.(k) <- directive :: bds;
    ( match findbadbinding pat bbs with
    | None -> badbindings.(k) <- pat :: bbs
    | Some _ -> () );
    if !bindingdebug then
      let bt = string_of_termlist in
      let qt = string_of_quadruple bt bt bt string_of_term "," in
      consolereport
        [
          "bindings type ";
          string_of_int k;
          " are now ";
          bracketed_string_of_list qt "; " bindingdirectives.(k);
          "; and bad bindings type ";
          string_of_int k;
          " are ";
          bt badbindings.(k);
        ] )

let clearbindingdirectives () =
  let rec mklist n = if n >= 0 then n :: mklist (n - 1) else [] in
  List.iter
    (fun i ->
      bindingdirectives.(i) <- [];
      badbindings.(i) <- [])
    (mklist termkindmax)

let bindingstructure =
  let rec lookup env x = _The (env <@> x)
  and matchterm term (bounds, scopes, unscopes, pat) =
    match match__ false pat term empty with
    | None -> None
    | Some mapping ->
        let rec fornth a1 a2 a3 =
          match (a1, a2, a3) with
          | k, i, v :: vs -> (v, (k, i)) :: fornth k (i + 1) vs
          | k, i, [] -> []
        in
        let env =
          (fornth 1 0 bounds @ fornth 2 0 scopes) @ fornth 3 0 unscopes
        in
        let _E = lookup mapping in
        Some ((_E <* bounds, _E <* scopes, _E <* unscopes), env, pat)
  in
  let doit t =
    let k = termkind t in
    match findfirst (matchterm t) bindingdirectives.(k) with
    | None -> (
        match findbadbinding t badbindings.(k) with
        | Some _ ->
            raise
              (ParseError_
                 [
                   string_of_term t;
                   " is almost a binding structure, but not quite";
                 ])
        | None -> None )
    | res -> res
  in
  doit
