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

open Listfuns
open Mappingfuns 
open Match
open Miscellaneous
open Optionfuns
open Stringfuns
open Term.Funs
open Term.Termstring

type term = Term.Funs.term

let bindingdebug = ref false
type bindingdirective = term list * term list * term list * term
let bindingdirectives =
  Array.make (termkindmax + 1) ([] : bindingdirective list)
let badbindings = Array.make (termkindmax + 1) ([] : term list)
let rec findbadbinding t bbs =
  findfirst (fun bb -> match__ false bb t empty) bbs
let rec addbindingdirective (bs, ss, us, pat as directive) =
  let k = termkind pat in
  let bds = Array.get bindingdirectives k in
  let bbs = Array.get badbindings k in
  (* test could be refined, but works for multiple re-loads of same syntax *)
  if List.exists (fun bd -> bd = directive) bds then ()
  else
    begin
      Array.set bindingdirectives k (directive :: bds);
      begin match findbadbinding pat bbs with
        None -> Array.set badbindings k (pat :: bbs)
      | Some _ -> ()
      end;
      if !bindingdebug then
        let bt = termliststring in
        let qt = quadruplestring bt bt bt termstring "," in
        consolereport
          ["bindings type "; string_of_int k; " are now ";
           bracketedliststring qt "; " (Array.get bindingdirectives k);
           "; and bad bindings type "; string_of_int k; " are ";
           bt (Array.get badbindings k)]
    end
let rec clearbindingdirectives () =
  let rec mklist n = if n >= 0 then n :: mklist (n - 1) else [] in
  List.iter
    (fun i ->
       Array.set bindingdirectives i [];
       Array.set badbindings i [])
    (mklist termkindmax)
let bindingstructure =
  let rec lookup env x = _The ((env <@> x))
  and matchterm term (bounds, scopes, unscopes, pat) =
    match match__ false pat term empty with
      None -> None
    | Some mapping ->
        let rec fornth a1 a2 a3 =
          match a1, a2, a3 with
            k, i, v :: vs -> (v, (k, i)) :: fornth k (i + 1) vs
          | k, i, [] -> []
        in
        let env =
          (fornth 1 0 bounds @ fornth 2 0 scopes) @ fornth 3 0 unscopes
        in
        let _E = lookup mapping in
        Some
          (((_E <* bounds), (_E <* scopes), (_E <* unscopes)), env,
           pat)
  in
  let rec bindingstructure t =
    let k = termkind t in
    match findfirst (matchterm t) (Array.get bindingdirectives k) with
      None ->
        begin match findbadbinding t (Array.get badbindings k) with
          Some _ ->
            raise
              (ParseError_
                 [termstring t;
                  " is almost a binding structure, but not quite"])
        | None -> None
        end
    | res -> res
  in
  bindingstructure
