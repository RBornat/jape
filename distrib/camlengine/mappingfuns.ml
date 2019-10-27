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
open Sml

type ('a, 'b) mapping = ('a * 'b) list

let empty = []

let isempty xs = xs=[]

let rec ( |-> ) a b = [a, b]

let rec ( ++  ) m n = n @ m

let rec mapped same mapping a =
  let rec ff =
    function
      [] -> None
    | (x, y) :: mapping -> if same (x, a) then Some y else ff mapping
  in
  ff mapping

(* -- [x] is the inverse of ++ x |-> : that is, it deletes only the outermost value of x.
 * -- [x,x] deletes two, and so on.
 *) 

let rec ( -- ) xs ys =
  match (xs,ys) with
    [], ys -> []
  | (x, xv) :: ps, ys ->
      if member (x, ys) then
        (ps -- listsub (fun (x, y) -> x = y) ys [x])
      else (x, xv) :: (ps -- ys)

let rec (<@>) mapping a = mapped (fun (x, y) -> x = y) mapping a (* eta-conversion doesn't work here *)

let rec mem a1 a2 =
  match a1, a2 with
    x, [] -> false
  | x, x' :: xs -> x = x' || mem x xs

let rec lfold f r m =
  let rec ff a1 a2 a3 =
    match a1, a2, a3 with
      dom, r, [] -> r
    | dom, r, (x, y as pair) :: map ->
        if mem x dom then ff dom r map
        else ff (x :: dom) (f (pair, r)) map
  in
  ff [] r m

let rec remapping (f, mapping) =
  lfold
    (fun (pair, m) -> (uncurry2 (|->) (f pair) ++ m))
    empty mapping
(* dom now gives its result in reverse insertion order, just like rawdom *)

let aslist (m : ('a * 'b) list) =
  seteq (fun (a, b) (a1, b1) -> a = a1) m
  
let fromlist m = m

let rec dom m = List.map (fun (r,_)->r) (aslist m)

let rec ran m = List.map (fun (_,r)->r) (aslist m)

let rec rawaslist m = m

let rec rawdom (m : ('a, 'b) mapping) = List.map (fun (r,_)->r) m

let rec rawran (m : ('a, 'b) mapping) = List.map (fun (_,r)->r) m

let rec formappingpairs (f, mapping) =
  List.iter (fun d -> f (d, Optionfuns._The ((mapping <@> d)))) (dom mapping)

let rec mkmap pairs =
  nj_fold (fun ((a, b), map) -> (map ++ (a |-> b))) pairs empty

let rec catelim_string_of_mapping astring bstring sep mapping ss =
  "<<" ::
    catelim_string_of_list
      (fun (a, b) ss -> "(" :: astring a ("|->" :: bstring b (")" :: ss)))
      sep (List.rev mapping) (">>" :: ss)

let rec string_of_mapping a b =
  stringfn_of_catelim
    (catelim_string_of_mapping (catelim_of_stringfn a) (catelim_of_stringfn b) "++")
