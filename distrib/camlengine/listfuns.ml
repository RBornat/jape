(*
	$Id$

    Copyright (C) 2003-4 Richard Bornat & Bernard Sufrin
     
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
open Sml
          
exception First exception Reduce
(* Bird-Meertens folding *)
let rec foldr f z =
 function
    []      -> z
  | x :: xs -> f x (foldr f z xs)

let rec foldl f z =
  function
    []      -> z
  | x :: xs -> foldl f (f z x) xs

(* <| is infix filter *)
let ( <| ) = List.filter

(* <* is infix map *)
let ( <* ) = List.map

(* ||| is infix zip *)
exception Zip_ 
let ( ||| ) xs ys = 
  try List.combine xs ys with Invalid_argument "List.combine" -> raise Zip_

(* this appears to be map f o filter pp *)
let rec doubleslosh (f, pp) =
  let rec ff =
    function
      [] -> []
    | x :: xs -> if pp x then f x :: ff xs else ff xs
  in
  ff
(* /> looks like foldl *)

let rec ( /> ) ((e : 'a), (( ++ ) : 'a * 'b -> 'a)) : 'b list -> 'a =
  (* infix ++; *)
  let rec ff a1 a2 =
    match a1, a2 with
      r, [] -> r
    | r, x :: xs -> ff (( ++ ) (r, x)) xs
  in
  ff e
(* </ looks like foldr *)

let rec ( </ ) (( ++ ), e) =
  (* infix ++; *) let rec ff =
    function
      [] -> e
    | x :: xs -> ( ++ ) (x, ff xs)
  in
  ff
(* wassis? *)

let rec ( // ) =
  function
    ( ++ ), x :: xs -> ( /> ) (x, ( ++ )) xs
  | ( ++ ), [] -> raise Reduce


let _All f = not <.> List.exists (not <.> f)
let _All1 f xs = _All f xs && not (null xs)

let rec member (x, sf) = List.mem x sf
let rec nonmember (x, sf) = not (member (x, sf))

(* slosh (was \) is list subtract *)
let rec slosh = fun (sf, tt) -> (fun x -> nonmember (x, tt)) <| sf

(*
val set = [] /> (fn (s, e) => if e member s then s else e :: s)
*)

(* now we get this out in the same order as we gave it ... *)
let rec seteq eq xs =
  nj_revfold
    (fun (x, ys) ->
       if List.exists (fun x' -> eq x x') ys then ys else x :: ys)
    xs []

let rec set (xs : 'a list) =
  seteq ( = ) xs

let rec subset (xs, ys) = _All (fun x -> member (x, ys)) xs

let _INTER xs ys = (fun y -> member (y, xs)) <| ys

let rec interpolate a1 a2 =
  match a1, a2 with
    sep, [] -> []
  | sep, [s] -> [s]
  | sep, s1 :: ss -> s1 :: sep :: interpolate sep ss

let rec catelim_interpolate a1 a2 a3 a4 =
  match a1, a2, a3, a4 with
    f, sep, [], ys -> ys
  | f, sep, [x], ys -> f x ys
  | f, sep, x :: xs, ys -> f x (sep :: catelim_interpolate f sep xs ys)

let rec stringfn_of_catelim f x = implode (f x [])

let rec catelim_of_stringfn f x ss = f x :: ss

let rec catelim_string_of_list obstring punct =
  catelim_interpolate obstring punct

let rec catelim_liststring2 obstring sepn sep2 xs tail =
  match xs with
    [] -> tail
  | [x] -> obstring x tail
  | [x1; x2] -> obstring x1 (sep2 :: obstring x2 tail)
  | x :: xs ->
      obstring x (sepn :: catelim_liststring2 obstring sepn sep2 xs tail)

let rec catelim_bracketedstring_of_list obstring punct xs tail =
  "[" :: catelim_string_of_list obstring punct xs ("]" :: tail)

let rec string_of_list obstring punct =
  stringfn_of_catelim (catelim_string_of_list (catelim_of_stringfn obstring) punct)

let rec liststring2 obstring sepn sep2 =
  stringfn_of_catelim
    (catelim_liststring2 (catelim_of_stringfn obstring) sepn sep2)

let rec bracketedstring_of_list obstring punct =
  stringfn_of_catelim
    (catelim_bracketedstring_of_list (catelim_of_stringfn obstring) punct)

let rec replacenth a1 a2 a3 =
  match a1, a2, a3 with
    x :: xs, 0, y -> y :: xs
  | x :: xs, n, y -> x :: replacenth xs (n - 1) y
  | [], _, _ -> []
exception Last_

let rec last =
  function
    [x] -> x
  | _ :: xs -> last xs
  | [] -> raise Last_
(* these things revised to Bird-Meertens standards - no Failure "nth" here! *)

let rec take a1 a2 =
  match a1, a2 with
    0, xs -> []
  | n, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs

let rec drop a1 a2 =
  match a1, a2 with
    0, xs -> xs
  | n, [] -> []
  | n, x :: xs -> drop (n - 1) xs

let rec takewhile a1 a2 =
  match a1, a2 with
    f, x :: xs -> if f x then x :: takewhile f xs else []
  | f, [] -> []

let rec dropwhile a1 a2 =
  match a1, a2 with
    f, x :: xs -> if f x then dropwhile f xs else x :: xs
  | f, [] -> []

let rec _BMzip xs ys = 
  match xs, ys with
    x::xs, y::ys -> (x, y) :: _BMzip xs ys
  | _    , _     -> []

let rec isprefix a1 a2 a3 =
  match a1, a2, a3 with
    eq, [], ys -> true
  | eq, x :: xs, y :: ys -> eq (x, y) && isprefix eq xs ys
  | eq, _, _ -> false
exception Extract_

let rec extract a1 a2 =
  match a1, a2 with
    f, [] -> raise Extract_
  | f, x :: xs ->
      if f x then x, xs else let (y, ys) = extract f xs in y, x :: ys

let rec split a1 a2 =
  match a1, a2 with
    f, [] -> [], []
  | f, x :: xs ->
      let (yess, nos) = split f xs in
      if f x then x :: yess, nos else yess, x :: nos
(* smooth applicative merge sort
 * Taken from "ML for the Working Programmer", Paulson, pp 99-100
   - filched by us from SMLNJ profile.script
   - and changed, because that function sorted in > order.
 *)
exception Matchinmergepairs
(* spurious, shut up compiler *)
let rec sort (<) ls =
  let rec merge =
    function
      [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys ->
        if x < y then x :: merge (xs, y :: ys)
        else y :: merge (x :: xs, ys)
  in
  let rec mergepairs =
    function
      ([l] as ls), k -> ls
    | l1 :: l2 :: ls, k ->
        if k mod 2 = 1 then l1 :: l2 :: ls
        else mergepairs (merge (l1, l2) :: ls, k / 2)
    | _ -> raise Matchinmergepairs
  in
  let rec nextrun =
    function
      run, [] -> run, []
    | run, x :: xs ->
        if x < List.hd run then nextrun (x :: run, xs) else run, x :: xs
  in
  let rec samsorting =
    function
      [], ls, k -> List.hd (mergepairs (ls, 0))
    | x :: xs, ls, k ->
        let (run, tail) = nextrun ([x], xs) in
        samsorting (tail, mergepairs (run :: ls, k + 1), k + 1)
  in
  match ls with
    [] -> []
  | _ -> samsorting (ls, [], 0)

let rec sortandcombine (<) ( ++ ) ls =
  (* infix ++ *)
  let rec merge =
    function
      [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys ->
        if x < y then x :: merge (xs, y :: ys)
        else if y < x then y :: merge (x :: xs, ys)
        else ( ++ ) (x, y) :: merge (xs, ys)
  in
  let rec mergepairs =
    function
      ([l] as ls), k -> ls
    | l1 :: l2 :: ls, k ->
        if k mod 2 = 1 then l1 :: l2 :: ls
        else mergepairs (merge (l1, l2) :: ls, k / 2)
    | _ -> raise Matchinmergepairs
  in
  let rec nextrun =
    function
      run, [] -> run, []
    | run, x :: xs ->
        if x < List.hd run then nextrun (x :: run, xs) else run, x :: xs
  in
  let rec samsorting =
    function
      [], ls, k -> List.hd (mergepairs (ls, 0))
    | x :: xs, ls, k ->
        let (run, tail) = nextrun ([x], xs) in
        samsorting (tail, mergepairs (run :: ls, k + 1), k + 1)
  in
  match ls with
    [] -> []
  | _ -> samsorting (ls, [], 0)
(* remdups removes consecutive duplicates *)

let rec remdups =
  function
    [] -> []
  | [x] -> [x]
  | x1 :: x2 :: xs ->
      let rest = remdups (x2 :: xs) in
      if x1 = x2 then rest else x1 :: rest

let rec sortunique (<) = remdups <.> sort (<)

let rec earlierlist a1 a2 a3 =
  match a1, a2, a3 with
    (<), x :: xs, y :: ys ->
      x < y || not (y < x) && earlierlist (<) xs ys
  | _, [], [] -> false
  | _, [], _  -> true
  | _, _, _  -> false
(* lists sorted by < or <=; does set diff or bag diff accordingly *)

let rec sorteddiff a1 a2 a3 =
  match a1, a2, a3 with
    (<), [], ys -> []
  | (<), xs, [] -> xs
  | (<), x1 :: xs, y1 :: ys ->
      if x1 = y1 then sorteddiff (<) xs ys
      else if x1 < y1 then
        x1 :: sorteddiff (<) xs (y1 :: ys)
      else sorteddiff (<) (x1 :: xs) ys
(* lists sorted by < or <=; does set or bag intersect accordingly *)

let rec sortedsame a1 a2 a3 =
  match a1, a2, a3 with
    (<), [], ys -> []
  | (<), xs, [] -> []
  | (<), x1 :: xs, y1 :: ys ->
      if x1 = y1 then x1 :: sortedsame (<) xs ys
      else if x1 < y1 then sortedsame (<) xs (y1 :: ys)
      else sortedsame (<) (x1 :: xs) ys
(* given sorted by < -- no duplicates -- lists. designed to be folded ... *)

let rec sortedmergeandcombine (<) ( + ) xs ys =
  let rec s a1 a2 =
    match a1, a2 with
      [], ys -> ys
    | xs, [] -> xs
    | x1 :: xs, y1 :: ys ->
        if x1 < y1 then x1 :: s xs (y1 :: ys)
        else if y1 < x1 then y1 :: s (x1 :: xs) ys
        else x1 + y1 :: s xs ys
  in
  s xs ys

let rec sortedmerge (<) xs ys =
  sortedmergeandcombine (<) (fun x _ -> x) xs ys
(* this ignores elements of ys after the last one that actually occurs in xs *)

let rec sortedlistsub eq xs ys =
  let rec g a1 a2 =
    match a1, a2 with
      xs, y :: ys ->
        let rec f =
          function
            x :: xs -> if eq (x, y) then g xs ys else x :: f xs
          | [] -> []
        in
        f xs
    | xs, [] -> xs
  in
  g xs ys
(* matchbag pp XS = { (x, pp x, XS -- [x]) | x<-XS ; x in dom pp } *)

let rec matchbag pp xs =
  let rec match__ a1 a2 a3 =
    match a1, a2, a3 with
      r, pre, [] -> r
    | r, pre, x :: xs ->
        match pp x with
          Some y -> match__ ((x, y, revapp pre xs) :: r) (x :: pre) xs
        | None -> match__ r (x :: pre) xs
  and revapp a1 a2 =
    match a1, a2 with
      [], ys -> ys
    | x :: xs, ys -> revapp xs (x :: ys)
  in
  match__ [] [] xs

let rec ( >< ) xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> x, y) ys) xs)

(* this function isn't xs><xs -- (xs,xs): 
 * it's the upper triangle (or the lower one) of the matrix xs><xs, without
 * the diagonal.  So it finds all distinct pairs, not pairing x with x and only
 * adding one of <x,y> and <y,x>.  Distinct, that is, if the list xs doesn't contain
 * duplicates.  The _first_ element of the list appears only as the first of a pair;
 * the _last_ element of the list appears only as the second of a pair.
 *)
let rec allpairs xs =
  (fun (_, r) -> r)
    (nj_fold
       (fun (x, (ys, ps)) ->
          x :: ys, nj_fold (fun (y, ps) -> (x, y) :: ps) ys ps)
       xs ([], []))
(* this function is and (map ee (bernardszip (xs,ys))) handle bernardszip_ => false, 
   but I'm trying to avoid consing ... Is that necessary or even a good idea?
   There's a lot of tupling going on ...
 *)

let rec eqlists a1 a2 =
  match a1, a2 with
    ee, ([], []) -> true
  | ee, (x :: xs, y :: ys) -> ee (x, y) && eqlists ee (xs, ys)
  | ee, _ -> false

let rec numbered xs =
  let rec r a1 a2 =
    match a1, a2 with
      n, x :: xs -> (n, x) :: r (n + 1) xs
    | _, [] -> []
  in
  r 0 xs
(* list subtraction, often faster than the list function slosh *)

let rec listsub eq xs ys =
  let rec sf a1 a2 =
    match a1, a2 with
      [], ys -> []
    | xs, [] -> xs
    | xs, y :: ys ->
        let rec strip =
          function
            [] -> []
          | x :: xs -> if eq (x, y) then xs else x :: strip xs
        in
        sf (strip xs) ys
  in
  sf xs ys

let rec eqbags =
  fun ee (xs, ys) -> List.length xs = List.length ys && null (listsub ee xs ys)
(* first attempt at a topological sort: doesn't try to coalesce cycles *)

let rec toposort roots depf =
  let rec ts visited (root, (order, cycles)) =
    if member (root, visited) then
      let cycle =
        root :: List.rev (root :: takewhile (fun x -> x <> root) visited)
      in
      order, cycle :: cycles
    else if member (root, order) then order, cycles
    else
      let children = depf root in
      let (order, cycles) =
        (* nj_revfold to get answer out in children order (whatever it is) *)
        nj_revfold (ts (root :: visited)) children (order, cycles)
      in
      root :: order, cycles
  in
  (* nj_revfold to get answer out in roots order *)
  nj_revfold (ts []) roots ([], [])
