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


open Sml

exception None_

let _The = function Some x -> x | None -> raise None_
let _Some x = Some x

let rec bool_of_opt = function Some _ -> true | None -> false

let rec optf f =
  function
    Some x -> Some (f x)
  | None   -> None

let rec somef f x =
  match f x with
    None -> Some x
  | v    -> v

let rec anyway f x =
  match f x with
    Some v -> v
  | None   -> x
  
let rec failpt f x =
  match f x with
    Some y -> failpt f y
  | None -> x

let rec (&~~) v g =
  match v with 
    None    -> None
  | Some v' -> g v'

let rec (|~~) v g =
  match v with
    None -> g ()
  | v    -> v

let rec (&~) f g x = f x &~~ g

let rec (|~) f g x =
  match f x with
    None -> g x
  | v    -> v

let rec optioncompose (f, g) x =
  match g x with
    Some y -> Some (f y)
  | None   -> None

let rec optionmap f  =
  function
    []      -> Some []
  | x :: xs -> f x &~~ (fun x -> (optionmap f xs &~~ (fun xs -> Some (x::xs))))
  
let rec optionfilter a1 a2 =
  match a1, a2 with
    f, [] -> []
  | f, x :: xs ->
      match f x with
        Some x' -> x' :: optionfilter f xs
      | None -> optionfilter f xs
      
let rec option_foldl f z =
  function
    []      -> Some z
  | x :: xs -> f z x &~~ (fun z' -> option_foldl f z' xs) 
  
let rec option_foldr f z =
  function
    []      -> Some z
  | x :: xs -> option_foldr f z xs &~~ f x
  
let rec option_njfold f =
  function
    []      -> _Some
  | x :: xs -> option_njfold f xs &~ (Miscellaneous.curry2 f) x

let rec findfirst f =
  function
    []      -> None
  | x :: xs -> match f x with
                 None -> findfirst f xs
               | t    -> t

let rec findbest f best =
  function
    []      -> None
  | x :: xs -> match f x with
                 None -> findbest f best xs
               | Some y1 ->
                   match findbest f best xs with
                     None    -> Some y1
                   | Some y2 -> Some (best y1 y2)

let rec stripoption =
  function
    None   -> None
  | Some x -> x

let rec optordefault =
  function
    Some v, _ -> v
  | None  , v -> v

let rec catelim_string_of_option catelim_astring aopt ss =
  match aopt with
    Some a -> "Some (" :: catelim_astring a (")" :: ss)
  | None   -> "None" :: ss
let rec string_of_option astring aopt =
  implode (catelim_string_of_option (fun a ss -> astring a :: ss) aopt [])

(* save space when rewriting structures *)
let rec option_rewrite2 fa fb (a, b) =
  match fa a, fb b with
    Some a, Some b -> Some (a, b)
  | Some a, None   -> Some (a, b)
  | None  , Some b -> Some (a, b)
  | None  , None   -> None

(* the next two could be composed from option_rewrite2, but that would cause churn *)
let rec option_rewrite3 fa fb fc (a, b, c) =
  match fa a, fb b, fc c with
    Some a, Some b, Some c -> Some (a, b, c)
  | Some a, Some b, None   -> Some (a, b, c)
  | Some a, None  , Some c -> Some (a, b, c)
  | Some a, None  , None   -> Some (a, b, c)
  | None  , Some b, Some c -> Some (a, b, c)
  | None  , Some b, None   -> Some (a, b, c)
  | None  , None  , Some c -> Some (a, b, c)
  | None  , None  , None   -> None

let rec option_rewritelist f xs =
  match xs with
    []    -> None
  | x::xs -> match f x, option_rewritelist f xs with
               Some x, Some xs -> Some (x::xs)
             | Some x, None    -> Some (x::xs)
             | None  , Some xs -> Some (x::xs)
             | None  , None    -> None
        
