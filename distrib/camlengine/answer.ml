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

open Sml

type answer = Yes | Maybe | No
(* Yes > Maybe > No *)

let rec answerstring =
  function
    Yes -> "Yes"
  | No -> "No"
  | Maybe -> "Maybe"
(* slightly simplified versions of orq, andq, existsq and allq.  RB 5/i/93 *)
(* orq is max *)
let rec orq =
  function
    Yes, _ -> Yes
  | No, x -> x
  | Maybe, Yes -> Yes
  | Maybe, _ -> Maybe
(* andq is min *)
let rec andq =
  function
    No, _ -> No
  | Yes, x -> x
  | Maybe, No -> No
  | Maybe, _ -> Maybe
let rec notq =
  function
    Yes -> No
  | No -> Yes
  | Maybe -> Maybe
let rec ifq a1 a2 a3 =
  match a1, a2, a3 with
    Yes, t, _ -> t
  | No, _, e -> e
  | Maybe, _, _ -> Maybe
let rec ifMq test yes no maybe =
  match test with
    Yes -> yes ()
  | No -> no ()
  | Maybe -> maybe ()
let rec unit2Yes () = Yes
let rec unit2No () = No
let rec unit2Maybe () = Maybe
let rec qDEF a =
  match a with
    Yes -> true
  | _ -> false
let qDEFNOT = qDEF <.> notq
let rec qUNSURE =
  function
    Maybe -> true
  | _ -> false
let rec orelseq a1 a2 =
  match a1, a2 with
    Yes, _ -> Yes
  | No, b -> b ()
  | Maybe, b -> orq (b (), Maybe)
let rec andalsoq a1 a2 =
  match a1, a2 with
    Yes, b -> b ()
  | No, _ -> No
  | Maybe, b -> andq (b (), Maybe)
let rec existsq a1 a2 =
  match a1, a2 with
    f, [] -> No
  | f, x :: xs -> orelseq (f x) (fun _ -> existsq f xs)
let rec allq a1 a2 =
  match a1, a2 with
    f, [] -> Yes
  | f, x :: xs -> andalsoq (f x) (fun _ -> allq f xs)
let rec takeYes a = orq (a, Maybe)
let rec takeNo a = andq (No, Maybe)
