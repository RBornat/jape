(*
    Copyright (C) 2003-17 Richard Bornat & Bernard Sufrin
     
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

type prestring =
  Prestr of string | Prestrs of string list | Prepres of prestring list

let pre_string s = Prestr s
let pre__comma   = Prestr ", "
let pre__nil     = Prepres []
let pre__space   = Prestr " "

(*  Primitive preprinters *)

let rec pre_int (i : int) = Prestr (string_of_int i)
let rec pre_real (r : float) = Prestr (string_of_float r)
let rec pre_unit () = Prestr "()"
let rec pre_bool (b : bool) = Prestr (string_of_bool b)

(*  Built-in constructors *)

let pre_option a1 a2 =
  match a1, a2 with
    f, None   -> Prestr "None"
  | f, Some x -> Prepres [Prestr "Some("; f x; Prestr ")"]

let pre_array f a =
  let s = Array.length a in
  let rec p n =
    if n = s then []
    else
      f (Array.get a n) ::
        (if n + 1 = s then pre__nil else pre__space) :: p (n + 1)
  in
  Prepres (p 0)

let pre_vector f a =
  let s = Array.length a in
  let rec p n =
    if n = s then []
    else
      Prepres [pre_int n; Prestr ":"; f (Array.get a n)] ::
        (if n + 1 = s then pre__nil else pre__comma) :: p (n + 1)
  in
  Prepres [Prestr "["; Prepres (p 0); Prestr "]"]

exception Matchinpre_Comma (* spurious *)

let rec pre_Comma a1 a2 =
  match a1, a2 with
    f, [] -> Prepres []
  | f, [x] -> Prepres [f x]
  | f, x :: xs ->
      match pre_Comma f xs with
        Prepres ps -> Prepres (f x :: pre__comma :: ps)
      | _ -> raise Matchinpre_Comma

let pre_Tuple f xs =
  Prepres [Prestr "("; pre_Comma f xs; Prestr ")"]

let pre_Set f xs = Prepres [Prestr "{"; pre_Comma f xs; Prestr "}"]

let pre_List f xs =
  Prepres [Prestr "["; pre_Comma f xs; Prestr "]"]

let pre_list f xs =
  Prepres [Prestr "["; pre_Comma f xs; Prestr "]"]

let pre_implode p =
  let rec _I (p,ss) =
    match p with
      Prestr s -> s :: ss
    | Prestrs sl -> sl @ ss
    | Prepres ps -> nj_fold _I ps ss
  in
  implode (_I (p,[]))

let rec pre_app a1 a2 =
  match a1, a2 with
    p, Prestr s   -> p s
  | p, Prestrs ss -> List.iter p ss
  | p, Prepres ps -> List.iter (pre_app p) ps

let ascii_chars = "0123456789abcdef"

let pre_Ascii s =
  Sml.implode (List.map (fun c -> if Char.code c<=0x7f then String.make 1 c else 
                                    let s = String.create 4 in
                                    s.[0]<-'\\'; s.[1]<-'x';
                                    s.[2]<-ascii_chars.[Char.code c lsr 4];
                                    s.[3]<-ascii_chars.[Char.code c land 0xf];
                                    s)
                        (Sml.chars_of_string s))
