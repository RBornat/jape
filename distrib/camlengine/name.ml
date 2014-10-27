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

open Nametype

open Miscellaneous
open Stringfuns
open Symbol 
open Symboltype 
open Termfuns
open Termtype 
open Termparse

type name = Nametype.name
 and term = Termtype.term

let string_of_name (Name s) = s
let nameorder (Name s1) (Name s2) = s1 < s2

(* this thing ain't cateliminated.  Can you see a way to do it? 
 * I can't: not without a cateliminated explode, and even then ...
 *)
let rec parseablestring_of_name =
  fun (Name s) ->
    let rec parsename sy =
      match currsymb () with
        ID _     -> scansymb ()
      | STRING _ -> raise (Catastrophe_ ["double quoting in Name "; s])
      | _        -> raise (ParseError_ [])
    in
    try let _ = tryparse parsename s in s with
      ParseError_ _ -> enQuote s 
        (* String.escaped would seem to be useful here, but it calls isprint 
           (see the Unix manual) and that, on MacOS X if nowhere else, objects 
           to most of the funny characters in Konstanz font (for example).
           So no escaping for the time being, unless and until we can tell the 
           system what font we want isprint to work in.
         *)

let rec nameopt_of_term t =
  match t with
  | Id (_, v, _)          -> Some (Name (string_of_vid v))
  | Unknown (_, v, _)     -> Some (Name (metachar_as_string ^ string_of_vid v))
  | Literal (_, Number s) -> Some (Name (string_of_int s))
  | Literal (_, String s) -> Some (Name s)
  | _                     -> None

(* this is better than disQuote, because it parses the string *)
let rec name_of_string s =
  try
    match tryparse parseTerm s with
      Literal (_, String s) -> Name s
    | _ ->(* without the quotes *)
       raise (ParseError_ [])
  with
    ParseError_ _ -> Name s
