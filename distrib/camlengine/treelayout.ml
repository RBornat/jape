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

open Match
open Optionfuns
open Stringfuns
open Termfuns
open Termstring

type term = Termtype.term

type treelayout =
    HideRootLayout
  | HideCutLayout
  | CompressedLayout of (term * term option)
  | NamedLayout of (term * term option)
(* fmt * list of subtrees to show *)

let rec string_of_treelayout =
  function
    HideRootLayout -> "HIDEROOT"
  | HideCutLayout -> "HIDECUT"
  | CompressedLayout stuff ->
      begin match tls stuff with
        "\"%s\" ALL" -> "COMPRESS"
      | s -> "COMPRESS " ^ s
      end
  | NamedLayout stuff -> tls stuff
and tls =
  function
    fmt, Some tns -> ((string_of_term fmt ^ " (") ^ string_of_term tns) ^ ")"
  | fmt, None -> string_of_term fmt ^ " ALL"
let rec debugstring_of_treelayout =
  function
    HideRootLayout -> "HideRootLayout"
  | HideCutLayout -> "HideCutLayout"
  | CompressedLayout stuff -> "CompressedLayout" ^ stls stuff
  | NamedLayout stuff -> "NamedLayout" ^ stls stuff
and stls stuff =
  string_of_pair debugstring_of_term (string_of_option debugstring_of_term) "," stuff
let rec remaptreelayout a1 a2 =
  match a1, a2 with
    env, HideRootLayout -> HideRootLayout
  | env, HideCutLayout -> HideCutLayout
  | env, CompressedLayout stuff -> CompressedLayout (rmtl env stuff)
  | env, NamedLayout stuff -> NamedLayout (rmtl env stuff)
and rmtl env (fmt, tns) = remapterm env fmt, try__ (remapterm env) tns
