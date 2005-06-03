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

open Termtype
open Mappingfuns

(* everything you can say in a LAYOUT tactic *)
type treelayout =
    HideRootLayout
  | HideCutLayout
  | CompressedLayout of (term * term option)
  | NamedLayout      of (term * term option)
                      (* fmt    list of subtrees to show *)

val string_of_treelayout : treelayout -> string
val debugstring_of_treelayout : treelayout -> string
val remaptreelayout : (term, term) mapping -> treelayout -> treelayout
