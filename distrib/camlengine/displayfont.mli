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

type displayfont = TermFont | ReasonFont | ProvisoFont
type pane = ProofPane | DisproofPane

val allfonts : displayfont list
val displayfontstring : displayfont -> string
val displayfont2int : displayfont -> int
val int2displayfont : int -> displayfont
val panestring : pane -> string

(* Useful translation for Japeserver marshalling.
 *
 *  ProofPane = 0
 *  DisproofPane = 1
 *
 *)
val pane2int : pane -> int
val int2pane : int -> pane
