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

exception Catastrophe_ = Miscellaneous.Catastrophe_

type displayfont = TermFont | ReasonFont | ProvisoFont

let allfonts = [TermFont; ReasonFont; ProvisoFont]

let rec displayfontstring =
  function TermFont -> "TermFont"
		 | ReasonFont -> "ReasonFont"
		 | ProvisoFont -> "ProvisoFont"

(* Useful translation for Japeserver marshalling.
 * Current C/Java/Tk interfaces believe in these integers.
 *
 *  TermFont = 0
 *  ReasonFont = 1
 *  ProvisoFont = 2
 *
 *)

let rec displayfont2int =
  function
	TermFont -> 0
  | ReasonFont -> 1
  | ProvisoFont -> 2

let rec int2displayfont =
  function
	0 -> TermFont
  | 1 -> ReasonFont
  | 2 -> ProvisoFont
  | n -> raise (Catastrophe_ ["int2displayfont "; string_of_int n])

type pane = ProofPane | DisproofPane

let rec panestring =
  function
	ProofPane -> "ProofPane"
  | DisproofPane -> "DisproofPane"

(* Useful translation for Japeserver marshalling.
 *
 *  ProofPane = 0
 *  DisproofPane = 1
 *
 *)
let rec pane2int =
  function
	ProofPane -> 0
  | DisproofPane -> 1

let rec int2pane =
  function
	0 -> ProofPane
  | 1 -> DisproofPane
  | n -> raise (Catastrophe_ ["int2pane "; string_of_int n])
