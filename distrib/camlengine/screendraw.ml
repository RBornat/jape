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

module type T = sig
  open Absprooftree
  open Hit
  open Displayclass
  open Box

  type layout

  val layout : box -> tree -> layout

  val defaultpos : box -> layout -> pos

  val rootpos : box -> layout -> pos

  val postoinclude : box -> box -> layout -> pos

  (* viewport -> proof box -> layout -> pos *)

  val draw : int list option -> pos -> tree -> layout -> unit

  val print : out_channel -> int list option -> pos -> tree -> layout -> unit

  val locateHit :
    pos ->
    displayclass option ->
    hitkind ->
    pos * tree * layout ->
    int list hit option

  val locateElement : element -> pos * tree * layout -> pos list

  val refineSelection : bool

  val notifyselect :
    (pos * displayclass) option ->
    (pos * displayclass) list ->
    pos * tree * layout ->
    unit

  val highlight : pos -> displayclass option -> unit

  val targetbox : pos -> int list option -> layout -> textbox option

  val samelayout : layout * layout -> bool

  val allFormulaHits : pos -> layout -> (textbox * int list fhit) list
end
