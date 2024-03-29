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

open Box
open Displayclass
open Draw
open Termtype
open Sequent

type planclass = ElementClass of (element * displayclass) 
               | PunctClass
               | ReasonClass

val makeelementplan        : (element -> string) -> displayclass -> element -> pos 
                          -> planclass plan
val makeseqplan            : int -> (element -> string) -> bool -> pos -> seq 
                          -> planclass plan list * textbox
val displayclass_of_planclass : planclass -> displayclass
val string_of_planclass        : planclass -> string
val seqdraw                : pos -> textbox -> planclass plan list -> unit
val seqelementpos          : pos -> textbox -> planclass plan -> pos
