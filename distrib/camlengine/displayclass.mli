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

type displayclass = DisplayHyp
                  | DisplayConc
                  | DisplayAmbig
                  | DisplayReason
                  | DisplayPunct

val string_of_displayclass : displayclass -> string

(* Useful translation for Japeserver marshalling.
 * Current C/Java/Tk interfaces believe in these integers.
 *
 *   DisplayPunct  0
 *   DisplayConc   1
 *   DisplayHyp    2
 *   DisplayReason 3
 *   DisplayAmbig  4
 *
 *)

val int_of_displayclass : displayclass -> int
val displayclass_of_int : int -> displayclass

