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

type answer = Yes | Maybe | No

val allq : ('a -> answer) -> 'a list -> answer
val andalsoq : answer -> (unit -> answer) -> answer
val andq : answer * answer -> answer
val existsq : ('a -> answer) -> 'a list -> answer
val ifMq : answer -> (unit -> answer) -> (unit -> answer) -> (unit -> answer) -> answer
val ifq : answer -> answer -> answer -> answer
val notq : answer -> answer
val orelseq : answer -> (unit -> answer) -> answer
val orq : answer * answer -> answer
val qDEF : answer -> bool
val qDEFNOT : answer -> bool
val qUNSURE : answer -> bool
val takeNo : answer -> answer
val takeYes : answer -> answer
val _Maybe_of_unit : unit -> answer
val _No_of_unit : unit -> answer
val _Yes_of_unit : unit -> answer

val string_of_answer : answer -> string
