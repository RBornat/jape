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

type seq   = Sequent.seq
and tactic = Tactictype.tactic
and term   = Termtype.term

type dclick = DClickHyp | DClickConc

val adddoubleclick : dclick * tactic * seq -> unit
val deldoubleclick : dclick * seq -> unit
val cleardoubleclicks : unit -> unit
val matchdoubleclick : dclick -> seq -> tactic option

val string_of_dclick : dclick -> string