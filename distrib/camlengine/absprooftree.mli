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

type tree = Prooftree.Tree.Vistree.prooftree
and structurerule = Thing.structurerule
and font = Displayfont.displayfont
and sequent = Seqtype.seq
and reason
and text = Text.text
and term = Termtype.term
and element = Termtype.element

val allTipConcs : tree -> int list -> (int list * element list) list
val comma : unit -> text
val text_of_element : (element -> string) -> element -> text
val explode : sequent -> string * element list * element list
val isStructureRulenode : tree -> structurerule -> bool
val ishiddencut : tree -> bool
val ismultistep : tree -> bool
val matched : tree -> element list * element list
val reason : tree -> reason option
val fontNstring_of_reason : reason -> font * string
val text_of_reason : reason -> text
val sequent : tree -> sequent
val stillopen : tree -> int list -> bool
val subtrees : tree -> tree list
val text_of_term : (term -> string) -> term -> text
val tip : tree -> int list -> sequent option
val turnstile : string -> text
val validconc : tree -> element -> int list -> bool
val validhyp : tree -> element -> int list -> bool
