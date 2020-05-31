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

open Symbol
open Termtype

val canstartTerm : symbol -> bool
val canstartidentifier : symbol -> bool
val parseVariable : unit -> term
val parseidentifier : unit -> term
val parseTerm : symbol -> term
val parseBindingpattern : unit -> term
val parseCollection : idclass -> term
val parseElementList :
  (symbol -> bool) -> (symbol -> term) -> symbol -> idclass option ->
    idclass option * element list
val parseList : (symbol -> bool) -> (symbol -> 'a) -> symbol -> 'a list
val parseUnsepList : (symbol -> bool) -> (symbol -> 'a) -> 'a list
val parsecurriedarglist : unit -> term list
val asTactic : ('a -> term) -> 'a -> term
(* parse with no notice of name classification, non-vars allowed in Substs *)
val checkTacticTerm : term -> unit
(* raises Tacastrophe_ (really) if argument isn't a pukka term *)

val term_of_string : string -> term
val tactic_of_string : string -> term
val declareOutRightfix : symbol list -> symbol -> unit
val declareLeftMidfix : symbol list -> unit
val resettermparse : unit -> unit

val pushSyntax     : string -> unit
val popSyntax      : unit -> unit
val popAllSyntaxes : unit -> unit

val termparsedebug : bool ref
