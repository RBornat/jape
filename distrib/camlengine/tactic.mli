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

open Mappingfuns
open Name
open Termtype
open Tactictype

val tacname : term -> name                  (* or raise ParseError_ *)

val transTactic : term -> tactic
val explodeForExecute : term -> name * term list

val string_of_tactic        : tactic -> string (* the simple, unvarnished string *)
val argstring_of_tactic     : tactic -> string (* with brackets if necessary *)
val stringwithNLs_of_tactic : tactic -> string (* guess what this one does *)

val catelim_string_of_tactic : tactic -> string list -> string list
val catelim_stringwithNLs_of_tactic : tactic -> string list -> string list

val remaptactic : (term, term) mapping -> tactic -> tactic
val isguard : tactic -> bool

val showargasint  : (term -> int) option ref
val readintasarg  : term array option ref
val stripextrabag : bool ref

