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

open Termtype

val string_of_term                      : term -> string
val invisbracketedstring_of_term        : bool -> term -> string (* first arg sets bracketing *)
val chooseinvisbracketedstring_of_term  : (term -> string) -> (term -> string) -> term -> string

val catelim_string_of_term                     : term -> string list -> string list
val catelim_invisbracketedstring_of_term       : bool -> term -> string list -> string list
val catelim_chooseinvisbracketedstring_of_term : (term -> string) -> (term -> string) -> term
                                              -> string list -> string list

val string_of_termarg         : term -> string
val catelim_string_of_termarg : term -> string list -> string list

val debugstring_of_term         : term -> string
val catelim_debugstring_of_term : term -> string list -> string list

val string_of_vts         : (term * term) list -> string
val catelim_string_of_vts : (term * term) list -> string list -> string list

(* bracketed for use as args in curried functions *)
val string_of_collection               : string -> term -> string
val invisbracketedstring_of_collection : bool -> string -> term -> string

val catelim_string_of_collection               : string -> term -> string list -> string list
val catelim_invisbracketedstring_of_collection : bool -> string -> term -> string list -> string list

(* for those who don't want to see the details *)
val string_of_termOrCollection               : string -> term -> string
val invisbracketedstring_of_termOrCollection : bool -> string -> term -> string

val catelim_string_of_termOrCollection               : string -> term -> string list -> string list
val catelim_invisbracketedstring_of_termOrCollection : bool -> string -> term -> string list -> string list

val string_of_element                     : element -> string
val invisbracketedstring_of_element       : bool -> element -> string
val chooseinvisbracketedstring_of_element : (term -> string) -> (term -> string) -> element -> string

val catelim_string_of_element               : element -> string list -> string list
val catelim_invisbracketedstring_of_element : bool -> element -> string list -> string list

val debugstring_of_element         : (term -> string) -> element -> string
val catelim_debugstring_of_element : (term -> string list -> string list) -> element 
                                  -> string list -> string list

val string_of_resnum         : resnum -> string
val catelim_string_of_resnum : resnum -> string list -> string list

val string_of_termlist         : term list -> string
val catelim_string_of_termlist : term list -> string list -> string list

val isInfixApp: term -> bool

val debracketapplications : bool ref

(* this is internals showing.  Sorry. RB *)
val remake : ((term -> term option) -> term -> 'a)
          -> int option * (term list * term list * term list) *
             (term * (int * int)) list * term -> 'a 
