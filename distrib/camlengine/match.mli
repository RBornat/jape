(*
    Copyright (C) 2003-17 Richard Bornat & Bernard Sufrin
     
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
open Termtype

val matchdebug : bool ref

(* first arg is matchbra: whether to match brackets exactly (true) 
   or debracket before matching (false)
 *)
val matchterm :
  bool -> term -> term -> (term, term) mapping list ->
    (term, term) mapping list
val matchtermvars :
  bool -> (term -> bool) -> term -> term -> (term, term) mapping list ->
    (term, term) mapping list
val match__ :
  bool -> term -> term -> (term, term) mapping ->
    (term, term) mapping option
val matchvars :
  bool -> (term -> bool) -> term -> term -> (term, term) mapping ->
    (term, term) mapping option

(* remapping doesn't have that problem, I think ... *)
val remapterm : (term, term) mapping -> term -> term
val option_remapterm : (term, term) mapping -> term -> term option
val simplepat : term -> term

(* straight into the vein *)
type matchresult =
  Certain of (term, term) mapping | Uncertain of (term, term) mapping
val match3term :
  bool -> term -> term -> matchresult list -> matchresult list
val match3termvars :
  bool -> (term -> bool) -> term -> term -> matchresult list ->
    matchresult list
val match3 : bool -> term -> term -> matchresult -> matchresult option
val match3vars :
  bool -> (term -> bool) -> term -> term -> matchresult ->
    matchresult option
