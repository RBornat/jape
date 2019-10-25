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

open Cxttype
open Termtype
open Proviso
open Mappingfuns
open Seqtype

val checkprovisos : cxt -> cxt option
val deferrable : cxt -> term * term -> bool
val expandFreshProviso :
  bool -> bool * bool * bool * term -> term -> term -> visproviso list -> visproviso list
val groundedprovisos : term list -> visproviso list -> visproviso list option
val relevantprovisos : seq -> proviso list -> proviso list
val remapproviso : (term, term) mapping -> proviso -> proviso
val verifyprovisos : cxt -> cxt

val draganddropmapping: proviso list -> (element list * element list) list

exception Verifyproviso_ of proviso
