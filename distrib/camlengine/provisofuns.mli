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

type prooftree = Prooftree.Tree.Fmttree.prooftree

val deferrable : cxt -> term * term -> bool
val expandFreshProviso :
  bool -> bool * bool * bool * term -> term -> term -> visproviso list -> visproviso list

exception Verifyproviso of proviso
val verifycxtprovisos : cxt -> cxt                  (* raises VerifyProviso *)
val verifytreeprovisos : prooftree -> cxt -> cxt    (* raises VerifyProviso *)
val checkcxtprovisos : cxt -> cxt option 
val checkprovisos : prooftree -> cxt -> cxt option 

val draganddropmapping: proviso list -> (element list * element list) list
