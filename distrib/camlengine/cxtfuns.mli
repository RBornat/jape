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
open Seqtype
open Termtype
open Mappingfuns
open Proviso
open Idclass
                  
val newcxt : cxt
val dont_rewrite_with_this : cxt
(* interrogation functions *)
val varmap : cxt -> (vid, term) mapping
val resmap : cxt -> (int, (resnum * term)) mapping
val provisos : cxt -> visproviso list
val usedVIDs : cxt -> vid list
val nextresnum : cxt -> int
(* assignment functions *)
val withvarmap : cxt -> (vid, term) mapping -> cxt
val withresmap : cxt -> (int, (resnum * term)) mapping -> cxt
val withprovisos : cxt -> visproviso list -> cxt
val withvisibleprovisos : cxt -> proviso list -> cxt
val withusedVIDs : cxt -> vid list -> cxt
val withexterior : cxt -> (seq list * seq) -> cxt
val withresnum : cxt -> int -> cxt
(* augmentation functions *)
val plusvarmap : cxt -> (vid, term) mapping -> cxt
val plusresmap : cxt -> (int, (resnum * term)) mapping -> cxt
val plusprovisos : cxt -> visproviso list -> cxt
val plusvisibleprovisos : cxt -> proviso list -> cxt
val plususedVIDs : cxt -> vid list -> cxt
(* 'side-effecting' functions *)
val freshVID : cxt -> idclass -> vid -> cxt * vid
val freshproofvar : cxt -> idclass -> vid -> cxt * term
val freshresnum : cxt -> cxt * int
(* normalising functions *)
val selfparentprovisos : cxt -> cxt
