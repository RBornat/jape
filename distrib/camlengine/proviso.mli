(*
    $Id$

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

open Term.Funs
open Provisotype

type proviso = Provisotype.proviso
 and visproviso

val catelim_provisostring : proviso -> string list -> string list
val earlierproviso : proviso -> proviso -> bool
val isFreshProviso : proviso -> bool
val maxprovisoresnum : proviso -> int
val mkparentedvisproviso : proviso -> bool * proviso -> visproviso
val mkvisproviso : bool * proviso -> visproviso
val parseProvisos : unit -> proviso list
    (* yes, really a proviso list - it has to translate x,y NOTIN A, B into
     * x NOTIN A AND x NOTIN B AND y NOTIN A AND y NOTIN B; similarly
     * FRESH and all its derivatives
     *)
val provisoVIDs : proviso -> vid list
val provisoactual : visproviso -> proviso
val provisodebug : bool ref
val provisoparent : visproviso -> proviso
val provisoresetactual : visproviso -> proviso -> visproviso
val provisoselfparent : visproviso -> visproviso
val provisostring : proviso -> string
val provisovars : (term -> 'a) -> ('a -> 'a -> 'a) -> proviso -> 'a
val provisovisible : visproviso -> bool
val visprovisostring : visproviso -> string
val visprovisostring_invisbracketed : bool -> visproviso -> string
val visprovisostringall : visproviso -> string
