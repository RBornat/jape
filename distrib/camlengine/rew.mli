(*
    $Id$

    Copyright (C) 2003 Richard Bornat & Bernard Sufrin
     
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
open Seqtype
open Rewinf
open Proviso

type rawinf

(* parameters for rew_... are dosubst, cxt, whatsit *)
val rew_Term : bool -> cxt -> term -> term option
val rew_Seq : bool -> cxt -> seq -> seq option
val rew_resnum : cxt -> resnum -> resnum option
val rew_elements : bool -> cxt -> element list -> element list option
val rew_substmap : bool -> cxt -> (term * term) list -> (term * term) list option
(* rew_cxt always does substs, you can't choose *)
val rew_cxt : cxt -> cxt option
(* rew_resnum doesn't deal with substitutions *)

(* and now some useful ones - really special optionfuns *)
val rew_ : ('a -> 'a option) -> 'a -> ('a -> 'b) -> 'b option
val rew_2 :
('a -> 'a option) -> 'a -> ('b -> 'b option) -> 'b -> ('a * 'b -> 'c) ->
'c option
val rew_Pair : ('a -> 'a option) -> 'a * 'a -> ('a * 'a) option
val mkrawinf : term list * vid list * int list * int option -> rawinf
val nullrawinf : rawinf

(* functions for folding *)
val rawinfTerm : cxt -> term * rawinf -> rawinf
val rawinfSeq : cxt -> seq * rawinf -> rawinf
val rawinfElements : cxt -> element list * rawinf -> rawinf
val raw2rew_ : rawinf -> rewinf
val rewinfCxt : cxt -> rewinf
val rew_worthwhile : bool -> cxt -> rewinf -> bool
val rewinfstring : rewinf -> string

val rewritedebug : bool ref
