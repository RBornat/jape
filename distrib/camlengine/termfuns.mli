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
open Idclass

val isconstant : term -> bool
val isId : term -> bool
val isUnknown : term -> bool
val isVariable : term -> bool
val isleaf : term -> bool
val isidentifier : term -> bool
val ismetav : term -> bool
val isextensibleId : term -> bool
val isemptycollection : term -> bool
val isCollection : term -> bool
val isleafelement : element -> bool
val issegvar : element -> bool
val isselectionSubst : element -> bool
val termkind : term -> int
val termkindmax : int
val emptycollection : idclass -> term
val term_of_collection : term -> term option
val term_of_element : element -> term option
val term_of_int : int -> term
val int_of_term : term -> int
(* may raise AtoI_ or Catastrophe_ *)
   
val mapterm : (term -> term option) -> term -> term
val option_mapterm : (term -> term option) -> term -> term option
(* Some if rewritten *)
val foldterm : (term * 'a -> 'a option) -> 'a -> term -> 'a
val nj_foldterm : (term * 'a -> 'a option) -> term * 'a -> 'a
val foldelements : (term * 'a -> 'a option) -> 'a -> element list -> 'a
val findterm : (term -> 'a option) -> term -> 'a option
val findhole : ((term -> term) -> term -> 'a option) -> term -> 'a option
val searchterm : (term -> 'a option) -> 'a -> term -> 'a
val existsterm : (term -> bool) -> term -> bool
(* functions to give access to the innards of a term without giving away the whole type *)
val decodeSubst : term -> (bool * term * (term * term) list) option
(* gives r, p_, vts *)
val decodeBinding : term -> (term list * term list * term list) option
(* gives bs, ss, us *)
val decodeBracketed : term -> term option
val canonicalsubstmap : (term * term) list -> (term * term) list
val bracketed : term -> bool
val debracket : term -> term
val enbracket : term -> term
val comma_enbracket : term -> term
val eqterms : term * term -> bool
(* ignoring bracketing *)
val eqalphaterms : term * term -> bool
(* ignoring bracketing and alpha-conversion *)
val eqalphadebug : bool ref
val termoccursin : term -> term -> bool
val simterms : term * term -> bool
val termvars : term -> term list
val tmerge : term list -> term list -> term list
val varbindings : term -> (term * term list list list) list
val bmerge :
  (term * term list list list) list -> (term * term list list list) list ->
        (term * term list list list) list
val freevarsfrombindings :
  (term * term list list list) list -> (term * term) list ->
        term list * (term * term list) list
val varbindingsdebug : bool ref
val earliervar : term -> term -> bool
val mergevars : term list -> term list -> term list
val termVIDs : term -> vid list
val vid_of_var : term -> vid
val conVIDs : term list -> vid list
val orderVIDs : vid list -> vid list
val uniqueVID : idclass -> vid list -> vid list -> vid -> vid
val mergeVIDs : vid list -> vid list -> vid list
val idclass : term -> idclass
val isSubstClass : term -> bool
val specialisesto : idclass * idclass -> bool
(* meant to be infix; A specialisesto B if a B-thing is a special kind of A-thing *)
val canoccurfreein : idclass * idclass -> bool
(* temporary additions to ease the passage to Collection use *)
val explodeCollection : term -> element list
val augmentCollection : term -> element list -> term option
(* possibly permanent additions to ease passage to Collection use *)
val elementnumbers : term -> resnum list
val int_of_resnum : resnum -> int
val isProperResnum : resnum -> bool
val elementnumbered : term -> resnum -> term option
val collectionkind : term -> idclass option
val replaceelement : term -> element -> term -> element * term
val eqelements : (term * term -> bool) -> element * element -> bool
(* takes no notice of resource numbers *)
val sameresource : element * element -> bool
(* only looks at resource numbers *)
val earlierresource : element -> element -> bool
(* only looks at resource numbers *)

val explodeApp : bool -> term -> term * term list
val implodeApp : bool -> term * term list -> term
val explodebinapp : term -> (term * string * term) option

(* passed on from Termtype for convenience *)
val string_of_vid : vid -> string
val vid_of_string : string -> vid
