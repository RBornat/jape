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

open Seqtype 
open Rewinf
open Termtype
open Mappingfuns
open Proviso

(* this information perhaps ought to be elsewhere in the proof state, but it's here
 * now, so what the hell.
 *
 * It records the 'exteriority' of a proof: information about the base judgment and 
 * the given judgements, which are where the proof touches the outside world.
 *
 * Exterior(ss*s,inf option,fvinf option):
 *   ss are the givens, s is the base sequent of the proof;
 *   inf is the rewinf of that collection of sequents;
 *   fvinf is information about the ways that names relate to each other, used
 *     to decide 'what dominates what', and some other stuff.  It consists of
 *       avs  : all variables in the exterior
 *       fvs  : all free variables in the exterior
 *       vmap : the 'what dominates what' mapping
 *       bhfvs: all free variables of base hypotheses
 *       bcfvs: all free variables of base conclusions
 *
 *       (I promise, one day, to write down what vmap is and how it's used.
 *        Until that day, look in facts.sml, where it's used.
 *       )
 *       
 *)

type fvinf = {avs:  term list;
              fvs:  term list;
              vmap: (term, term list) mapping;
              bhfvs: term list;
              bcfvs: term list}

type exterior =
    NoExterior
  | Exterior of ((seq list * seq) * rewinf option * fvinf option)

type cxtrec = 
      { varmap : (vid, term) mapping;
        resmap : (int, (resnum * term)) mapping;
        provisos : visproviso list * rewinf option; provisosig : int;
        outside : exterior; usedVIDs : vid list; nextresnum : int }

type cxt = Context of cxtrec
