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

open Cxttype

open Listfuns
open Sequent
open Stringfuns
open Optionfuns
open Rewinf
open Termstring
open Mappingfuns
open Sml
open Proviso

type cxt      = Cxttype.cxt
 and exterior = Cxttype.exterior
 and fvinf    = Cxttype.fvinf
 
let string_of_fvinf {avs=avs; fvs=fvs; vmap=vmap; bhfvs=bhfvs; bcfvs=bcfvs} =
  Sml.implode ["{avs="  ; string_of_termlist avs;
               "; fvs=" ; string_of_termlist fvs;
               "; vmap="; string_of_mapping string_of_term string_of_termlist vmap;
               "; bhfvs="; string_of_termlist bhfvs;
               "; bcfvs="; string_of_termlist bcfvs;
               "}"]
               
 
let string_of_exterior =
  function
    NoExterior -> "NoExterior"
  | Exterior e ->
      "Exterior" ^
        string_of_triple
          (string_of_pair (bracketedstring_of_list string_of_seq " AND ") string_of_seq ",")
          (string_of_option string_of_rewinf)
          (string_of_option string_of_fvinf)
          ", " e

let pint = string_of_int

let pid = Termfuns.string_of_vid

let string_of_cxtvarmap = fun (Context {varmap=varmap}) -> string_of_mapping pid string_of_term varmap

let string_of_cxt =
  fun
    (Context
       {varmap = varmap;
        resmap = resmap;
        provisos = ps, inf;
        provisosig = provisosig;
        outside = outside;
        usedVIDs = usedVIDs;
        nextresnum = nextresnum}) ->
    implode
      ["Context{"; "varmap="; string_of_mapping pid string_of_term varmap; ", ";
       "resmap=";
       string_of_mapping pint (string_of_pair string_of_resnum string_of_term ",") resmap;
       ", "; "provisos=(";
       bracketedstring_of_list detailedstring_of_visproviso " AND " ps; ",";
       string_of_option string_of_rewinf inf; "), "; "provisosig=";
       string_of_int provisosig; ", "; "outside="; string_of_exterior outside;
       ", "; "usedVIDs="; bracketedstring_of_list pid "," usedVIDs; ", ";
       "nextresnum="; string_of_int nextresnum; "}"]
