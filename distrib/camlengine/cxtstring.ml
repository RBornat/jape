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
 
let fvinfstring {avs=avs; fvs=fvs; vmap=vmap; bhfvs=bhfvs; bcfvs=bcfvs} =
  Sml.implode ["{avs="  ; termliststring avs;
               "; fvs=" ; termliststring fvs;
               "; vmap="; mappingstring termstring termliststring vmap;
               "; bhfvs="; termliststring bhfvs;
               "; bcfvs="; termliststring bcfvs;
               "}"]
               
 
let exteriorstring =
  function
    NoExterior -> "NoExterior"
  | Exterior e ->
      "Exterior" ^
        triplestring
          (pairstring (bracketedliststring seqstring " AND ") seqstring ",")
          (optionstring rewinfstring)
          (optionstring fvinfstring)
          ", " e

let pint = string_of_int

let pid = Termfuns.string_of_vid

let cxtstring =
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
      ["Context{"; "varmap="; mappingstring pid termstring varmap; ", ";
       "resmap=";
       mappingstring pint (pairstring resnumstring termstring ",") resmap;
       ", "; "provisos=(";
       bracketedliststring visprovisostringall " AND " ps; ",";
       optionstring rewinfstring inf; "), "; "provisosig=";
       string_of_int provisosig; ", "; "outside="; exteriorstring outside;
       ", "; "usedVIDs="; bracketedliststring pid "," usedVIDs; ", ";
       "nextresnum="; string_of_int nextresnum; "}"]
