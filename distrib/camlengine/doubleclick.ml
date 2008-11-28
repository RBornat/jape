(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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

open Sequent
open Listfuns
open Mappingfuns
open Tactic
open Optionfuns

type seq = Seqtype.seq 
 and tactic = Tactictype.tactic 
 and term = Termtype.term

let seqmatch = seqmatch false

type dclick = DClickHyp | DClickConc

type doubleclickdef = dclick * tactic * seq

let doubleclickdefs : doubleclickdef list ref = ref []

let rec adddoubleclick (b, s, seq as p) =
  let rec insert =
    function
      [] -> [p]
    | (b', _, seq' as p') :: doubleclicks ->
        if b = b' && eqseqs (seq, seq') then p :: doubleclicks
        else p' :: insert doubleclicks
  in
  doubleclickdefs := insert !doubleclickdefs

let rec deldoubleclick (b, seq) =
  doubleclickdefs :=
      ((fun (b', _, seq') -> b <> b' || not (eqseqs (seq, seq'))) <|
       !doubleclickdefs)

let rec cleardoubleclicks () = doubleclickdefs := []

let rec matchdoubleclick sense seq =
  let rec match1 (sense', action', seq') =
    if sense = sense' then
      match seqmatch seq' seq empty with
        Some env -> Some (remaptactic env action')
      | None -> None
    else None
  in
  findfirst match1 !doubleclickdefs
