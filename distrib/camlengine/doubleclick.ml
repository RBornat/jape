(* $Id$ *)

open Sequent.Funs
open Listfuns
open Mappingfuns
open Tactic.Funs
open Optionfuns.M

type seq = Sequent.Funs.seq 
 and tactic = Tactic.Funs.tactic 
 and term = Sequent.Funs.term

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
