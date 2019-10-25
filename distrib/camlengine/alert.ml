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


(* Now ANY alert can be patched. ANY of them, from ANYWHERE. RB 19/x/99 *)

open Japeserver
open Optionfuns
open Sml
open Miscellaneous
open Listfuns

let askChoice = Japeserver.askChoice
let setComment = Japeserver.setComment

type alertspec =
    Alert of (string * (string * alertspec option) list * int)
  | HowToFormulaSelect
  | HowToTextSelect
  | HowToDragFormulae
  | HowToDragDisproofStuff
  
type alertseverity = Info | Warning | Error | Question

let rec defaultseverity bs = if List.length bs <= 1 then Warning else Question

let defaultseverity_alert = Warning

let rec intseverity =
  function
    Info     -> 0
  | Warning  -> 1
  | Error    -> 2
  | Question -> 3
  
let alertpatches : (string * alertspec) list ref = ref []
(* kept in inverse lexical order, so that "abcd" comes before "abc" *)

let patchalertdebug = ref false

let rec alertpatched s =
  if !patchalertdebug then 
    consolereport ["alertpatched checking "; Stringfuns.enQuote s];
  let rec patched (h, a) =
    let r = if String.length h <= String.length s && String.sub s 0 (String.length h) = h 
            then Some a else None
    in
    if !patchalertdebug then
      consolereport ["    checked "; Stringfuns.enQuote h; " => "; string_of_bool (bool_of_opt r)];
    r
  in
  match findfirst patched !alertpatches with
    Some (Alert ("", bs, def)) -> Some (Alert (s, bs, def))
  | aopt                       -> aopt

let rec resetalertpatches () = alertpatches := []

let rec patchalert (h, a) =
  if !patchalertdebug then 
    consolereport ["patchalert patching "; h];
  let rec f =
    function
      (h', a' as p) :: ps ->
        if h > h' then (h, a) :: p :: ps
        else if h = h' then (h, a) :: ps
        else p :: f ps
    | [] -> [h, a]
  in
  alertpatches := f !alertpatches

let rec ask code m (bs : (string * 'a) list) def =
  if null bs then raise (Catastrophe_ ["ask no buttons \""; m; "\""])
  else if def < 0 || def >= List.length bs then
    raise
      (Catastrophe_
         ["ask bad default \""; m; "\"";
          bracketed_string_of_list fst "," bs; " "; string_of_int def])
  else
    let i = ask_unpatched (intseverity code) m (List.map fst bs) def in
    if i<0 then 
			raise (Catastrophe_ ["ask window closed \""; m; "\"";
							              bracketed_string_of_list fst "," bs; " ";
							              string_of_int def; " => "; string_of_int i]) 
		else (try snd (Listfuns.guardednth bs i)
				  with Listfuns.Bad_nth ->
				        raise
				          (Catastrophe_
				             ["ask bad result \""; m; "\"";
				              bracketed_string_of_list fst "," bs; " ";
				              string_of_int def; " => "; string_of_int i]))

let rec askCancel code m (bs : (string * 'a) list) c def =
  if null bs then
    raise (Catastrophe_ ["askCancel no buttons \""; m; "\""])
  else if def < 0 || def > List.length bs then
    raise
      (Catastrophe_
         ["askCancel bad default \""; m; "\"";
          bracketed_string_of_list fst "," bs; " "; string_of_int def])
  else
    match
      askCancel_unpatched (intseverity code) m (List.map fst bs) def
    with
      Some i ->
				if i<0 then c (* Java close window is reported as -1, means Cancel *) 
        else snd (try Listfuns.guardednth bs i with
			             Listfuns.Bad_nth ->
			               raise
			                 (Catastrophe_
			                    ["ask bad result \""; m; "\"";
			                     bracketed_string_of_list fst "," bs; " ";
			                     string_of_int def; " => "; string_of_int i]))
    | None -> c

let rec askDangerously m (dol, doa) (dontl, donta) cancela =
  match askDangerously_unpatched m dol dontl with
    Some 0  -> doa
  | Some 1  -> donta
  | None    -> cancela
	| Some -1 -> cancela (* it can happen if alerts are closed *)
  | Some i ->
      raise (Catastrophe_ ["askDangerously_unpatched => "; string_of_int i])

(* we allow the user to patch an alert *)
let rec showAlert code s =
  let rec display code s = ask code s ["OK", ()] 0 in
  let rec patch code aopt =
    match aopt with
      None                      -> ()
    | Some (Alert (m, bs, def)) ->
        let code' = if List.length bs > 1 then Question else code in
        patch Info (ask code' m bs def)
    | Some HowToTextSelect        -> showHowTo "TextSelect"
    | Some HowToFormulaSelect     -> showHowTo "FormulaSelect"
    | Some HowToDragFormulae      -> showHowTo "DragFormulae"
    | Some HowToDragDisproofStuff -> showHowTo "DragDisproofStuff"
  in
  match alertpatched s with
    None -> display code s
  | aopt -> patch code aopt

and showHowTo s =
  match Japeserver.howToText s with
    "" -> raise (Tacastrophe_ ["Alert.showHowTo doesn't understand"; Stringfuns.enQuote s])
  | s' -> showAlert Info s'