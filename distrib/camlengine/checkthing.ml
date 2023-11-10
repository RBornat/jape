(*
    Copyright (C) 2023 Richard Bornat & Bernard Sufrin
     
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

open Sml
open Miscellaneous
open Listfuns
open Stringfuns
open Name
open Proofstore
open Proviso
open Thing

(* There is a horridness lurking here which has only just been noticed. If adding a thing
   obliterates an existing rule or theorem, then all the stored proofs which use that rule
   or theorem, and all the proofs in progress ditto, have to be invalidated, unless the 
   new rule or theorem is evidently the same. This was first noticed in Runproof.doProof,
   but it applies to addthing entirely. RB 2023/03/11
 *)
(* so now addthing looks at every thing that is added. It's used everywhere there's an
   addthing call, I hope. RB 2023/06/11 
 *)

exception AddThing_

(* filled in by Dialogue *)
let windowsnamed      : (name -> ((name * int) * int) list) ref = ref (fun _ -> [])
let windows_which_use : (name -> ((name * int) * int) list) ref = ref (fun _ -> [])
let addpendingclosures : (int list -> unit)                 ref = ref (fun _ -> ())
let pendingclosures   : int list                            ref = ref []

let check_addthing name new_thing = 
  let obliterate old_thing old_params old_bpros old_givens old_seq old_ax =
    (* we are about to obliterate a Rule or a Theorem. Let's see if that matters *)
    let namestring = string_of_name name in
    let oproof  = is_proofnamed name in
    let ostored = proofs_which_use name in
    consolereport ["** check_addthing\n";
                   "windowsnamed "; namestring; " = "; 
                     bracketed_string_of_list (string_of_pair (string_of_pair string_of_name string_of_int ",")
                                                              string_of_int 
                                                              ","
                                              )
                                              ";" (!windowsnamed name);
                   "\npendingclosures = "; bracketed_string_of_list string_of_int ";" !pendingclosures 
                  ];
    let wproofs = (not <.> (Miscellaneous.swapargs List.mem) !pendingclosures <.> snd) <| !windowsnamed name in
    let _ = consolereport ["wproofs = "; 
                   bracketed_string_of_list (string_of_pair (string_of_pair string_of_name string_of_int ",")
                                                             string_of_int 
                                                             ","
                                              ) ";" wproofs
                  ] in
    let wused   = !windows_which_use name in
    let thingkind givens = if givens=[] then "rule" else "theorem" in
    let thingdiff kind = 
      String.concat "" ["the stored "; string_of_name name; " defines a "; thingkind old_givens;
                        " but the new definition is a "; kind
                       ] 
    in
    let problem_count = (if oproof<>(false,false) then 1 else 0) +
                        List.length ostored +
                        List.length wproofs +
                        List.length wused
    in
    let sentence_string es =
      let es = List.filter (fun e -> e<>[]) es in
      if es=[] then "" 
      else Listfuns.sentencestring_of_list (String.concat "") ", " " and "  es
    in
    if old_ax then
      (if Alert.ask Alert.Warning "You are renaming an axiom!" [("OK",false);("Cancel",true)] 1
       then raise AddThing_
      );
    (* ok if it's the _same_ conjecture *)
    let samething params pros givens seq ax =
      let showpros ps = if null ps then "no provisos"
                        else sentencestring_of_list (string_of_visproviso <.> mkvisproviso) ", " " and " ps
      in
      let storablebps bps = storableprovisos (mkvisproviso <* bps) in
      let verdict =
        sentence_string
          [if eqbags Sequent.eqseqs (givens, old_givens) then [] 
           else
            ["different premises/GIVENs"];
           if params=old_params then [] else ["different parameters"];
           if eqbags (uncurry2 (=)) (List.map snd (storablebps old_bpros), List.map snd pros) then []
                             else ["different provisos (stored version has "; showpros (storablebps old_bpros);
                                                          "; new version has "; showpros pros;
                                                          ")"
                                  ];
           if Sequent.eqseqs (old_seq,seq) then []
           else ["a different conclusion sequent"]
          ]
      in
      if verdict="" then "" else String.concat "" ["Your new definition has "; verdict; " to the stored version."]
    in
    let verdict = match new_thing with
                  | Rule ((params, bpros, givens, seq), ax) -> samething params bpros givens seq ax
                  | Theorem (params, bpros, seq)            -> samething params bpros [] seq false
                  | Tactic _                                -> thingdiff "tactic"
                  | Macro _                                 -> thingdiff "macro"
    in
    if verdict="" then () (* it's the same thing as before, forget it *)
    else 
      (let message_list =
         ["(Congratulations on activating a very obscure Jape error message.)\n
          \n
          In defining "; namestring; " IS "; string_of_thing new_thing; "\n
          you will obliterate an existing "; thingkind old_givens; "\n
          \n";
          verdict; 
          if problem_count=0 then "" else
            ("\n
              \n
              In addition, Jape must " ^ 
                sentence_string [if oproof<>(false,false) then ["delete a stored proof of"; namestring] else [];
                                 (match ostored with
                                  | []  -> []
                                  | [_] -> ["delete a proof which uses"; namestring] 
                                  | _   -> ["delete "; wordstring_of_int (List.length ostored);
                                            " proofs which use "; namestring
                                           ]
                                 );
                                 (match wproofs with
                                  | []  -> []
                                  | [_] -> ["close a window with a proof of "; namestring]
                                  | _   -> ["close "; wordstring_of_int (List.length wproofs);
                                            " windows with proofs of "; namestring
                                           ]
                                 );
                                 (match wused with
                                  | []  -> []
                                  | [_] -> ["close a window with a proof which uses "; namestring]
                                  | _   -> ["close "; wordstring_of_int (List.length wproofs);
                                            " windows with proofs which use "; namestring
                                           ]
                                 )
                                ]  
             )
         ]
       in
       if Alert.askCancel Alert.Error
                          (String.concat "" message_list)
                          [("OK", true)]
                          false
                          1
       then (* do it *)
         (discard_proofs (if oproof<>(false,false) then name::ostored else ostored);
          !addpendingclosures (List.map snd (wproofs @ wused))
         )
       else raise AddThing_ (* don't do it *)
      )
  in
  match freshThingtoprove name with
  | Some (Rule ((old_params, old_bpros, old_givens, old_seq), old_ax) as r) -> 
      obliterate r old_params old_bpros old_givens old_seq old_ax
  | Some (Theorem (old_params, old_bpros, old_seq) as t)   -> 
      obliterate t old_params old_bpros [] old_seq false
  | Some (Tactic _)    
  | Some (Macro _)     
  | None               -> () (* it doesn't matter, it wasn't a rule or a theorem before *)

let addthing (name, thing, place) = check_addthing name thing; addthing (name, thing, place)
