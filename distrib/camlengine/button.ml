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

(*      This is essentially the same as the qmw button stuff: evidently
        the two interfaces have converged in this area. 
        
        Differences are:
                
                1. I like putting separators in the Edit menus to separate different
                   kinds of editing action.
                
                2. I don't really care to make use of the variability provided
                   by markpanelentry in choosing the mark to put by a 
                   proven conjecture. (Too right: RB.  Now it is a true/false marker)
        BAS
*)

open Menu 
open Mappingfuns
open Panelkind
open Name
    
type name = Name.name


let reportGUIdead = Interaction.reportGUIdead

type button = UndoProofbutton
            | RedoProofbutton
            | UndoDisproofbutton
            | RedoDisproofbutton
            | Finishedbutton
            | Resetbutton
            | Savebutton
            | SaveAsbutton
            | Disprovebutton

type mybutton = MyUndoProof
              | MyRedoProof
              | MyUndoDisproof
              | MyRedoDisproof
              | MyDone
              | MyReset
              | MySave
              | MySaveAs
              | MyDisprove


let buttoncache : (mybutton, bool ref) mapping ref = ref empty

(* even though there are no longer Undo/Redo Proof/Disproof Step entries in the Edit menu,
   we still send the commands.  The interface interprets the instruction according to the 
   focussed pane, and sets Undo or Redo appropriately.
 *)
let rec enable (button, state) =
  let rec doit b v =
    let (m, c, proofsonly) =
      match b with
        MyUndoProof    -> "Edit", "Undo Proof Step", true
      | MyRedoProof    -> "Edit", "Redo Proof Step", true
      | MyUndoDisproof -> "Edit", "Undo Disproof Step", true
      | MyRedoDisproof -> "Edit", "Redo Disproof Step", true
      | MyDone         -> "Edit", "Done", true
      | MyReset        -> "File", "Erase theory", false
      | MySave         -> "File", "Save Proofs", false
      | MySaveAs       -> "File", "Save Proofs As...", false
      | MyDisprove     -> "Edit", "Disprove", true
    in
    if match (!buttoncache <@> b) with
         Some r -> if !r = state then false else begin r := state; true end
       | None   -> buttoncache := (!buttoncache ++ (b |-> ref state)); true
    then
      Japeserver.enablemenuitem proofsonly m c state
  in
  match button with
    UndoProofbutton    -> doit MyUndoProof state
  | RedoProofbutton    -> doit MyRedoProof state
  | UndoDisproofbutton -> doit MyUndoDisproof state
  | RedoDisproofbutton -> doit MyRedoDisproof state
  | Finishedbutton     -> doit MyDone state
  | Resetbutton        -> doit MyReset state
  | Savebutton         -> doit MySave state
  | SaveAsbutton       -> doit MySaveAs state
  | Disprovebutton     -> doit MyDisprove state

let (resetfontstuff, setfontstuff, getfontstuff) =
  let fontstuff : string option ref = ref None in
  let rec resetfontstuff () = fontstuff := None
  and setfontstuff stuff = fontstuff := Some stuff
  and getfontstuff () = !fontstuff in
  resetfontstuff, setfontstuff, getfontstuff

let rec reloadmenusandpanels markconjecture oplist =
    try
      (* was freshmenus... *)
      Japeserver.sendOperators oplist;
      buttoncache := empty;
      menuiter
        (fun (proofsonly,menu) ->
           let string_of_menu = string_of_name menu in
           Japeserver.newmenu proofsonly string_of_menu;
           menuitemiter menu
             (fun proofsonly label keyopt cmd ->
                Japeserver.menuentry string_of_menu proofsonly (string_of_name label) keyopt cmd)
             (fun proofsonly label cmd ->
                (* checkbox *)
                Japeserver.menucheckbox string_of_menu proofsonly (string_of_name label) cmd)
             (fun proofsonly lcs ->
                (* radio button *)
                Japeserver.menuradiobutton
                   string_of_menu proofsonly (List.map (fun (label, cmd) -> string_of_name label, cmd) lcs))
             (Japeserver.menuseparator string_of_menu));
      paneliter
        (fun (panel, kind) ->
           let string_of_panel = string_of_name panel in
           Japeserver.newpanel string_of_panel kind;
           panelitemiter panel
             (fun (label, entry) ->
                Japeserver.panelentry string_of_panel (string_of_name label) entry;
                if kind = ConjecturePanelkind then
                  match markconjecture label with
                    Some mark ->
                      Japeserver.markpanelentry string_of_panel entry mark
                  | _ -> ())
             (fun (name, cmd) -> (* button *)
                Japeserver.panelbutton string_of_panel (string_of_name name) cmd)
             (* (fun (label, cmd) -> (* checkbox *)
                   Japeserver.panelcheckbox string_of_panel (string_of_name label) cmd)
                (fun lcs -> (* radio button *)
                   Japeserver.panelradiobutton string_of_panel
                      (List.map (fun (n, v) -> string_of_name n, v) lcs))
              *));
      Japeserver.mapmenus true;
      let _ = Japeserver.echo "" (* synchronise *) in ()
    with
      Japeserver.DeadGUI_ -> reportGUIdead ["WARNING: server broken"]

let rec markproof cmd proved =
  (* given parseable name - look in the cmd part of conjecture panels *)
  paneliter
    (function
       panel, ConjecturePanelkind ->
         panelitemiter panel
           (fun (label, entry) ->
              if entry = cmd then
                Japeserver.markpanelentry (string_of_name panel) cmd proved)
           (fun _ -> ()) (* (fun _ -> ()) (fun _ -> ()) *)
     | panel, _ -> ())

let rec initButtons () =
  let ( -------- ) = Mseparator in
  let rec _E (name, cut, cmd) = Mentry (name_of_string name, cut, cmd) in
  let _EditEntries =
    [( -------- );
     _E ("Backtrack", None, "backtrack");
     _E ("Prune", None, "prune"); 
     ( -------- );
     _E ("Unify", None, "unify"); 
     ( -------- );
     _E ("Hide/Show subproof", None, "collapse");
     _E ("Expand/Contract detail", None, "layout"); 
     ( -------- );
     _E ("Done", Some "D", "done"); ]
  in
  clearmenusandpanels ();
  addmenu true (name_of_string "Edit"); (* this isn't necessary ... *)
  addmenudata true (name_of_string "Edit") (List.map (fun e -> MCdata e) _EditEntries)

let rec initFonts () =
  match getfontstuff () with
    Some stuff -> Japeserver.setFonts stuff
  | None       -> ()
