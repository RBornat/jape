(*
	$Id$

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


let deadServer = Interaction.deadServer

and runningServer = (fun() -> Optionfuns.opt2bool !Japeserver.serverpid)
 
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
			  | MyClose
			  | MySave
			  | MySaveAs
			  | MyDisprove


let buttoncache : (mybutton, bool ref) mapping ref = ref empty

let rec enable (button, state) =
  let rec doit b v =
    let (m, c) =
      match b with
        MyUndoProof    -> "Edit", "Undo Proof Step"
      | MyRedoProof    -> "Edit", "Redo Proof Step"
      | MyUndoDisproof -> "Edit", "Undo Disproof Step"
      | MyRedoDisproof -> "Edit", "Redo Disproof Step"
      | MyDone         -> "Edit", "Done"
      | MyClose        -> "File", "Close"
      | MySave         -> "File", "Save Proofs"
      | MySaveAs       -> "File", "Save Proofs As..."
      | MyDisprove     -> "Edit", "Disprove"
    in
    if match (!buttoncache <:> b) with
         Some r -> if !r = state then false else begin r := state; true end
       | None   -> buttoncache := (!buttoncache ++ (b |-> ref state)); true
    then
      Japeserver.enablemenuitem m c state
  in
  match button with
    UndoProofbutton    -> doit MyUndoProof state
  | RedoProofbutton    -> doit MyRedoProof state
  | UndoDisproofbutton -> doit MyUndoDisproof state
  | RedoDisproofbutton -> doit MyRedoDisproof state
  | Finishedbutton     -> doit MyDone state
  | Resetbutton        -> doit MyClose state
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
  if runningServer () then
    try
      (* was freshmenus... *)
      Japeserver.sendOperators oplist;
      buttoncache := empty;
      menuiter
        (fun (proofsonly,menu) ->
           let menustring = namestring menu in
           Japeserver.newmenu proofsonly menustring;
           menuitemiter menu
             (fun (label, keyopt, cmd) ->
                Japeserver.menuentry menustring (namestring label) keyopt cmd)
             (fun (label, cmd) ->
                (* checkbox *)
                Japeserver.menucheckbox menustring (namestring label) cmd)
             (fun lcs ->
                (* radio button *)
                Japeserver.menuradiobutton
                   menustring (List.map (fun (label, cmd) -> namestring label, cmd) lcs))
             (fun _ -> Japeserver.menuseparator menustring));
      paneliter
        (fun (panel, kind) ->
           let panelstring = namestring panel in
           Japeserver.newpanel panelstring kind;
           panelitemiter panel
             (fun (label, entry) ->
                Japeserver.panelentry panelstring (namestring label) entry;
                if kind = ConjecturePanelkind then
                  match markconjecture label with
                    Some mark ->
                      Japeserver.markpanelentry panelstring entry mark
                  | _ -> ())
             (fun (name, cmd) -> (* button *)
                Japeserver.panelbutton panelstring (namestring name) cmd)
             (fun (label, cmd) -> (* checkbox *)
                Japeserver.panelcheckbox panelstring (namestring label) cmd)
             (fun lcs -> (* radio button *)
                Japeserver.panelradiobutton panelstring
                   (List.map (fun (n, v) -> namestring n, v) lcs)));
      Japeserver.mapmenus true;
      let _ = Japeserver.echo "" (* synchronise *) in ()
    with
      Japeserver.DeadServer_ -> deadServer ["WARNING: server broken"]

let rec markproof cmd proved =
  (* given parseable name - look in the cmd part of conjecture panels *)
  paneliter
    (function
       panel, ConjecturePanelkind ->
         panelitemiter panel
           (fun (label, entry) ->
              if entry = cmd then
                Japeserver.markpanelentry (namestring panel) entry proved)
           (fun _ -> ()) (fun _ -> ()) (fun _ -> ())
     | panel, _ -> ())

let rec initButtons () =
  let ( -------- ) = Mseparator in
  let rec _E (name, cut, cmd) = Mentry (namefrom name, cut, cmd) in
  let _EditEntries =
    [( -------- );
     _E ("Undo Proof Step", None, "undo_proof");
     _E ("Redo Proof Step", None, "redo_proof"); 
     ( -------- );
     _E ("Undo Disproof Step", None, "undo_disproof");
     _E ("Redo Disproof Step", None, "redo_disproof"); 
     ( -------- );
     _E ("Backtrack", None, "backtrack");
     _E ("Prune", None, "prune"); 
     ( -------- );
     _E ("Unify selected terms", None, "unify"); 
     ( -------- );
     _E ("Refresh", None, "refreshdisplay"); 
     ( -------- );
     _E ("Hide/Show subproof", None, "collapse");
     _E ("Expand/Contract detail", None, "layout"); 
     ( -------- );
     _E ("Done", Some "D", "done"); ]
  in
  clearmenusandpanels ();
  addmenu true (namefrom "Edit"); (* this isn't necessary ... *)
  addmenudata (namefrom "Edit") _EditEntries

let rec initFonts () =
  match getfontstuff () with
    Some stuff -> Japeserver.setFonts stuff
  | None -> ()













