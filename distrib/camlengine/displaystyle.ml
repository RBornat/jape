(*
	$Id$

    Copyright (C) 2003 Richard Bornat & Bernard Sufrin
     
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

(* this really is a functor: hence the embedded module type *)

module type Style = sig val style : Displaystate.displaystate end

module F
  (AAA :
    sig
      module Screendraw : Screendraw.T 
      val abstracttree  : Absprooftree.tree -> Absprooftree.tree
    end)
  : Style =
  struct
    open Box 
    open AAA.Screendraw 
    open Displaystate
    open Treeformat.VisFmt
    open Prooftree.Tree.Vistree
        
    let ( &~~ ) = Optionfuns.( &~~ )
    let ( |~~ ) = Optionfuns.( |~~ )
    let abstracttree = AAA.abstracttree
    let consolereport = Miscellaneous.consolereport
    let fmtpath = Prooftree.Tree.viewpathtopath
    let optionstring = Optionfuns.optionstring
    let parentPath = Prooftree.Tree.Fmttree.parentPath
    let posstring = Box.posstring
    let rootPath = Prooftree.Tree.Fmttree.rootPath
    let screenpositiondebug = Miscellaneous.screenpositiondebug
    let textboxstring = Box.textboxstring
    let tranhitpath = Hit.tranhitpath
    let try__ = Optionfuns.try__
    let vispath = Prooftree.Tree.pathtoviewpath
    let visproof = Prooftree.Tree.visproof Proofstore.proved
    let showallproofsteps = Prooftree.Tree.showallproofsteps
    let hideuselesscuts = Prooftree.Tree.hideuselesscuts
    
    exception Catastrophe_ = Miscellaneous.Catastrophe_
    
    let rec deVis = fun (VisPath ns) -> ns

    let ministate =
      try__
        (fun (proof, pos, vgoal, vproof, plan, showall, hideuseless) ->
           pos, abstracttree vproof, plan)
    let rec vpath showall proof popt = popt &~~ vispath showall proof
    let rec tranvpath state vpath =
         state &~~
         (fun (proof, _, _, _, _, showall, _) -> fmtpath showall proof vpath)
    let rec tranfpath state fpath =
         state &~~
         (fun (proof, _, _, _, _, showall, _) -> vispath showall proof fpath)
    let rec ints2fmtpath f state =
      let rec tr ns =
        match tranvpath state (VisPath ns) with
          Some fp -> fp
        | _ ->
            raise
              (Catastrophe_
                 ["ints2fmtpath (displaystyle) can't translate ";
                  pathstring (VisPath ns)])
      in
      f tr
    let rec fmtpath2ints f state =
      let rec tr fp =
        match tranfpath state fp with
          Some (VisPath ns) -> ns
        | _ ->
            raise
              (Catastrophe_
                 ["fmtpath2ints (displaystyle) can't translate ";
                  Prooftree.Tree.Fmttree.pathstring fp])
      in
      f tr
    let rec locateHit state pos classopt hitkind =
      try__ (ints2fmtpath tranhitpath state)
        (ministate state &~~ AAA.Screendraw.locateHit pos classopt hitkind)
    let rec notifyselect state selopt sels =
      match ministate state with
        Some s -> AAA.Screendraw.notifyselect selopt sels s
      | _ -> ()
    let rec saveanddraw proof pos vgoal vproof plan =
      clearView ();
      draw (try__ deVis vgoal) pos (abstracttree vproof) plan;
      screendrawstate
        (Some
           (proof, pos, vgoal, vproof, plan, !showallproofsteps,
            !hideuselesscuts))
    and refresh proof vgoal vproof plan =
      clearView (); saveanddraw proof (defaultpos plan) vgoal vproof plan
    (* these functions - showProof and showFocussedProof - now get the raw proof and the 
     * raw target/goal.  This is because we may have to interpret them relative both to 
     * a new and an old proof, and this may cause horridness (specifically, the proof
     * can jump about the screen). 
     *)
    and showProof oldstate proof target goal =
      (* Draw the proof on the screen.
         If target is None, use defaultpos.
         If there is a saved layout which includes the targeted sequent, use its
         position information to ensure that the targeted sequent doesn't move.
       *)
      (* there are problems with boxdraw - because of cut and hyp it often loses the 
       * target box.  What this function has to do when all is lost is to find the 
       * 'characteristic position' of the proof - in treedraw, the position of the bottom 
       * sequent; in boxdraw, the position of the last line - and draw things so that 
       * the characteristic position doesn't shift.
       *)
      let vproof = visproof !showallproofsteps !hideuselesscuts proof in
      let vgoal = vpath !showallproofsteps proof goal in
      let plan = layout (abstracttree vproof) in
      if !screenpositiondebug then
        consolereport
          ["looking for "; optionstring Prooftree.Tree.Fmttree.pathstring goal;
           " (in visible proof that's "; optionstring pathstring vgoal;
           ")"; " target "; optionstring Prooftree.Tree.Fmttree.pathstring target];
      match oldstate with
        None ->
          if !screenpositiondebug then
            consolereport ["showProof calls refresh, no old state"];
          refresh proof vgoal vproof plan
      | Some
          (oldproof, oldpos, _, oldvproof, oldplan, oldshowall,
           oldhideuseless) ->
          let rec doit oldpoint newpoint =
            if !screenpositiondebug then
              consolereport
                ["gotcha "; posstring oldpoint; "; "; posstring newpoint];
            saveanddraw proof (oldpos +->+ oldpoint +<-+ newpoint) 
                        vgoal vproof plan
          in
          let rec findtarget target =
            if !screenpositiondebug then
              consolereport ["tracking "; optionstring Prooftree.Tree.Fmttree.pathstring target];
            match
              vpath oldshowall oldproof target,
              vpath !showallproofsteps proof target
            with
              (Some _ as oldpath), (Some _ as path) ->
                begin match
                  targetbox (try__ deVis oldpath) oldplan,
                  targetbox (try__ deVis path) plan
                with
                  Some oldbox, Some box -> doit (tbPos oldbox) (tbPos box)
                | _ -> retry target
                end
            | _ ->(* one of the boxes wasn't there *)
               retry target
          and retry =
            function
              Some targetpath ->
                begin match
                  try Some (parentPath proof targetpath) with
                    _ -> None
                with
                  None -> doit (rootpos oldplan) (rootpos plan)
                | newtarget -> findtarget newtarget
                end
            | None -> findtarget (Some (rootPath proof))
          in
          findtarget target
    and printProof str proof target goal =
      let vproof = visproof !showallproofsteps !hideuselesscuts proof in
      let vgoal = vpath !showallproofsteps proof goal in
      let plan = layout (abstracttree vproof) in
      AAA.Screendraw.print str (try__ deVis vgoal) origin (abstracttree vproof)
        plan
    and showFocussedProof proof goal =
      (* for use when we change styles.
         Ensures that the goal sequent is at least on-screen.
       *)
      let vproof = visproof !showallproofsteps !hideuselesscuts proof in
      let vgoal = vpath !showallproofsteps proof goal in
      let plan = layout (abstracttree vproof) in
      match targetbox (try__ deVis vgoal) plan with
        Some box ->
          saveanddraw proof (postoinclude (textbox2box box) plan) vgoal vproof
            plan
      | None -> refresh proof vgoal vproof plan
    and refreshProof state () =
      clearView ();
      match state with
        None -> ()
      | Some (proof, pos, vgoal, vproof, plan, showall, hideuseless) ->
          draw (try__ deVis vgoal) pos (abstracttree vproof) plan
    and storedProof state () =
      try__ (fun (proof, _, _, _, _, _, _) -> proof) state
    and screendrawstate state =
      DisplayState
        {showProof = showProof state; showFocussedProof = showFocussedProof;
         refreshProof = refreshProof state; printProof = printProof;
         locateHit = locateHit state; notifyselect = notifyselect state;
         refineSelection = AAA.Screendraw.refineSelection;
         storedProof = storedProof state}
    (* and for starters *)
    let style = screendrawstate None
  end

module Treestyle : Style = F (struct
								module Screendraw = Treedraw
								let abstracttree t = t
							  end)

module Boxstyle : Style = F (struct
							   module Screendraw = Boxdraw
							   let abstracttree t = t
							 end)
