(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
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

(* this is really a functor: hence the embedded module type *)

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
        
    let (&~~) = Optionfuns.(&~~)
    let (|~~) = Optionfuns.(|~~)
    let (<|)  = Listfuns.(<|)
    let abstracttree = AAA.abstracttree
    let bracketed_string_of_list = Listfuns.bracketed_string_of_list
    let consolereport = Miscellaneous.consolereport
    let findfirst = Optionfuns.findfirst
    let fmtpath = Prooftree.Tree.viewpathtopath
    let parentPath = Prooftree.Tree.Fmttree.parentPath
    let string_of_box = Box.string_of_box
    let string_of_fhit = Hit.string_of_fhit
    let string_of_option = Optionfuns.string_of_option
    let string_of_pair = Stringfuns.string_of_pair
    let string_of_pos = Box.string_of_pos
    let rootPath = Prooftree.Tree.Fmttree.rootPath
    let screenpositiondebug = Miscellaneous.screenpositiondebug
    let string_of_textbox = Box.string_of_textbox
    let tranhitpath = Hit.tranhitpath
    let optf = Optionfuns.optf
    let vispath = Prooftree.Tree.pathtoviewpath
    let visproof = Prooftree.Tree.visproof Proofstore.proved
    let showallproofsteps = Prooftree.Tree.showallproofsteps
    let hideuselesscuts = Prooftree.Tree.hideuselesscuts
    let string_of_layout = AAA.Screendraw.string_of_layout
    
    exception Catastrophe_ = Miscellaneous.Catastrophe_
    
    let rec deVis = fun (VisPath ns) -> ns
    
    type staterec = {proof: Prooftree.Tree.Fmttree.prooftree;
                     pos  : Box.pos;
                     vgoal: Prooftree.Tree.Vistree.path option;
                     aenv : (Termtype.resnum, string) Mappingfuns.mapping;
                     vproof: Prooftree.Tree.Vistree.prooftree;
                     plan  : AAA.Screendraw.layout;
                     showall: bool;
                     hideuseless: bool
                    }
    
    type state = staterec option
    
    let ministate =
      optf (fun staterec -> staterec.pos, abstracttree staterec.vproof, staterec.plan)
    
    let rec vpath showall proof popt = popt &~~ vispath showall proof
    
    let rec tranvpath state vpath =
         state &~~ (fun staterec -> fmtpath staterec.showall staterec.proof vpath)
    
    let rec tranfpath state fpath =
         state &~~ (fun staterec -> vispath staterec.showall staterec.proof fpath)
    
    let rec fmtpath_of_ints f state =
      let rec tr ns =
        match tranvpath state (VisPath ns) with
        | Some fp -> fp
        | _       -> raise (Catastrophe_
                              ["fmtpath_of_ints (displaystyle) can't translate ";
                               string_of_path (VisPath ns)])
      in
      f tr
    
    let rec ints_of_fmtpath f state =
      let rec tr fp =
        match tranfpath state fp with
        | Some (VisPath ns) -> ns
        | _ ->
            raise
              (Catastrophe_
                 ["ints_of_fmtpath (displaystyle) can't translate ";
                  Prooftree.Tree.Fmttree.string_of_path fp])
      in
      f tr
    
    let rec locateHit state pos classopt hitkind =
      optf (fmtpath_of_ints tranhitpath state)
           (ministate state &~~ AAA.Screendraw.locateHit pos classopt hitkind)
    
    let rec notifyselect state selopt sels =
      match ministate state with
      | Some s -> AAA.Screendraw.notifyselect selopt sels s
      | _ -> ()
    
    let locateElement state el =
      match ministate state with
      | Some s -> AAA.Screendraw.locateElement el s
      | _      -> []
    
    let rec saveanddraw proof pos vgoal aenv vproof plan =
      if !screenpositiondebug then 
        consolereport ["saveanddraw at "; string_of_pos pos];
      Draw.clearView ();
      draw (optf deVis vgoal) pos aenv (abstracttree vproof) plan;
      screendrawstate
        (Some {proof=proof; pos=pos; vgoal=vgoal; aenv=aenv; vproof=vproof; plan=plan; 
               showall=(!showallproofsteps); hideuseless=(!hideuselesscuts)})
    
    and refresh proof viewport vgoal aenv vproof plan =
      Draw.clearView (); saveanddraw proof (defaultpos viewport plan) vgoal aenv vproof plan
    
    (* these functions - showProof and showFocussedProof - now get the raw proof and the 
     * raw target/goal.  This is because we may have to interpret them relative both to 
     * a new and an old proof, and this may cause horridness (specifically, the proof
     * can jump about the screen). 
     *)
    and showProof oldstate proof viewport target goal =
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
      (* But this isn't enough when refreshing the display if the window size is 
       * stretched -- we need to know what's visible. In general we need that information
       * anyway.
       *)
      let aenv, vproof = visproof !showallproofsteps !hideuselesscuts proof in
      let vgoal = vpath !showallproofsteps proof goal in
      let plan = layout viewport aenv (abstracttree vproof) in
      if !screenpositiondebug then
        consolereport
          ["looking for "; string_of_option Prooftree.Tree.Fmttree.string_of_path goal;
           " (in visible proof that's "; string_of_option string_of_path vgoal;
           "); target="; string_of_option Prooftree.Tree.Fmttree.string_of_path target;
           "; plan="; string_of_layout plan; "; proof="; Prooftree.Tree.Fmttree.string_of_prooftree proof
          ];
      match oldstate, target with
      | None, _
      | _   , None ->
          if !screenpositiondebug then
            consolereport ["showProof calls refresh, no old state / target"];
          refresh proof viewport vgoal aenv vproof plan
      | Some oldrec, _ ->
          if !screenpositiondebug then
            consolereport ["oldpos="; string_of_pos oldrec.pos; "; oldplan="; string_of_layout oldrec.plan;
                          "; oldproof="; Prooftree.Tree.Fmttree.string_of_prooftree oldrec.proof
                          ];
          let rec doit oldpoint newpoint =
            if !screenpositiondebug then
              consolereport
                ["doit "; string_of_pos oldpoint; "; "; string_of_pos newpoint];
            saveanddraw proof (oldrec.pos +->+ oldpoint +<-+ newpoint) 
                        vgoal aenv vproof plan
          in
          (* The objective is to find the target conclusion in both proof plans, and then to use its
             positions in the two plans to make sure that it comes up in the same place on screen
             when the new proof is drawn as it did when the old proof was drawn. The mechanism I
             had (use vpath on the two plans with target) works fine when dealing with a box
             proof with hidden CUTIN steps: it takes the element from the original tree and
             the right side of the cut in the new tree. But this is not what we want when cuts
             are not hidden, as they are not in trees, ever, and sometimes not in box proofs.
             
             And then (I think) there could be a problem with multiple conclusion calculi.
             
             The solution I have adopted is as follows. 
             1. Find, using allFormulaHits, the hits of old and new proofs. Filter for 
                ConcHits and target path. Find if there's a hit which appears in both lists:
                if so, job done. [I now think this is useless and will always fail. Hmm.]
             2. Some trickery with cuts is going on. Find the path to target in both proofs
                and if you can, job done.
             3. If all that fails, try the target's parent.
             
             Eventually we'll reach the root. If that doesn't work, something is very wrong.
             
             RB 10/2021
           *)
          (*
             And now (still 10/21) I have stopped the crashing at the expense of sometimes jumping.
             
             The problem is that looking for the target in the old proof should always work, but 
             it might (especially because of cuts and CUTINs) have vanished in the new proof. This won't
             (I believe) be a problem in tree display, but it happens in box proofs. I try harder to find
             the target (see boxdraw.ml targetbox) but I also have to allow for undos and layout stuff.
             
             So now, if it can't find the target, it searches for the parent. If there is no parent it
             searches for [] (which, with CUTINery, isn't always the root but sometimes it gets you to
             the place where the original root has been displaced to). If that fails it complains in the 
             console log and draws the new proof in the same place as the old. This will cause nasty
             jumping, but I don't know what else to do.
             
             I've left the hit stuff in because it does no harm, especially if it never works.
           *)
          let rec findtarget ftarget =            
            let retry () =
              let badretry () = 
                consolereport ["retry finds infinite loop. target="; string_of_option Prooftree.Tree.Fmttree.string_of_path target;
                               " ftarget="; string_of_option Prooftree.Tree.Fmttree.string_of_path ftarget;
                               " oldplan="; string_of_layout oldrec.plan;
                               " newplan="; string_of_layout plan;
                               " oldproof="; Prooftree.Tree.Fmttree.string_of_prooftree oldrec.proof;
                               " newproof="; Prooftree.Tree.Fmttree.string_of_prooftree proof
                    ];
              in
              match ftarget with
              | None              -> findtarget (Some (FmtPath []))
              | Some (FmtPath []) -> badretry (); doit oldrec.pos oldrec.pos (* failed attempt to find the original root -- no other possibilities *)
              | Some targetpath   -> let newtarget = try Some (parentPath oldrec.proof targetpath) with _ -> Some (FmtPath []) in
                                     findtarget newtarget
            in
            let string_of_hit = string_of_fhit (bracketed_string_of_list string_of_int ";") in
            let string_of_hits = bracketed_string_of_list (string_of_pair string_of_textbox string_of_hit ",") ";" in
            
            if !screenpositiondebug then
              consolereport ["findtarget tracking "; string_of_option Prooftree.Tree.Fmttree.string_of_path ftarget];
            match vpath oldrec.showall oldrec.proof ftarget with 
            | Some (VisPath oldns as oldpath) -> (* found ftarget in old proof *)
                if !screenpositiondebug then
                  consolereport ["found oldpath="; Prooftree.Tree.Vistree.string_of_path oldpath];
                let getHits pos plan =
                  let ts = allFormulaHits pos plan in
                  List.filter (function | (tbox, Hit.ConcHit(ns,_))       -> ns=oldns 
                                        | (tbox, Hit.AmbigHit((ns,_), _)) -> ns=oldns 
                                        | _ -> false
                              ) 
                              ts 
                in
                let oldts = getHits oldrec.pos oldrec.plan in
                let newts = getHits oldrec.pos plan in
                if !screenpositiondebug then
                  consolereport ["oldts="; string_of_hits oldts; " newts="; string_of_hits newts];
                
                (match findfirst (fun (oldbox,oldhit) -> 
                                    findfirst (fun (newbox, newhit) -> if oldhit=newhit then Some (oldbox, newbox) else None)
                                              newts
                                 )
                                 oldts
                 with
                 | Some (oldbox, newbox) -> 
                     if !screenpositiondebug then
                        consolereport [" oldbox="; string_of_textbox oldbox; " newbox="; string_of_textbox newbox];
                     doit (tbP oldbox) (tbP newbox)
                 | None                  ->
                     (match vpath !showallproofsteps proof ftarget with
                      | Some (VisPath newns as newpath) ->
                          if !screenpositiondebug then
                            consolereport ["hits failed; found newpath="; Prooftree.Tree.Vistree.string_of_path newpath];
                          (match targetbox origin (Some oldns) oldrec.plan, targetbox origin (Some newns) plan with
                           | Some oldbox, Some newbox ->
                               if !screenpositiondebug then
                                 consolereport [" oldbox="; string_of_textbox oldbox; " newbox="; string_of_textbox newbox];
                               doit (tbP oldbox) (tbP newbox)
                           | ob, nb ->
                               if !screenpositiondebug then
                                 consolereport ["targetbox in oldplan failed (a case for dropdead?) -- ob="; 
                                                string_of_option string_of_textbox ob;
                                                "; nb="; string_of_option string_of_textbox nb
                                               ];
                               retry()
                          )
                      | _ ->
                          if !screenpositiondebug then
                            consolereport ["vpath in newproof failed (a case for dropdead?)"];
                          retry()
                     )
                )
            | None -> 
                if !screenpositiondebug then
                  consolereport ["couldn't find oldpath -- what?? dropdead??"];
                retry()
          in
          (* body of showProof *)
          findtarget target
    
    and printProof str proof target goal =
      let viewport = Draw.viewBox() in (* daft, but that's what it used to do *)
      let aenv, vproof = visproof !showallproofsteps !hideuselesscuts proof in
      let vgoal = vpath !showallproofsteps proof goal in
      let plan = layout viewport aenv (abstracttree vproof) in
      AAA.Screendraw.print str (optf deVis vgoal) origin (abstracttree vproof)
        plan
    
    and showFocussedProof proof viewport goal =
      (* for use when we change styles.
         Ensures that the goal sequent is at least on-screen.
       *)
      let aenv, vproof = visproof !showallproofsteps !hideuselesscuts proof in
      let vgoal = vpath !showallproofsteps proof goal in
      let plan = layout viewport aenv (abstracttree vproof) in
      match targetbox origin (optf deVis vgoal) plan with (* DANGER: but it used to be origin before *)
        Some box ->
          saveanddraw proof (postoinclude viewport (box_of_textbox box) plan) vgoal aenv vproof
            plan
      | None -> refresh proof viewport vgoal aenv vproof plan
    
    and refreshProof state () =
      Draw.clearView ();
      match state with
      | None          -> ()
      | Some staterec -> draw (optf deVis staterec.vgoal) staterec.pos staterec.aenv (abstracttree staterec.vproof) staterec.plan
    
    and storedProof state () =
      optf (fun staterec -> staterec.proof) state
    
    and screendrawstate state =
      DisplayState
        {showProof = showProof state; showFocussedProof = showFocussedProof;
         refreshProof = refreshProof state; printProof = printProof;
         locateHit = locateHit state; locateElement = locateElement state;
         notifyselect = notifyselect state;
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
