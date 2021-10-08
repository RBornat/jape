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
           ")"; " target="; string_of_option Prooftree.Tree.Fmttree.string_of_path target];
      match oldstate with
      | None ->
          if !screenpositiondebug then
            consolereport ["showProof calls refresh, no old state"];
          refresh proof viewport vgoal aenv vproof plan
      | Some oldrec ->
          let rec doit oldpoint newpoint =
            if !screenpositiondebug then
              consolereport
                ["doit "; string_of_pos oldpoint; "; "; string_of_pos newpoint];
            saveanddraw proof (oldrec.pos +->+ oldpoint +<-+ newpoint) 
                        vgoal aenv vproof plan
          in
          let rec findtarget target =
            if !screenpositiondebug then
              consolereport ["tracking "; 
                string_of_option Prooftree.Tree.Fmttree.string_of_path target];
            match
              vpath oldrec.showall oldrec.proof target,
              vpath !showallproofsteps proof target
            with
            | (Some _ as oldpath), (Some _ as path) ->
                if !screenpositiondebug then
                  consolereport ["found oldpath="; string_of_option Prooftree.Tree.Vistree.string_of_path oldpath;
                    "; path="; string_of_option Prooftree.Tree.Vistree.string_of_path oldpath];
                (match
                   targetbox origin (optf deVis oldpath) oldrec.plan,
                   targetbox origin (optf deVis path) plan
                 with
                 | Some oldbox, Some box -> 
                     if !screenpositiondebug then
                       consolereport ["oldbox="; string_of_textbox oldbox; "; box="; string_of_textbox box; "; viewport="; string_of_box viewport;
                                      "; intersects (box_of_textbox oldbox) viewport = "; string_of_bool (intersects (box_of_textbox oldbox) viewport)
                                     ];
                     if intersects (box_of_textbox oldbox) viewport (* it was visible *)
                     then doit (tbP oldbox) (tbP box)
                     else handleinvis ()
                 | _                     -> retry target)
            | _ ->  (* one of the boxes wasn't there *)
               retry target
          and retry =
            function
            | Some targetpath ->
                begin match
                  try Some (parentPath proof targetpath) with
                  | _ -> None
                with
                | None      -> doit (rootpos viewport oldrec.plan) (rootpos viewport plan)
                | newtarget -> findtarget newtarget
                end
            | None -> default()
          and default () = findtarget (Some (rootPath proof))
          and handleinvis () =
            let string_of_path = bracketed_string_of_list string_of_int ";" in
            let string_of_hit = string_of_fhit string_of_path in
            let oldts = allFormulaHits oldrec.pos oldrec.plan in
            if !screenpositiondebug then 
              consolereport ["Displaystyle.showProof.handleinvis oldpos="; string_of_pos oldrec.pos; "; oldts=";
                       bracketed_string_of_list (string_of_pair string_of_textbox string_of_hit ",") ";" oldts;
                       "; viewport="; string_of_box viewport]; 
            let newts = allFormulaHits oldrec.pos plan in
            let acceptable f ts = (fun (tbox, _) -> f (box_of_textbox tbox) viewport) <| ts in
            if !screenpositiondebug then
              consolereport ["Displaystyle.showProof.acceptable intersects oldts="; 
                   bracketed_string_of_list (string_of_pair string_of_textbox string_of_hit ",") ";" 
                                           (acceptable intersects oldts)]; 
                                           
            let bad s =
               consolereport ["We have a problem: Displaystyle.showProof.handleinvis can't find a visible path ("; s; ")"];
               consolereport ["oldpos="; string_of_pos oldrec.pos; "; oldts=";
                       bracketed_string_of_list (string_of_pair string_of_textbox string_of_hit ",") ";" oldts;
                       "; viewport="; string_of_box viewport]; 
               Japeserver.dropdead(); (* doesn't return *)
               default() (* would cause an infinite recursion, if it happened *) 
            in
            let process ts = (* bloody hell, N^2. Oh well. *)
              if !screenpositiondebug then 
                consolereport ["Displaystyle.showProof.process ";
                       bracketed_string_of_list (string_of_pair string_of_textbox string_of_hit ",") ";" ts;
                       "; newts = ";
                       bracketed_string_of_list (string_of_pair string_of_textbox string_of_hit ",") ";" newts]; 
              findfirst 
                (fun (oldbox, hit) -> 
                  findfirst (fun (box, hit') -> if hit=hit' then Some (oldbox, box) else None)
                  newts)
                ts
            in
            match acceptable intersects oldts with
            | [] -> bad "[]"
            | grazers -> 
                match (match acceptable entirelywithin grazers with
                       | []     -> process grazers
                       | inners -> process inners |~~ (fun _ -> process grazers))
                with
                | None              -> bad "None"
                | Some(oldbox, box) -> doit (tbP oldbox) (tbP box)
          in
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
