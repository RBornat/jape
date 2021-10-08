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

(* draw a Gentzen tree *)

open Absprooftree
open Box
open Displayclass
open Displayfont
open Draw
open Hit
open Seqdraw
open Sml

let (&~~) = Optionfuns.(&~~)
let (<*) = Listfuns.(<*)
let (<|) = Listfuns.(<|)
let _Some = Optionfuns._Some
let consolereport = Miscellaneous.consolereport
let foldsequents = Miscellaneous.foldsequents
let highlight = Draw.highlight
let invisbracketedstring_of_element = Termstring.invisbracketedstring_of_element
let last = Listfuns.last
let sameresource = Termfuns.sameresource
let string_of_element = Termstring.string_of_element
let string_of_list = Listfuns.string_of_list
let turnstiles = Sequent.getsyntacticturnstiles
let uncurry2 = Miscellaneous.uncurry2
let bracketed_string_of_list = Listfuns.bracketed_string_of_list
let string_of_pair = Stringfuns.string_of_pair
let string_of_triple = Stringfuns.string_of_triple
let string_of_option = Optionfuns.string_of_option

exception Catastrophe_ = Miscellaneous.Catastrophe_

(* we have a new way of putting trees next to each other, so that they are as 
 * close as may be.  Essentially, we make sure that no formula is too close to the
 * formula to its left or right, even though this may mean that it is overhung by lines
 * above.
 *)

(* this makes it a bit harder to tell whether a click is within a subtree, 
 * because proofboxes can overlap.  Luckily the findfirst coding still works (phew!)
 *)

type outline = (int * int) list (* L, R boundaries *)
       
type treeplanrec =
      { proofbox     : box; 
        proofoutline : outline;
        seqplan      : planclass plan list; 
        seqbox       : textbox; (* now there's a Lehmann-style assumption: single-line sequents! *)
        reasonplan   : planclass plan option; 
        linespec     : (bool * pos * pos) option;
        subplans     : (pos * treeplan) list; 
        linethickness : int }

and treeplan = Treeplan of treeplanrec

let rec string_of_treeplan = function
  | Treeplan r ->
      Printf.sprintf "Treeplan{proofbox=%s; proofoutline=%s; seqplan=%s; seqbox=%s; \
                               reasonplan=%s; linespec=%s; linethickness=%d; \nsubplans=%s\n
                              }"
            (string_of_box r.proofbox)
            (bracketed_string_of_list (string_of_pair string_of_int string_of_int ",") ";" r.proofoutline)
            (bracketed_string_of_list (debugstring_of_plan string_of_planclass) "; " r.seqplan)
            (string_of_textbox r.seqbox)
            (string_of_option (debugstring_of_plan string_of_planclass) r.reasonplan)
            (string_of_option (string_of_triple string_of_bool string_of_pos string_of_pos ",") r.linespec)
            r.linethickness
            (bracketed_string_of_list (string_of_pair string_of_pos string_of_treeplan ": ") ";\n" r.subplans)
            
type layout = treeplan
let string_of_layout = string_of_treeplan

(* let sequentsfolded  = ref false (* result variable, in case anybody cares *) *)

let rec shiftoutline indent (ys : outline) =
  ((fun (l, r) -> l + indent, r + indent) <* ys)

let place subh gap 
          ((Treeplan {proofbox = proofbox; proofoutline = proofoutline} as plan), 
           (topbox, alloutline, sps)) =
    let rec fo (r, l) = r + gap - l in
    let rec oz a1 a2 a3 =
      match a1, a2, a3 with
        n, xs, [] -> xs
      | n, [], ys -> shiftoutline n ys
      | n, (l, r) :: xs, (l', r') :: ys -> (l, r' + n) :: oz n xs ys
    in
    let rec m3 f xs =
      match xs with
        _ :: _ ->
          let rec _MAX ys zs = (f <* Listfuns._BMzip ys zs) in
          let m2 = _MAX xs (List.tl xs) in
          _MAX (List.hd xs :: m2) (m2 @ [last xs])
      | _ -> xs
    in
    let rs = m3 (uncurry2 max) ((snd <* alloutline)) in
    let ls = m3 (uncurry2 min) ((fst <* proofoutline)) in
    let offset = nj_fold (uncurry2 max) ((fo <* Listfuns._BMzip rs ls)) 0 in
    (* doesn't take account of indentation; bound to be 0 if rs is null *)
    let newpos = pos offset (subh - sH (bSize proofbox)) in
    let newbox = box newpos (bSize proofbox) in
    (if null alloutline then newbox else topbox +||+ newbox),
    oz offset alloutline proofoutline, (newpos, plan) :: sps

let rec tpH = fun (Treeplan {proofbox = proofbox}) -> sH (bSize proofbox)

(* from a treeplan, find (x1,x2) of the sequent that underpins it *)
let rec subline =
  fun (Treeplan {seqbox = seqbox}) ->
    let x1 = posX (tbP seqbox) in x1, x1 + tsW (tbS seqbox)

let rec drawReason a1 a2 =
  match a1, a2 with
    p, None -> ()
  | p, Some plan -> drawplan displayclass_of_planclass p plan

let rec maketreeplan viewport proof =
  let termfontleading = thrd (fontinfo TermFont) in
  let reasonfontleading = thrd (fontinfo ReasonFont) in
  let leading = List.fold_left max 1 [termfontleading; reasonfontleading] in
  let hspace = max 20 (10 * leading) (* was 30,15*leading: seemed a bit excessive *)
  and vspace = leading in
  let linethickness = Draw.linethickness leading in
  
  setproofparams Japeserver.TreeStyle linethickness; (* do this early, so GUIs are ready for anything *)

  let noreasoninf = textinfo_of_string ReasonFont "" in
  let showturnstiles = List.length (turnstiles ()) <> 1 in
  let sequentplan =
    makeseqplan (if !foldsequents then leading else 0) 
                (invisbracketedstring_of_element true) showturnstiles
  in
  
  let rec _TP t =
    let s = sequent t in
    let r = reason t in
    let isMulti = ismultistep t in
    (* resolve steps and other multisteps get a double line in the tree *)
    let thickness =
      if isMulti then 3 * linethickness else linethickness
    in
    let subps = List.map _TP (subtrees t) in
    let (seqplan, seqbox) = sequentplan origin s in
    let (reasonsize, _ as reasoninf) =
      match r with
        None -> noreasoninf
      | Some why -> textinfo_of_reason why
    in
    let subh = nj_fold (uncurry2 max) ((tpH <* subps)) 0 in
    let (topbox, suboutline, subplans) =
      nj_revfold (place subh hspace) subps (emptybox, [], [])
    in
    let subw = sW (bSize topbox) in
    let subplans = List.rev subplans in
    (* not allowing for subindent below *)
    let reasonw = tsW reasonsize in
    let seqsize = tbS seqbox in
    let seqw = tsW seqsize in
    let superw = max reasonw seqw in
    let superh =
      tsH seqsize +
        (match r with
           None -> 0
         | _ -> vspace + tsH reasonsize)
    in
    let supery = if subh = 0 then 0 else subh + thickness + 2 * vspace in
    (* we set the subtrees in if they are narrower than the seq/reason *)
    let (subindent, subplans, suboutline) =
      if superw > subw then
        let subindent = max 0 (superw - subw) / 2 in
        let subplans =
            ((fun (pos, plan) -> rightby pos subindent, plan) <* subplans)
        in
        let suboutline = shiftoutline subindent suboutline in
        subindent, subplans, suboutline
      else 0, subplans, suboutline
    in
    (* the minimum line cover is halfway between the reason width and the sequent width *)
    let minlineindent = max 0 ((superw - reasonw) / 3) in
    (* if the topbox is wider than the sequent, we try to centre the sequent 
     * within the line
     *)
    let (superindent, sublineleft, sublineright) =
      match subplans with
        [] -> 0, minlineindent, superw - minlineindent
      | _ :: _ ->
          let spl = List.hd subplans in
          let spr = last subplans in
          let sublineleft =
            posX (fst spl) +
              fst (subline (snd spl))
          in
          let sublineright =
            posX (fst spr) +
              snd (subline (snd spr))
          in
          let sublinew = sublineright - sublineleft in
          let superindent =
            max 0 (sublineleft + (sublinew - superw) / 2)
          in
          superindent,
          min sublineleft (superindent + minlineindent),
          max sublineright (superindent + superw - minlineindent)
    in
    let reasonindent = superindent + (superw - reasonw) / 2 in
    let seqindent = superindent + (superw - seqw) / 2 in
    let subbox = box (pos subindent (posY origin)) (bSize topbox) in
    let superbox =
      box (pos superindent supery) (size superw superh)
    in
    let seqoutline = superindent, superindent + superw in
    Treeplan { proofbox = subbox +||+ superbox; seqplan = seqplan;
               seqbox = textbox (pos seqindent (supery + superh - tsD seqsize)) seqsize;
               proofoutline = (match r with
                                 None -> [seqoutline]
                               | _ -> seqoutline :: (sublineleft, sublineright) :: suboutline);
               reasonplan = (match r with
                               None -> None
                             | Some _ ->
                                 let rplan =
                                   plan_of_textinfo reasoninf ReasonClass
                                     (pos reasonindent (supery + tsA reasonsize))
                                 in Some rplan);
               linespec =
                 if reasonw = 0 then None
                 else Some (isMulti, pos sublineleft (supery - vspace - linethickness),
                                     pos sublineright (supery - vspace - linethickness));
               subplans = subplans; linethickness = linethickness }
  in
  _TP proof

let rec _FIRSTOFN  _P xs =
  let rec _F a1 a2 =
    match a1, a2 with n, []      -> None
    |                 n, x :: xs -> match _P (n, x) with None   -> _F (n + 1) xs
                                    |                    result -> result
  in _F 0 xs

let rec elinfo plan =
  match info_of_plan plan with
    ElementClass info -> Some info
  | _                 -> None

let rec hit_of_pos pos path
                   (Treeplan {proofbox = proofbox; seqplan = seqplan; seqbox = seqbox; 
                              reasonplan = reasonplan; subplans = subplans}) =
  if within pos proofbox then
    if withintb pos seqbox then
      match findfirstplanhit ( (pos +<-+ tbP seqbox)) seqplan &~~ elinfo with
        Some (el, c) ->
          if c = DisplayHyp then
            Some (FormulaHit (HypHit (List.rev path, el)))
          else if c = DisplayConc then
            Some (FormulaHit (ConcHit (List.rev path, (el, None))))
          else None
      | _ -> None
    else if
      match reasonplan &~~ (_Some <.> textbox_of_plan) with
        Some reasonbox -> withintb pos reasonbox
      | _              -> false
    then
      Some (ReasonHit (List.rev path))
    else
      _FIRSTOFN
         (fun (n, (pos', plan)) -> hit_of_pos ( (pos +<-+ pos')) (n :: path) plan)
         subplans
  else None

let locateHit pos _ _ (prooforigin, proof, plan) =
  hit_of_pos ( (pos +<-+ prooforigin)) [] plan

let locateElement locel (prooforigin, proof, plan) = 
  let rec locate p ps = 
    function Treeplan {seqplan = seqplan; seqbox = seqbox; subplans = subplans} ->
      let locsub ps (subpos, subplan) =
        locate (p +->+ subpos) ps subplan
      in
      let locplan ps (Formulaplan (_, textbox, c)) =
        match c with
          ElementClass(el,_) ->
            if sameresource(el,locel) then
              (p +->+ tbP seqbox +->+ tbP textbox) :: ps
            else ps
        | PunctClass  -> ps
        | ReasonClass -> ps
      in
      Listfuns.foldl locplan (Listfuns.foldl locsub ps subplans) seqplan
  in
  locate prooforigin [] plan

let allFormulaHits prooforigin p =
  let rec allroots revpath pos (Treeplan p) rs =
    let path = List.rev revpath in
    let oneel pos rs (Formulaplan (_, tbox, c)) =
      let tbox = tbOffset tbox pos in
      match c with
        ElementClass(el,DisplayHyp)  -> (tbox, HypHit(path,el)) :: rs
      | ElementClass(el,DisplayConc) -> (tbox, ConcHit(path,(el,None))) :: rs
      | _                            -> rs 
    in
    allchilds 0 revpath pos p.subplans 
                (List.fold_left (oneel (pos +->+ tbP p.seqbox)) rs p.seqplan)
  and allchilds n revpath pos ps rs =
    match ps with
      [] -> rs
    | (cpos, p)::ps -> allroots (n::revpath) (pos +->+ cpos) p (allchilds (n+1) revpath pos ps rs)
  in
  allroots [] prooforigin p []

let refineSelection = false

let rec notifyselect bposclassopt posclasslist (prooforigin, proof, plan) =
  let rec cleanup test =
    List.iter
      (fun (oldpos, _) ->
         match hit_of_pos ( (oldpos +<-+ prooforigin)) [] plan with
           Some oldhit ->
             if test (oldpos, oldhit) then highlight oldpos None
         | None ->
             raise (Catastrophe_ ["notifyselect (treedraw) can't re-identify "; string_of_pos oldpos]))
      posclasslist
  in
  match bposclassopt with
    None -> cleanup (fun _ -> false) 
            (* this is what happens when you shift-click to deselect ... i.e. nothing. RB 30/x/02 *)
  | Some (hitpos, _) ->
      (* cancel anything lying around *)
      (* cancel hits in other sequents ... *)
      match hit_of_pos ( (hitpos +<-+ prooforigin)) [] plan with
        Some (ReasonHit _) ->
          (* only one selection *)
          cleanup (fun (oldpos, _) -> oldpos <> hitpos)
      | Some hit ->
          (* clear selections in other sequents *)                    
          cleanup (fun (oldpos, oldhit) -> hitpath oldhit <> hitpath hit)
      | None ->
          raise (Catastrophe_ ["notifyselect (treedraw) can't identify "; string_of_pos hitpos])

(* There is a notion of a 'target' in the proof - the sequent selected for action -
 * which is identified by path, and the tree is always drawn so that the target
 * doesn't move.
 * If no target is selected, the tree is drawn so that the root is at the bottom middle
 * of the current screen.
 * If the new proof is the same as the old, nothing is drawn.
 *)

let rec _FORNUMBERED f xs =
  let rec _F n = function []      -> ()
                 |        x :: xs -> f (n, x); _F (n + 1) xs
  in _F 0 xs    

let rec samepath =
  function None   , _  -> false
  |        Some p1, p2 -> p1 = p2

let rec revgoal =
  function
    None -> None
  | Some path -> Some (List.rev path)

let rec elementofclass class__ plan =
  match info_of_plan plan with
    ElementClass (_, c) -> class__ = c
  | _ -> false

(* aenv is ignored: it's for boxdraw assumptions *)
let rec draw goal pos _ proof (Treeplan {linethickness = linethickness} as plan) =
  let rgoal = revgoal goal in
  let rec _D (Treeplan
                {seqplan = seqplan; reasonplan = reasonplan; linespec = linespec;
                 subplans = subplans; seqbox = seqbox})
             p here =
    seqdraw p seqbox seqplan;
    drawReason p reasonplan;
    begin match linespec with
      None -> ()
    | Some (isMulti, p1, p2) ->
        let p1 = p1 +->+ p in
        let p2 = p2 +->+ p in
        if isMulti then
          begin
            drawLine (upby p1 (2 * linethickness)) (upby p2 (2*linethickness));
            drawLine p1 p2
          end
        else drawLine p1 p2
    end;
    begin match
      samepath (rgoal, here),
      elementofclass DisplayConc <| seqplan
    with
      true, [plan] ->
        highlight (seqelementpos p seqbox plan) (Some DisplayConc)
    | _ -> ()
    end;
    _FORNUMBERED
       (fun (n, (stp, st)) -> _D st (p +->+ stp) (n :: here))
       subplans
  in
  drawinproofpane (); _D plan pos []

let rec print str goal pos proof plan =
  (* let rgoal = revgoal goal in *)
  let out = output_string str in
  let outesc = out <.> String.escaped in
  let rec outplan p = out "\""; outesc (string_of_plan  p); out "\" " in
  let rec outsp n = if n = 0 then () else (out " "; outsp (n - 1)) in
  let rec _D (Treeplan {seqplan = seqplan; reasonplan = reasonplan; linespec = linespec;
                        subplans = subplans; seqbox = seqbox})
             p here =
      out "(BY ";
      begin match reasonplan with
        Some p -> outplan p
      | None -> out "\"\" "
      end;
      out "(PROVE "; List.iter outplan seqplan; out ") ";
      (*            
      case linespec of None => () 
      |                Some (isMulti, p1, p2) ->
            let p1 = bOffset b p1 in
            let p2 = bOffset b p2 in
            if isMulti then
              begin
                drawLine (upby (p1, 2 * linethickness)) (upby (p2, 2*linethickness));
                drawLine p1 p2
              end
            else drawLine p1 p2;
      case (samepath(rgoal,here), elementofclass DisplayConc <| seqplan) of
        (true, [plan]) => 
           highlight (seqelementpos p seqbox plan) DisplayConc
      | _ => ();
      *)
      out "\n"; outsp p; out "(FROM ";
      _FORNUMBERED
         (fun (n, (stp, st)) -> out "\n"; outsp p; _D st (p + 2) (n :: here))
         subplans;
      out "))\n"
  in
  _D plan 0 []

let rec targetbox pos path plan =
  let rec _P a1 a2 a3 =
    match a1, a2, a3 with
      []     , Treeplan {seqbox = seqbox}    , pos -> Some (tbOffset seqbox pos)
    | n :: ns, Treeplan {subplans = subplans}, pos ->
        match subplans with
          [] -> None
        | _ ->
            try
              let (sp, s) = Listfuns.guardednth subplans n in
              _P ns s (pos +->+ sp)
            with
              Listfuns.Bad_nth -> None
  in
  match path with
    Some route -> _P route plan pos
  | None       -> None

let rec defaultpos screen (Treeplan {seqbox=seqbox; linethickness=linethickness}) =
    let screensize = bSize screen in
    let seqpos = tbP seqbox in
    let seqsize = tbS seqbox in
    (* put the base sequent in the middle of the bottom of the screen *)
    (* with enough space below it to allow for the way that the GUI makes selections *)
      (downby
         (rightby (bPos screen) ((sW screensize - tsW seqsize) / 2))
         (sH screensize - tsD seqsize - 1 - 2*linethickness)
      +<-+ seqpos),
    screen

let layout _ = maketreeplan (* aenv is ignored *)

let rec postoinclude viewport box =
  fun (Treeplan {proofbox = proofbox} as layout) ->
    let (defpos, screen) = defaultpos viewport layout in
    (* prefer a centred proof *)
    if entirelywithin (bOffset box defpos) screen then defpos
    else
      (* get it in the middle of the screen, but try to keep the bottom
       * of the proof at the bottom of the screen.  
       * It would be nice not to exceed the other margins of the proof, 
       * but I can't get my head round the calculation.
       *)
      let screenpos = bPos screen in
      let screensize = bSize screen in
      let proofpos = bPos proofbox in
      let proofsize = bSize proofbox in
      let boxsize = bSize box in
      let midp =
          (rightby
             (downby (bPos screen) ((sH screensize - sH boxsize) / 2))
             ((sW screensize - sW boxsize) / 2)
           +<-+ bPos box)
      in
      let screenbottom = posY screenpos + sH screensize in
      let proofbottom = posY proofpos + posY midp + sH proofsize in
      if proofbottom >= screenbottom then midp
      else downby midp (screenbottom - proofbottom)

let rec samelayout (a, b) = a = b

let defaultpos screen = fst <.> defaultpos screen
(* for export *)

let rootpos = defaultpos
