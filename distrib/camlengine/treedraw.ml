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
let (|||) = Listfuns.(|||)
let clearView = Draw.clearView
let consolereport = Miscellaneous.consolereport
let elementstring = Term.Termstring.elementstring
let elementstring_invisbracketed = Term.Termstring.elementstring_invisbracketed
let highlight = Draw.highlight
let last = Listfuns.last
let liststring = Listfuns.liststring
let turnstiles = Sequent.Funs.syntacticturnstiles
let uncurry2 = Miscellaneous.uncurry2
let viewBox = Draw.viewBox
       
exception Catastrophe_ = Miscellaneous.Catastrophe_

(* we have a new way of putting trees next to each other, so that they are as 
 * close as may be.  Essentially, we make sure that no formula is too close to the
 * formula to its left or right, even though this may mean that it is overhung by lines
 * above.
 *)

(* this makes it a bit harder to tell whether a click is within a subtree, 
 * because proofboxes can overlap.  Luckily the findfirst coding still works (phew!)
 *)

type outline = (int * int) list
(* L, R boundaries *)
       
type treeplanrec =
      { proofbox : box; proofoutline : outline;
        seqplan : planclass plan list; seqbox : textbox;
        reasonplan : planclass plan option; linebox : (bool * box) option;
        subplans : (pos * treeplan) list; linethickness : int }
and treeplan = Treeplan of treeplanrec

type layout = treeplan

let rec shiftoutline indent (ys : outline) =
  ((fun (l, r) -> l + indent, r + indent) <* ys)

let rec place subh gap =
  fun
    ((Treeplan {proofbox = proofbox; proofoutline = proofoutline} as
        plan), (topbox, alloutline, sps)) ->
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
          let rec _MAX ys zs = (f <* (ys|||zs)) in
          let m2 = _MAX xs (List.tl xs) in
          _MAX (List.hd xs :: m2) (m2 @ [last xs])
      | _ -> xs
    in
    let rs = m3 (uncurry2 max) ((snd <* alloutline)) in
    let ls = m3 (uncurry2 min) ((fst <* proofoutline)) in
    let offset = nj_fold (uncurry2 max) ((fo <* (rs|||ls))) 0 in
    (* doesn't take account of indentation; bound to be 0 if rs is null *)
    let newpos = pos (offset, subh - sH (bSize proofbox)) in
    let newbox = box (newpos, bSize proofbox) in
    (if null alloutline then newbox else ( +||+ ) (topbox, newbox)),
    oz offset alloutline proofoutline, (newpos, plan) :: sps

let rec tpH = fun (Treeplan {proofbox = proofbox}) -> sH (bSize proofbox)
(* from a treeplan, find (x1,x2) of the sequent that underpins it *)

let rec subline =
  fun (Treeplan {seqbox = seqbox}) ->
    let x1 = posX (tbPos seqbox) in x1, x1 + tsW (tbSize seqbox)

let rec drawReason a1 a2 =
  match a1, a2 with
    p, None -> ()
  | p, Some plan -> drawplan planclass2displayclass p plan

let rec maketreeplan proof =
  let termfontleading = thrd (fontinfo TermFont) in
  let reasonfontleading = thrd (fontinfo ReasonFont) in
  let leading = nj_fold (uncurry2 max) [termfontleading; reasonfontleading] 1 in
  let hspace = max (20) (10 * leading)
  (* was 30,15*leading, seemed a bit excessive *)
  and vspace = leading in
  let linethickness = Draw.linethickness leading in
  let _ = setproofparams Japeserver.TreeStyle linethickness in
  (* do this early, so GUIs are ready for anything *)

  let noreasoninf = string2textinfo ReasonFont "" in
  let showturnstiles = List.length (turnstiles ()) <> 1 in
  let sequentplan =
    makeseqplan elementstring_invisbracketed showturnstiles
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
      | Some why -> reason2textinfo why
    in
    let subh = nj_fold (uncurry2 max) ((tpH <* subps)) 0 in
    let (topbox, suboutline, subplans) =
      nj_revfold (place subh hspace) subps (emptybox, [], [])
    in
    let subw = sW (bSize topbox) in
    let subplans = List.rev subplans in
    (* not allowing for subindent below *)
    let reasonw = tsW reasonsize in
    let seqsize = tbSize seqbox in
    let seqw = tsW seqsize in
    let superw = max (reasonw) (seqw) in
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
        let subindent = max (0) (superw - subw) / 2 in
        let subplans =
            ((fun (pos, plan) -> rightby (pos, subindent), plan) <*
             subplans)
        in
        let suboutline = shiftoutline subindent suboutline in
        subindent, subplans, suboutline
      else 0, subplans, suboutline
    in
    (* the minimum line cover is halfway between the reason width and the sequent width *)
    let minlineindent = max (0) ((superw - reasonw) / 3) in
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
            max (0) (sublineleft + (sublinew - superw) / 2)
          in
          superindent,
          min (sublineleft) (superindent + minlineindent),
          max (sublineright) (superindent + superw - minlineindent)
    in
    let reasonindent = superindent + (superw - reasonw) / 2 in
    let seqindent = superindent + (superw - seqw) / 2 in
    let subbox = box (pos (subindent, posY origin), bSize topbox) in
    let superbox =
      box (pos (superindent, supery), size (superw, superh))
    in
    let seqoutline = superindent, superindent + superw in
    Treeplan { proofbox = ( +||+ ) (subbox, superbox); seqplan = seqplan;
               seqbox = textbox (pos (seqindent, supery + superh - tsD seqsize), seqsize);
               proofoutline = (match r with
                                 None -> [seqoutline]
                               | _ -> seqoutline :: (sublineleft, sublineright) :: suboutline);
               reasonplan = (match r with
                               None -> None
                             | Some _ ->
                                 let rplan =
                                   textinfo2plan reasoninf ReasonClass
                                     (pos (reasonindent, supery + tsA reasonsize))
                                 in Some rplan);
               linebox =
                 if reasonw = 0 then None
                 else Some (isMulti, box (pos (sublineleft, supery - vspace - linethickness),
                                          size (sublineright - sublineleft, linethickness)));
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
  match planinfo plan with
    ElementClass info -> Some info
  | _ -> None

let rec pos2hit pos path =
  fun
    (Treeplan
       {proofbox = proofbox;
        seqplan = seqplan;
        seqbox = seqbox;
        reasonplan = reasonplan;
        subplans = subplans}) ->
    if within (pos, proofbox) then
      if withintb (pos, seqbox) then
        match
            (findfirstplanhit ( (pos +<-+ tbPos seqbox)) seqplan &~~
             elinfo)
        with
          Some (el, c) ->
            if c = DisplayHyp then
              Some (FormulaHit (HypHit (List.rev path, el)))
            else if c = DisplayConc then
              Some (FormulaHit (ConcHit (List.rev path, (el, None))))
            else None
        | _ -> None
      else if
        match
          (reasonplan &~~ (fSome <*> plantextbox))
        with
          Some reasonbox -> withintb (pos, reasonbox)
        | _ -> false
      then
        Some (ReasonHit (List.rev path))
      else
        _FIRSTOFN
           (fun (n, (pos', plan)) -> pos2hit ( (pos +<-+ pos')) (n :: path) plan)
           subplans
    else None

let rec locateHit pos _ _ (prooforigin, proof, plan) =
  pos2hit ( (pos +<-+ prooforigin)) [] plan

let refineSelection = false

let rec notifyselect
  bposclassopt posclasslist (prooforigin, proof, plan) =
  let rec cleanup test =
    List.iter
      (fun (oldpos, _) ->
         match pos2hit ( (oldpos +<-+ prooforigin)) [] plan with
           Some oldhit ->
             if test (oldpos, oldhit) then highlight oldpos None
         | None ->
             raise
               (Catastrophe_
                  ["notifyselect (treedraw) can't re-identify ";
                   posstring oldpos]))
      posclasslist
  in
  match bposclassopt with
    None -> cleanup (fun _ -> true)
  | Some (hitpos, _) ->
      (* cancel anything lying around *)
      (* cancel hits in other sequents ... *)
      match pos2hit ( (hitpos +<-+ prooforigin)) [] plan with
        Some (ReasonHit _) ->
          (* only one selection *)
          cleanup (fun (oldpos, _) -> oldpos <> hitpos)
      | Some hit ->
          (* clear selections in other sequents *)                    
          cleanup (fun (oldpos, oldhit) -> hitpath oldhit <> hitpath hit)
      | None ->
          raise
            (Catastrophe_
               ["notifyselect (treedraw) can't identify ";
                posstring hitpos])
(* There is a notion of a 'target' in the proof - the sequent selected for action -
 * which is identified by path, and the tree is always drawn so that the target
 * doesn't move.
 * If no target is selected, the tree is drawn so that the root is at the bottom middle
 * of the current screen.
 * If the new proof is the same as the old, nothing is drawn.
 *)

let rec _FORNUMBERED f xs =
  let rec _F a1 a2 = match a1, a2 with  n, []      -> ()
                     |                  n, x :: xs -> f (n, x); _F (n + 1) xs
  in _F 0 xs    

let rec samepath =
  function None   , _  -> false
  |        Some p1, p2 -> p1 = p2

let rec revgoal =
  function
    None -> None
  | Some path -> Some (List.rev path)

let rec elementofclass class__ plan =
  match planinfo plan with
    ElementClass (_, c) -> class__ = c
  | _ -> false

let rec draw goal pos proof =
  fun (Treeplan {linethickness = linethickness} as plan) ->
    let rgoal = revgoal goal in
    let rec _D =
      fun
        (Treeplan
           {seqplan = seqplan;
            reasonplan = reasonplan;
            linebox = linebox;
            subplans = subplans;
            seqbox = seqbox})
        p here ->
        seqdraw p seqbox seqplan;
        drawReason p reasonplan;
        begin match linebox with
          None -> ()
        | Some (isMulti, b) ->
            if isMulti then
              begin
                drawLine (bOffset b (upby (p, 2 * linethickness)));
                drawLine (bOffset b p)
              end
            else drawLine (bOffset b p)
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
           (fun (n, (stp, st)) -> _D st (( +->+ ) (p, stp)) (n :: here))
           subplans
    in
    drawinproofpane (); _D plan pos []

let rec print str goal pos proof plan =
  let rgoal = revgoal goal in
  let out = output_string str in
  let outesc = out <*> String.escaped in
  let rec outplan p = out "\""; outesc (plan2string p); out "\" " in
  let rec outsp n = if n = 0 then () else (out " "; outsp (n - 1)) in
  let rec _D (Treeplan {seqplan = seqplan; reasonplan = reasonplan; linebox = linebox;
                        subplans = subplans; seqbox = seqbox})
             p here =
      out "(BY ";
      begin match reasonplan with
        Some p -> outplan p
      | None -> out "\"\" "
      end;
      out "(PROVE "; List.iter outplan seqplan; out ") ";
      (*            
      case linebox of None => () | Some b => drawLine (bOffset b p);
      case (samepath(rgoal,here), elementofclass DisplayConc <| seqplan of
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

let rec targetbox path plan =
  let rec _P a1 a2 a3 =
    match a1, a2, a3 with
      [], Treeplan {seqbox = seqbox}, pos -> Some (tbOffset seqbox pos)
    | n :: ns, Treeplan {subplans = subplans}, pos ->
        match subplans with
          [] -> None
        | _ ->
            try
              let (sp, s) = List.nth (subplans) (n) in
              _P ns s (( +->+ ) (pos, sp))
            with
              Failure "nth" -> None
  in
  match path with
    Some route -> _P route plan origin
  | None -> None

let rec defaultpos =
  fun (Treeplan {seqbox=seqbox; linethickness=linethickness}) ->
    let screen = viewBox () in
    let screensize = bSize screen in
    let seqpos = tbPos seqbox in
    let seqsize = tbSize seqbox in
    (* put the base sequent in the middle of the bottom of the screen *)
    (* with enough space below it to allow for the way that the GUI makes selections *)
      (downby
         (rightby (bPos screen, (sW screensize - tsW seqsize) / 2),
          sH screensize - tsD seqsize - 1 - 2*linethickness)
      +<-+ seqpos),
    screen

let layout = maketreeplan

let rec postoinclude box =
  fun (Treeplan {proofbox = proofbox} as layout) ->
    let (defpos, screen) = defaultpos layout in
    (* prefer a centred proof *)
    if entirelywithin (bOffset box defpos, screen) then defpos
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
             (downby (bPos screen, (sH screensize - sH boxsize) / 2),
              (sW screensize - sW boxsize) / 2)
           +<-+ bPos box)
      in
      let screenbottom = posY screenpos + sH screensize in
      let proofbottom = posY proofpos + posY midp + sH proofsize in
      if proofbottom >= screenbottom then midp
      else downby (midp, screenbottom - proofbottom)

let rec samelayout (a, b) = a = b

let defaultpos = fst <*> defaultpos
(* for export *)

let rootpos = defaultpos
