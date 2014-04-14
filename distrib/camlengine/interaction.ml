(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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

open Answer
open Box
open Displayclass
open Displayfont
open Displaystate
open Hit
open Invisibles
open Proofstate
open Prooftree.Tree.Fmttree
open Sequent
open Seqtype
open Sml
open Stringfuns
open Treeformat.Fmt
open UTF

let (<|) = Listfuns.(<|)
let (<*) = Listfuns.(<*)
let (&~~) = Optionfuns.(&~~)
let (|~~) = Optionfuns.(|~~)
let _The = Optionfuns._The
let atoi = Miscellaneous.atoi
let bracketedstring_of_list = Listfuns.bracketedstring_of_list
let consolereport = Miscellaneous.consolereport
let dont_rewrite_with_this = Cxtfuns.dont_rewrite_with_this
let string_of_element = Termstring.string_of_element
let findfirst = Optionfuns.findfirst
let interpolate = Listfuns.interpolate
let lowercase = Stringfuns.lowercase
let member = Listfuns.member
let numbered = Listfuns.numbered
let optionfilter = Optionfuns.optionfilter
let string_of_option = Optionfuns.string_of_option
let provisos = Cxtfuns.provisos
let replaceelement = Termfuns.replaceelement
let rewritecxt = Rewrite.rewritecxt
let seektipselection = Miscellaneous.seektipselection
let selectiondebug = Miscellaneous.selectiondebug
let _Subst_of_selection = Selection._Subst_of_selection
let setComment = Alert.setComment
let showAlert = Alert.showAlert Alert.defaultseverity_alert
let debugstring_of_element = Termstring.debugstring_of_element
let sort = Listfuns.sort
let take = Listfuns.take
let string_of_term = Termstring.string_of_term

exception Catastrophe_ = Miscellaneous.Catastrophe_
exception Selection_ = Selection.Selection_
exception None_ = Optionfuns.None_
exception DeadGUI_ = Japeserver.DeadGUI_

type command =
    TextCommand of string list
  | HitCommand of (prooftree * path hit * path sel)

let rec string_of_command =
  function
    TextCommand ws -> "TextCommand" ^ bracketedstring_of_list enQuote ", " ws
  | HitCommand hc ->
      "HitCommand" ^
        string_of_triple (fun _ -> "....") (string_of_hit string_of_path)
          (string_of_sel string_of_path) "," hc

let string_of_intlist = bracketedstring_of_list (string_of_int : int -> string) ","

let setComment = setComment <.> implode

let terminateGUI = Japeserver.terminateGUI

let rec reportGUIdead strings = consolereport strings; raise DeadGUI_

let treestyle = Displaystyle.Treestyle.style

let boxstyle = Displaystyle.Boxstyle.style

let currentstyle = ref treestyle

let currentstylename = ref "tree"

let rec proofStyle s =
  match lowercase s with
    "tree" -> treestyle
  | "box" -> boxstyle
  | _ -> treestyle

let rec setdisplaystyle s =
  if !currentstylename <> s then
    begin currentstyle := proofStyle s; currentstylename := s end

let rec getdisplaystyle () = !currentstylename

let showProof (DisplayState d) target goal cxt tree withgoal =
    d.showProof tree (Draw.viewBox()) target (if withgoal then goal else None)

let rec showFocussedProof goal cxt tree withgoal =
  match !currentstyle with
    DisplayState {showFocussedProof = sfp} ->
      sfp tree (Draw.viewBox()) (if withgoal then goal else None)

let refreshProof (DisplayState d) = d.refreshProof ()

let locateHit (DisplayState {locateHit = lh}) p class__ kind = (lh p class__ kind : path hit option)

let locateElement (DisplayState{locateElement=le}) = le

let rec notifyselect (DisplayState {notifyselect = nsel}) bpcopt sels = (nsel bpcopt sels : unit)

let rec storedProof = fun (DisplayState {storedProof = sp}) -> sp ()

let rec refineSelection = fun (DisplayState {refineSelection = rS}) -> rS

let rec printProof outstream target goal cxt tree withgoal =
  match !currentstyle with
    DisplayState {printProof = pp} ->
      pp outstream tree target (if withgoal then goal else None)
(* one way of telling that I have the interface and datatypes wrong is all these blasted Catastrophe_ exceptions ... *)

let rec sortoutSelection state pathkind =
  let (proofsels, prooftextsels, givensel) = Japeserver.getAllProofSelections () in
  (* remove invisbra/kets from any text selections we see *)
  let rec deinvis s =
    utf8_implode ((fun c -> not (isInvisibleUcode c)) <| utf8_explode s)
  in
  let prooftextsels = List.map (fun (p, ss) -> p, List.map deinvis ss) prooftextsels in
  let givensel = List.map deinvis givensel in
  let rec hit_of_pos pos copt pathkind =
    match locateHit state pos copt pathkind with
      Some h -> h
    | None   ->
        raise (Catastrophe_
                 ["sortoutSelection (interaction) can't locate ";
                  string_of_pos pos; ", "; string_of_option string_of_displayclass copt])
  in
  let rec fhit_of_hit a1 a2 =
    match a1, a2 with
      s, FormulaHit fh -> fh
    | s, h ->
        raise
          (Catastrophe_
             ["sortoutSelection (interaction) sees "; s; " hit ";
              string_of_hit string_of_path h])
  in
  let fhits =
    List.map (fun (pos, class__) -> pos, hit_of_pos pos (Some class__) pathkind)
             proofsels
  in
  let thits =
    List.map
      (fun (pos, strings) ->
         (match
            findfirst
              (fun (pos', hit) -> if pos = pos' then Some hit else None)
              fhits
          with
            Some h -> fhit_of_hit "textsel" h
          | None -> fhit_of_hit "textsel" (hit_of_pos pos None pathkind)),
         strings)
      prooftextsels
  in
  List.map snd fhits, thits, givensel

let findDisproofSelections () = Japeserver.getAllDisproofSelections () 
  (* at present, we don't touch 'em *)

let rec findSelection state =
  let (fhits, thits, givensel) = sortoutSelection state HitPath in
  (* only path that makes sense for what we are trying to do ... *)
  if !selectiondebug then (
     let showstrings = bracketedstring_of_list enQuote "," in
     consolereport ["findSelection sees "; 
        bracketedstring_of_list (string_of_hit string_of_path) "," fhits; "; ";
        bracketedstring_of_list 
                (string_of_pair (string_of_fhit string_of_path) showstrings ",") "," thits; "; ";
        showstrings givensel]
  );
  let (conchits, hyphits, reasonhits) =
    nj_fold
      (function
         FormulaHit (ConcHit c), (cs, hs, rs) -> c :: cs, hs, rs
       | FormulaHit (HypHit  h), (cs, hs, rs) -> cs, h :: hs, rs
       | ReasonHit           r , (cs, hs, rs) -> cs, hs, r :: rs
       | h, _ -> raise (Catastrophe_
                          ["findSelection (interaction) sees hit "; string_of_hit string_of_path h]))
      fhits ([], [], [])
  in
  (* it gets too hard not to trust the interface here ... 
   * I'm just going to take the hyp interpretation of each ambiguous text selection.
   *)
  let (tcs, ths) =
    nj_fold
      (function
         (AmbigHit (_, (p, h)), ss), (tcs, ths) -> tcs, (p, h, ss) :: ths
       | (ConcHit (p, c), ss), (tcs, ths) -> (p, c, ss) :: tcs, ths
       | (HypHit (p, h), ss), (tcs, ths) -> tcs, (p, h, ss) :: ths)
      thits ([], [])
  in
  let tree =
    match storedProof state with
      Some p -> p
    | None ->
        raise
          (Catastrophe_ ["findSelection (interaction) - no stored proof"])
  in
  match conchits, hyphits, reasonhits with
    [cpath, conc], _, [] ->
      (* we have a definite conclusion path, which we are going to _trust_, and hang the consequences *)
      let hyps =
        List.map
          (fun (hpath, hypel) ->
             if Prooftree.Tree.Fmttree.validhyp tree hypel cpath then hypel
             else raise (Catastrophe_ ["incompatible double hit in findSelection (interaction): ";
                                       string_of_path hpath; " ";
                                       debugstring_of_element string_of_term hypel; "; ";
                                       string_of_path cpath; " ";
                                       string_of_pair (debugstring_of_element string_of_term)
                                         (string_of_option string_of_side) "," conc]))
          hyphits
      in
      Some (FormulaSel (cpath, Some conc, hyps, tcs, ths, givensel))
  | [], (hpath, _) :: hs, [] ->
      (* we don't have a conclusion path -- we take the highest path from all those offered.
       * If a hypothesis isn't valid in the supremum of all the paths looked at so 
       * far, take its path as the supremum.  This works for treedraw (which will give us identical paths for
       * all hyps) and for boxdraw (which credits each hypothesis with the path to its first occurrence).
       *)
      let path =
        nj_fold
          (fun ((hpath, hypel), path) ->
             if Prooftree.Tree.Fmttree.validhyp tree hypel path then path
             else hpath)
          hs hpath
      in
      let hypels = List.map snd hyphits in
      let rec ok path =
        not
          (List.exists
             (not  <.> (fun el -> Prooftree.Tree.Fmttree.validhyp tree el path))
             hypels)
      in
      (* now, because we don't have a definite conclusion path, we have to see if we can refine the path we 
       * have by (a) looking for a unique conclusion node above the position we have, or (b) using
       * text selections to discriminate.  For belt and braces, I begin by checking the path we have so far ...
       *)
      let path =
        if not (ok path) then
          raise
            (Catastrophe_
               ["invalid hypothesis selections in findSelection (interaction): ";
                bracketedstring_of_list
                  (string_of_pair string_of_path (debugstring_of_element string_of_term)
                     ",")
                  "," hyphits])
        else if !seektipselection && refineSelection state then
          (* is there a single conclusion above us, and are all our hypotheses still valid at that point? *)
          match
            Prooftree.Tree.Fmttree.allTipPaths (Prooftree.Tree.Fmttree.followPath tree path)
          with
            [cpath] -> if ok cpath then cpath else path
          | [] -> path
          | paths ->
              (* is this a BadSel_ candidate? *)
              (* we try to disambiguate these paths by looking at the text selections ... *)
              match
                findfirst
                  (function
                     ConcHit (cpath, _), _ ->
                       if member (cpath, paths) then Some path else None
                   | _ -> None)
                  thits
              with
                Some cpath -> cpath
              | None -> path
        else path
      in
      Some (FormulaSel (path, None, hypels, tcs, ths, givensel))
  | [], [], [rpath] -> Some (ReasonSel rpath)
  | [], [], [] ->
      begin match thits, givensel with
        [], [] -> None
      | _      -> Some (TextSel (thits, givensel))
      end
  | _ ->
      raise
        (Catastrophe_
           ["findSelection (interaction) sees too many hits: ";
            bracketedstring_of_list (string_of_hit string_of_path) "," fhits])

(* when looking for LayoutPath and PrunePath, findSelection is just too fussy.  E.g. if you Prune a 
 * hypothesis selection in boxdraw, the interface changes it to a conclusion selection, and that 
 * confuses findSelection no end.  Perhaps this will improve matters ...
 *)

let rec findLayoutSelection state pathkind =
  let (fhits, _, _) = sortoutSelection state pathkind in
  let rec getpath a1 a2 =
    match a1, a2 with
      popt, [] -> popt
    | None, h :: hs -> getpath (hitpath h) hs
    | Some p, h :: hs ->
        match hitpath h with
          Some p' -> if p = p' then getpath (Some p) hs else None
        | None ->
            raise
              (Catastrophe_
                 ["findLayoutSelection (interaction) sees ";
                  string_of_hit string_of_path h])
  in
  getpath None fhits

let els_of_formulahit where h =
  match h with
    Some (FormulaHit fh) ->
      (match fh with
         ConcHit (_, (c, _)) -> c
       | HypHit  (_, h)      -> h
       | AmbigHit _          -> 
           raise (Catastrophe_ ["els_of_formulahit can't handle AmbigHit (only use in tree mode)"]))
  | _ ->
      raise (Catastrophe_ ["els_of_formulahit (in "; where; ") can't handle ";
                           string_of_option (string_of_hit string_of_path) h])

let dropsource : element option ref = ref None
let droptarget : element option ref = ref None

(*
fun gooddrag d = (* you can't drag from or to one side of a formula *)
  let fun good (_, (_, None))   = true
      |   good (_, (_, Some _)) = false
  in
      case d of
        ConcHit s       => good s
      | HypHit  s       => true
      | AmbigHit(up,dn) => good up
  end
*)
  

let rec getCommand displayopt =
  let text = Japeserver.listen () in
  let rec getdisplay () =
    try _The displayopt with
      None_ ->
        raise
          (Catastrophe_
             ["no display in getCommand (interaction): "; text])
  in
  let rec mkpos x y =
    try pos (atoi x) (atoi y) with
      _ ->
        raise
          (Catastrophe_ ["bad pos in getCommand (interaction): "; text])
  in
  let rec mkclass c =
    try displayclass_of_int (atoi c) with
      _ -> raise (Catastrophe_ ["bad class in getCommand (interaction): "; text])
  in
  let rec parseselections =
    function
      x :: y :: c :: others ->
        (mkpos x y, mkclass c) :: parseselections others
    | [] -> []
    | _ -> raise (Catastrophe_ ["bad selection tail in getCommand (interaction): "; text])
  in
  match words text with
    "ACT" :: x :: y :: c :: _ ->
      begin match
        locateHit (getdisplay ()) (mkpos x y) (Some (mkclass c)) HitPath
      with
        Some h ->
          begin match findSelection (getdisplay ()) with
            Some s ->
              HitCommand (_The (storedProof (getdisplay ())), h, s)
          | None ->
              (* raise
                (Catastrophe_
                   ["getCommand (interaction) sees hit but not selection: ";
                    text]) *)
                showAlert("You double-clicked on a greyed out formula\n(this has no effect)");
                getCommand displayopt             
          end
      | None ->
          raise
            (Catastrophe_
               ["getCommand (interaction) can't decode hit: "; text])
      end
  | "SELECT" :: x :: y :: c :: others ->
      (* the SELECT/DESELECT mechanism used to be designed for incremental change, presumably in a desire to 
       * reduce interface traffic and computations with the proof in greyen/blacken decisions.  But actually
       * the computations are easier if we work with the current selection, and I don't think that greyen/blacken
       * takes much time in the GUI.  Also it gets difficult to try to handle the history when there is a 
       * large change of selection.
       * SELECT pos, class, (pos', class')* now means that the pos, class selection is new, and that it plus
       * the others make up the current selection.
       * Reason and formula selection are mutually exclusive.  The GUI might well do this for us, but I check it
       * here (belt and braces).
       * RB 30/viii/00
       *)
      (* Now I'm sure that the GUI gets it right :-), I just take what I'm given RB 29/x/02 *)
      let class__ = mkclass c in
      let sels = parseselections others in
      notifyselect (getdisplay ()) (Some (mkpos x y, class__)) sels;
      getCommand displayopt
  | "DESELECT" :: sels ->
      let sels = parseselections sels in
      (* DESELECT sels means something has been taken away, and this is the selection now. *)
      notifyselect (getdisplay ()) None sels;
      getCommand displayopt
  (* | "DRAGQ" :: x :: y :: _ ->
         dropsource :=
           els_of_formulahit "getCommand DRAGQ"
             (locateHit (getdisplay ()) (mkpos x y) None HitPath);
         TextCommand ["DRAGQUERY"]
   *)
  | "DROP" :: sx :: sy :: tx :: ty :: _ ->
      let rec decode x y =
        els_of_formulahit "getCommand DROP"
          (locateHit (getdisplay ()) (mkpos x y) None HitPath)
      in
      dropsource := Some (decode sx sy);
      droptarget := Some (decode tx ty);
      TextCommand ["DROPCOMMAND"]
  | "COMMAND" :: comm -> TextCommand comm
  | _ ->
      showAlert ("getCommand (interaction) cannot understand " ^ text);
      getCommand displayopt

let showallprovisos = ref false

let rec filterprovisos ps =
  if !showallprovisos then ps else Proviso.provisovisible <| ps

let rec sortprovisos ps =
  sort
    (fun p1 p2 ->
       let b1 = Proviso.provisovisible p1 in
       let b2 = Proviso.provisovisible p2 in
       (not b1 && b2) ||
       (b1 = b2 && Proviso.earlierproviso (Proviso.provisoactual p1) (Proviso.provisoactual p2)))
    ps

let rec displayProvisos cxt =
  let ps = sortprovisos (Proviso.compressVisProvisos (provisos cxt)) in
  Japeserver.displayProvisos 
    (Proviso.invisbracketedstring_of_visproviso true <* filterprovisos ps)

let displayGivens givens =
  Japeserver.displayGivens (numbered (List.map (invisbracketedstring_of_seq true) givens))

let rec printProvisos outstream cxt =
  let ps = sortprovisos (provisos (rewritecxt cxt)) in
  match ps with
    [] -> ()
  | ps ->
      output_string outstream "(PROVIDED ";
      List.iter
        (fun p ->
           output_string outstream
             (("\"" ^ Proviso.string_of_visproviso p) ^ "\" ");
           output_string outstream "\n")
        (filterprovisos ps);
      output_string outstream ")"

let rec printGivens outstream givens =
  match givens with
    [] -> ()
  | gs ->
      output_string outstream "(GIVEN ";
      List.iter
        (fun g ->
           output_string outstream (("\"" ^ string_of_seq g) ^ "\" ");
           output_string outstream "\n")
        gs;
      output_string outstream ")"

let showState displaystate (Proofstate p) withgoal =
    let ds = showProof displaystate p.target p.goal p.cxt p.tree withgoal in
    displayProvisos p.cxt; ds

let printState outstream (Proofstate p) withgoal =
    printProof outstream p.target p.goal p.cxt p.tree withgoal;
    printGivens outstream p.givens;
    printProvisos outstream p.cxt

let rec alterTip displaystate cxt gpath tree root ((selishyp, selpath, selel), ss) =
  let (wholepath, wholetree) = Prooftree.Tree.makewhole cxt root tree gpath in
  if Prooftree.Tree.Fmttree.validelement selishyp wholetree selel wholepath then
    let (cxt, subst) = _Subst_of_selection false ss cxt in
    let (Seq (st, hs, cs)) =
      try Prooftree.Tree.Fmttree.findTip tree gpath with
        _ -> raise (Catastrophe_ ["findTip failed in alterTip"])
    in
    let (newel, side) =
      replaceelement (if selishyp then hs else cs) selel subst
    in
    (* safety first: we use a dummy context in replaceTip *) 
    cxt, newel,
    Prooftree.Tree.replaceTip dont_rewrite_with_this gpath tree
      (Seq (if selishyp then st, side, cs else st, hs, side))
  else
    raise
      (Selection_
         ["your selection "; string_of_path selpath;
          " wasn't on the path to the goal "; string_of_path wholepath])

