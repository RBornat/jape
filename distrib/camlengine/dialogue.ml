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

(* This is decaying. Behaviour of the command loop, except when recently tested, is becoming 
 * unpredictably bizarre.  This is perhaps due to the separation of concerns between interaction
 * (which deals with some commands from the interface, and translates the rest) and the command
 * loop.  pointToSequent is where a good deal of the confusion centres ...
 * RB 6/vii/2000
 *)
open Answer
open Button
open Cxtfuns
open Displaystate
open Disproof
open Doubleclick
open Hit
open Interaction
open Japeenv
open Listfuns
open Mappingfuns
open Name
open Proofstate
open Prooftree.Tree
open Prooftree.Tree.Fmttree
open Proofstore
open Rewrite
open Runproof
open Sequent
open Seqtype
open Sml
open Stringfuns
open Tacticfuns
open Thing
open Treeformat.Fmt

(* this is the remains of a huge sml functor argument *)

exception AtoI_ = Miscellaneous.AtoI_
exception Catastrophe_ = Miscellaneous.Catastrophe_
exception ParseError_ = Miscellaneous.ParseError_
exception Tacastrophe_ = Miscellaneous.Tacastrophe_
exception None_ = Optionfuns.None_
exception Use_ = Paragraph.Use_
exception Verifyproviso_ = Provisofuns.Verifyproviso_
       
let rec resetallcachesandvariables () =
  Alert.resetalertpatches ();
  Binding.clearbindingdirectives ();
  Button.initButtons ();
  Button.resetfontstuff ();
  Disproof.clearforcedefs ();
  Doubleclick.cleardoubleclicks ();
  Japeserver.resetcache ();
  Minwaste.resetminwcache ();
  Proofstore.clearproofs ();
  Proofstate.clearautorules ();
  Sequent.resetsyntaxandturnstiles ();
  Symbol.resetSymbols ();
  Tacticfuns.resetcaches ();
  Termstore.resettermstore ();
  Termparse.resettermparse ();
  Thing.clearthings ();
  Thing.clearstructurerules ()
         
let ( &~~ ) = Optionfuns.( &~~ )
let atoi = Miscellaneous.atoi
let autoselect = Miscellaneous.autoselect
let autoAdditiveLeft = Miscellaneous.autoAdditiveLeft
let autoAdditiveRight = Miscellaneous.autoAdditiveRight
let clearbindingdirectives = Binding.clearbindingdirectives
let closedbugfile = Miscellaneous.close_reportfile
let consolereport = Miscellaneous.consolereport
let consolequery = Miscellaneous.consolequery
let createdbugfile = Miscellaneous.create_reportfile

(* fun draganddropmapping cxt = 
    Provisofuns.draganddropmapping 
      (List.map Proviso.provisoactual (Cxtfuns.provisos cxt))
*)

let elementstring = Termstring.elementstring
let explodeCollection = Termfuns.explodeCollection
let facts = Facts.facts
let get_oplist = Symbol.get_oplist
let findfirst = Optionfuns.findfirst
let givenMenuTactic = Miscellaneous.givenMenuTactic
       
let rec initGUI () =
  (Japeserver.setinvischars 
     (String.make 1 Miscellaneous.onbra, String.make 1 Miscellaneous.onket)
     (String.make 1 Miscellaneous.offbra, String.make 1 Miscellaneous.offket)
     (String.make 1 Miscellaneous.outbra, String.make 1 Miscellaneous.outket)
     (String.make 1 Miscellaneous.lockbra, String.make 1 Miscellaneous.lockket) :
   unit);
  Button.initFonts ()

let isCutStep = Prooftree.Tree.isCutStep
let mkvisproviso = Proviso.mkvisproviso
let observe = Miscellaneous.observe
let optionstring = Optionfuns.optionstring
let ( |~ ) = Optionfuns.( |~ )
let ( |~~ ) = Optionfuns.( |~~ )
let parseCurriedArgList =
  Termparse.tryparse (fun _ -> Termparse.parsecurriedarglist ())
let parseTactic = Termparse.asTactic Termparse.term_of_string
let parseTerm = Termparse.term_of_string
let parseTermCOMMAList =
  Termparse.tryparse
    (fun _ ->
       Termparse.parseList Termparse.canstartTerm Termparse.parseTerm
         Symbol.commasymbol)
let proofsdone = Runproof.proofsdone
let provisoactual = Proviso.provisoactual
let provisostring = Proviso.provisostring
let provisovisible = Proviso.provisovisible
let sameresource = Termfuns.sameresource
let seektipselection = Miscellaneous.seektipselection
let showInputError = Symbol.showInputError
let string2paragraph = Paragraph.string2paragraph
let tacticstring = Tactic.tacticstring
let termstring = Termstring.termstring
let try__ = Optionfuns.try__
let _The = Optionfuns._The
let _Title = Version._Title
let uncurry2 = Miscellaneous.uncurry2
let _Version = Version._Version
let verifyprovisos = Provisofuns.verifyprovisos

let profiling = ref false (* see below *)
let rec profileswitcher b =
  profiling := b (* ; if b then Profile.profileOn () else Profile.profileOff () *)
let rec profilereader () = !profiling
       
let thingguard = not <.> thingstodo
let tparam = Japeenv.guardedjapevar thingguard

let defaultenv =
  let bj = Japeenv.booljaperefvar
  and ij = Japeenv.intjaperefvar
  and sj = Japeenv.japerefvar
  and jv = Japeenv.japevar in
  let rec aj default r =
    Japeenv.unboundedjapevar default
      ((fun v -> r := Name.namestring (Name.namefrom v)),
       (* is this too much work to avoid a few quotes? *)
       (fun () -> Name.namestring (Name.namefrom !r)))

and ajd r = aj !r r in
(* default settings for all variables accessible via Japeish *)
let pairs =
  ["alwaysshowturnstile"  , bj                         false        Sequent.alwaysshowturnstile;
   "applyconjectures"     , bj                         true         Miscellaneous.applyconjectures;
   "applydebug"           , ij                         0            Applyrule.applydebug;
   "applyderivedrules"    , bj                         true         Miscellaneous.applyderivedrules;
   "autoAdditiveLeft"     , tparam (bj                 false        Miscellaneous.autoAdditiveLeft);
   "autoAdditiveRight"    , tparam (bj                 false        Miscellaneous.autoAdditiveRight);
   "autoselect"           , bj                         false        Miscellaneous.autoselect;
   "bindingdebug"         , bj                         false        Binding.bindingdebug;
   "boxfolddebug"         , bj                         false        Boxdraw.boxfolddebug;
   "boxlinedisplay"       , sj ["left"; "right"]       "right"      Boxdraw.boxlinedisplay;
   "boxseldebug"          , bj                         false        Boxdraw.boxseldebug;
   "cuthidingdebug"       , bj                         false        Prooftree.Tree.cuthidingdebug;
   "debracketapplications", bj                         false        Termstring.debracketapplications;
   "displaystyle"         , jv ["box"; "tree"]         "tree"       (Interaction.setdisplaystyle, Interaction.getdisplaystyle);
   "disproofdebug"        , bj                         false        Disproof.disproofdebug;
   "eqalphadebug"         , bj                         false        Termfuns.eqalphadebug;
   "factsdebug"           , bj                         false        Facts.factsdebug;
   "filteredfmt"          , ajd                                     Prooftree.Tree.filteredfmt;
   "FINDdebug"            , bj                         false        Tacticfuns._FINDdebug;
   "FOLDdebug"            , bj                         false        Tacticfuns._FOLDdebug;
   "foldedfmt"            , ajd                                     Prooftree.Tree.foldedfmt;
   "foldformulae"         , bj                         false        Miscellaneous.foldformulae; (* default false, till non-Mac GUIs catch up *)
   "givenMenuTactic"      , aj                         "GIVEN"      Miscellaneous.givenMenuTactic;
   "hidecut"              , bj                         true         Boxdraw.hidecut;
   "hidehyp"              , bj                         true         Boxdraw.hidehyp;
   "hidereflexivity"      , bj                         true         Boxdraw.hidereflexivity;
   "hidetransitivity"     , bj                         false        Boxdraw.hidetransitivity;
   "hideuselesscuts"      , bj                         false        Prooftree.Tree.hideuselesscuts;
   "innerassumptionplural", ajd                                     Boxdraw.innerassumptionplural;
   "innerassumptionword"  , ajd                                     Boxdraw.innerassumptionword;
   "interpretpredicates"  , tparam (bj                 false        Predicate.interpretpredicates);
   "matchdebug"           , bj                         false        Match.matchdebug;
   "menudebug"            , bj                         false        Menu.menudebug;
   "minwastedebug"        , bj                         false        Minwaste.minwastedebug;
   "outerassumptionplural", ajd                                     Boxdraw.outerassumptionplural;
   "outerassumptionword"  , ajd                                     Boxdraw.outerassumptionword;
   "outermostbox"         , bj                         true         Boxdraw.outermostbox;
   "predicatedebug"       , bj                         false        Predicate.predicatedebug;
   "profiling"            , Japeenv.booljapevar        false        (profileswitcher, profilereader);
   "prooftreedebug"       , bj                         false        Prooftree.Tree.prooftreedebug;
   "prooftreedebugheavy"  , bj                         false        Prooftree.Tree.prooftreedebugheavy;
   "prooftreerewinfdebug" , bj                         false        Prooftree.Tree.prooftreerewinfdebug;
   "provisodebug"         , bj                         false        Proviso.provisodebug;
   "rawfmt"               , ajd                                     Prooftree.Tree.rawfmt;
   "reasonstyle"          , sj ["short"; "long"]       "long"       Prooftree.Tree.reasonstyle;
   "rewritedebug"         , bj                         false        Rewrite.rewritedebug;
   "screenpositiondebug"  , bj                         false        Miscellaneous.screenpositiondebug;
   "seektipselection"     , bj                         true         Miscellaneous.seektipselection;
   "showallproofsteps"    , bj                         false        Prooftree.Tree.showallproofsteps;
   "showallprovisos"      , bj                         false        Interaction.showallprovisos;
   "substdebug"           , bj                         false        Substmapfuns.substdebug;
   "symboldebug"          , bj                         false        Symbol.symboldebug;
   "tactictracing"        , bj                         false        Tacticfuns.tactictracing;
   "textselectionmode"    , sj ["subformula"; "token"] "subformula" Miscellaneous.textselectionmode;
   "thingdebug"           , bj                         false        Thing.thingdebug;
   "thingdebugheavy"      , bj                         false        Thing.thingdebugheavy;
   "truncatereasons"      , bj                         false        Miscellaneous.truncatereasons;
   "tryresolution"        , bj                         true         Tacticfuns.tryresolution;
   "unfilteredfmt"        , ajd                                     Prooftree.Tree.unfilteredfmt;
   "unifydebug"           , bj                         false        Unify.unifydebug;
   "varbindingsdebug"     , bj                         false        Termfuns.varbindingsdebug]
in
let rec bjnr r () = bj !r r

and ujnr r () = Japeenv.unboundedjaperefvar !r r in
let nonresetpairs =
  ["termhashing", bjnr Termstore.termhashing;
   "tacticresult", ujnr Tacticfuns.tacticresult]
in
(* make sure we don't re-evaluate pairs every time, because of 
 * the ajd function, which takes the current value of a variable 
 * as the default ...
 *)
let rec defaultenv () =
  List.iter Japeenv.resetvar (List.map snd pairs);
  nj_revfold (uncurry2 Japeenv.(++))
    (List.map
       (uncurry2 Japeenv.( ||-> ) <.> ((fun (s, v) -> Name.namefrom s, v)))
       (nj_fold (fun ((s, f), ps) -> (s, f ()) :: ps) nonresetpairs pairs))
    Japeenv.empty
in
defaultenv

let displaynames =
  ["displaystyle"; "showallprovisos"; "showallproofsteps"; "reasonstyle";
   "truncatereasons"] 
             
let boxdisplaynames =
  ["boxlinedisplay"; "foldformulae"; "hidecut"; "hidehyp"; "hidetransitivity";
   "hidereflexivity"; "hideuselesscuts"]
       
let displayvars = List.map Name.namefrom (displaynames @ boxdisplaynames)

let rec mustredisplay env vals =
  let dispenv =
    mkmap (displayvars ||| vals)
  in
  let nenv =
    mkmap (List.map (fun n -> Name.namestring n, n) displayvars)
  in
  let rec lookup s =
    match
	((nenv <@> s) &~~ (fun n -> Japeenv.(<@>) env n))
    with
      Some t -> Some (termstring t)
    | None -> None
  in
  let rec changed s =
    lookup s <> (nenv <@> s &~~ (fun n -> dispenv <@> n))
  in
  List.exists changed displaynames ||
  lookup "displaystyle" = Some "box" && List.exists changed boxdisplaynames
         
let profileOn = (* Profile.profileOn *) (fun _ -> ())
let profileOff = (* Profile.profileOff *) (fun _ -> ())
let profileReset = (* Profile.reset *) (fun _ -> ())
let profileReport = (* Profile.report *) (fun _ -> ())

let rec disproof_finished =
  function
    Some state ->
      let res =
        disproofstate_countermodel state && disproofstate_conclusive state
      in
      (* consolereport ["disproof_finished ", disproofstatestring state, " => ", string_of_int res]; *)
      res
  | None -> false

exception Io of exn

type 'a histrec = { now : 'a; pasts : 'a list; futures : 'a list }
type 'a hist = Hist of 'a histrec

let string_of_hist f (Hist h) =
  "Hist{now=" ^ f (h.now) ^ "; pasts=" ^ bracketedliststring f ";" h.pasts ^
  "; futures=" ^ bracketedliststring f ";" h.futures

type winhistrec = 
      { changed : bool; proofhist : proofstate hist;
        disproofhist : disproofstate hist option }
type winhist = WinHist of winhistrec

let rec winhist_changed (WinHist {changed = changed}) = changed
let rec winhist_proofhist (WinHist {proofhist = proofhist}) = proofhist
let rec winhist_disproofhist (WinHist {disproofhist = disproofhist}) = disproofhist
let rec winhist_proofnow (WinHist {proofhist = Hist {now = now}}) = now
let rec winhist_disproofnow =
  function
    WinHist {disproofhist = Some (Hist {now = now})} -> Some now
  | _ -> None

let rec hist_now (Hist {now = now}) = now
let rec hist_pasts (Hist {pasts = pasts}) = pasts
let rec hist_futures (Hist {futures = futures}) = futures

let rec withchanged (WinHist wh) changed = WinHist {wh with changed = changed}
let rec withproofhist (WinHist wh) proofhist = WinHist {wh with proofhist = proofhist}
let rec withdisproofhist (WinHist wh) disproofhist = WinHist {wh with disproofhist = disproofhist}

let rec new_hist now = Hist {now = now; pasts = []; futures = []}

let rec withnow (Hist h) now = Hist {h with now = now}
let rec withpasts (Hist h) pasts = Hist {h with pasts = pasts}
let rec withfutures (Hist h) futures = Hist {h with futures = futures}
(* there are several things we can do with a history *)

let rec forward_step (Hist {now = now; pasts = pasts}) now' =
    Hist {now = now'; pasts = now :: pasts; futures = []}
let rec insert_step (Hist {now = now; pasts = pasts; futures = futures}) now' =
    Hist {now = now'; pasts = pasts; futures = now :: futures}
let rec append_step (Hist {now = now; pasts = pasts; futures = futures}) now' =
    Hist {now = now'; pasts = now :: pasts; futures = futures}

let rec redo_step =
  function
    Hist {now = now; pasts = pasts; futures = now' :: futures} ->
      Some (Hist {now = now'; pasts = now :: pasts; futures = futures})
  | _ -> None

let rec undo_step =
  function
    Hist {now = now; pasts = now' :: pasts; futures = futures} ->
      Some (Hist {now = now'; pasts = pasts; futures = now :: futures})
  | _ -> None

let rec reparentprovisos hist =
  let phist = winhist_proofhist hist in
  let proof = hist_now phist in
  let cxt = selfparentprovisos (proofstate_cxt proof) in
  withproofhist hist (withnow (phist) (withcxt (proof) (cxt)))

type proofinforec =
      { title : name * int; proofnum : int; displayvarsvals : string list;
        needsrefresh : bool; displaystate : displaystate; hist : winhist;
        fromstore : bool }
type proofinfo = Pinf of proofinforec

let rec proofinfo_title (Pinf {title = title}) = title
let rec proofinfo_proofnum (Pinf {proofnum = proofnum}) = proofnum
let rec proofinfo_displayvarsvals (Pinf {displayvarsvals = displayvarsvals}) = displayvarsvals
let rec proofinfo_needsrefresh (Pinf {needsrefresh = needsrefresh}) = needsrefresh
let rec proofinfo_displaystate (Pinf {displaystate = displaystate}) = displaystate
let rec proofinfo_hist (Pinf {hist = hist}) = hist
let rec proofinfo_fromstore (Pinf {fromstore = fromstore}) = fromstore

let rec withtitle (Pinf pi) title = Pinf {pi with title = title}
let rec withproofnum (Pinf pi) proofnum = Pinf {pi with proofnum = proofnum}
let rec withdisplayvarsvals (Pinf pi) displayvarsvals = 
  Pinf {pi with displayvarsvals = displayvarsvals}
let rec withneedsrefresh (Pinf pi) needsrefresh = Pinf {pi with needsrefresh = needsrefresh}
let rec withdisplaystate (Pinf pi) displaystate = Pinf {pi with displaystate = displaystate}
let rec withhist (Pinf pi) hist = Pinf {pi with hist = hist}
let rec withfromstore (Pinf pi) fromstore = Pinf {pi with fromstore = fromstore}

type showstate = ShowProof | ShowDisproof | ShowBoth | DontShow

let setComment = Alert.setComment <.> implode
let showAlert = Alert.showAlert Alert.defaultseverity_alert <.> implode

let rec screenquery ss y n def =
  let bs = [y, true; n, false] in
  Alert.ask (Alert.defaultseverity bs) (implode ss) bs def
let rec uncurried_screenquery (ss, y, n, def) = screenquery ss y n def

let savefilename : string option ref = ref None

let mbcache : (name, term ref) mapping ref = ref empty

let rec apply_cleanup () = selections := None

exception Applycommand_ of proofstate option
let rec docommand displaystate env target comm =
  fun (Proofstate {cxt = cxt; tree = tree; givens = givens} as state) ->
    let r =
      (let state =
         withroot
           (withtarget (withgoal (state) (Some target)) (Some target)) (None)
       in
       applyLiteralTactic (Some displaystate) env (respace comm)
         state)
    in apply_cleanup (); r

let rec badsel ss = showAlert ss; raise (Applycommand_ None)

(* There appear to be two reasonable behaviours, given a selection and a command.
 * 1. (the original) -- resolve the selection to a single tip, if possible, and work there.
 * 2. (the new and odd) -- just take exactly what was given, and damn the consequences.
 *
 * The new, odd, behaviour supports use of a box-and-line display in either forward or
 * backward mode.  It means that the user (tactic programmer) must guard against things
 * like acting at a non-tip conclusion, but that's possible now.
 *)
let rec pointToSequent
  displaystate env comm
    (target, concopt, hyps, csels, hsels, givensel as fsel) =
  fun (Proofstate {tree = tree} as state) ->
    try
      let rec doit path =
        selections :=
          Some (path, concopt, hyps, csels, hsels, givensel);
        docommand displaystate env path comm state
      in
      if !seektipselection then
        if isTip (followPath tree target) then doit target
        else
          match concopt with
            None ->
              begin match
                   (fun (path, _) ->
                      pathPrefix tree target path &&
                      not
                        (List.exists
                           (fun hypel -> not (validhyp tree hypel path))
                           hyps)) <|
                   allTipConcs tree
              with
                [path, _] -> doit path
              | [] ->
                  badsel
                    ["that is a dead hypothesis -- there are no unproved \
                                         conclusions to which it is relevant."]
              | _ ->
                  badsel
                    ["there is more than one unproved conclusion which \
                                          corresponds to that hypothesis - you must \
                                          select one of them as well as the hypothesis."]
              end
          | Some _ -> badsel ["that conclusion is already proved."]
      else doit target
    with
      Applycommand_ sopt -> apply_cleanup (); sopt
    | exn -> apply_cleanup (); raise exn

let rec nohitcommand displaystate env textselopt comm done__ =
  fun (Proofstate {cxt = cxt; tree = tree} as state) ->
    try
      let target =
        match !seektipselection, allTipConcs tree with
          _, [path, _] -> path
        | true, _ :: _ ->
            (* forgive them their trespasses, never mind seektipselection *)
            badsel
              ["There is more than one unproved conclusion. \
                                  Please select the one you want to work on."]
        | true, [] ->
            badsel
              [if done__ () then "The proof is finished!"
               else "There are no remaining unproved conclusions!"]
        | _ -> rootPath tree
      in
      selections :=
        begin match textselopt with
          Some (proofsels, givensels) ->
            (* this should be in Interaction.sml *)
            let (concsels, hypsels) =
              nj_fold
                (function
                   (ConcHit (_, c), ss), (cs, hs) -> (c, ss) :: cs, hs
                 | (HypHit (_, h), ss), (cs, hs) -> cs, (h, ss) :: hs
                 | (AmbigHit (_, (_, h)), ss), (cs, hs) ->
                     cs, (h, ss) :: hs)
                proofsels ([], [])
            in
            Some (target, None, [], concsels, hypsels, givensels)
        | None -> None
        end;
      docommand displaystate env target comm state
    with
      Applycommand_ sopt -> apply_cleanup (); sopt
    | exn -> apply_cleanup (); raise exn
    
(* local *)
let foldstuff = false, "", Some []

let defaultfolded = RotatingFormat (0, [foldstuff])

let rec rotateFormat =
  function
    tfk, DefaultFormat -> Some (tfk, defaultfolded)
  | tfk, RotatingFormat (i, nfs) ->
      Some
        (tfk,
         RotatingFormat ((if i >= List.length nfs then 0 else i + 1), nfs))

let rec getfmt mess tree path =
  try
    let subproof = followPath tree path in
    if isTip subproof then
      begin
        showAlert [mess (); " -- it's not a proved conclusion."]; None
      end
    else
      match format subproof with
        TreeFormat tf -> Some tf
  with
    FollowPath_ _ -> None

let rec parent tree path =
  try Some (parentPath tree path) with
    FollowPath_ _ -> None

let rec leftCutParent tree path =
  match parent tree path with
    Some parentpath ->
      if isCutStep tree parentpath &&
         (* going left should be impossible *)
         (try let _ = (siblingPath tree path true : path) in false with _ -> true)
      then
        Some parentpath
      else None
  | _ -> None
type layoutcommand = BacktrackCommand
		   | PruneCommand
		   | HideShowCommand
		   | ExpandContractCommand
		   | HideRootCommand
		   | ExposeParentCommand
		   | HideCutCommand

let rec doLayout command =
  fun (Proofstate {tree = tree} as state) path ->
    match command with
      BacktrackCommand ->
        raise (Catastrophe_ ["doLayout BacktrackCommand!"])
    | PruneCommand -> Some (true, prunestate path state)
    | HideShowCommand ->
        getfmt (fun () -> "can't hide subproofs there") tree path 
        &~~
        (function tfk, RotatingFormat (i, nfs) ->
           (findfirst
              (fun (i, nf) -> if nf = foldstuff then Some i else None)
              (numbered nfs) &~~
            (fun i' ->
               Some (tfk, RotatingFormat ((if i = i' then i + 1 else i'), nfs)))) 
           |~~
           (fun _ -> Some (tfk, RotatingFormat (0, foldstuff :: nfs)))
         | tfk, DefaultFormat -> Some (tfk, defaultfolded)) 
         &~~
         (fun fmt' ->
            Some (false, withtree (state) (set_prooftree_fmt tree path (TreeFormat fmt'))))
    | ExpandContractCommand ->
        getfmt
          (fun () -> raise (Catastrophe_ ["getLayout sees EXPAND/CONTRACT on a Tip!!!"]))
          tree path 
        &~~
        (function  _, DefaultFormat as fmt ->
           if null (subtrees (followPath tree path)) then
              (showAlert ["no point double-clicking that -- it doesn't have any subproofs"];
               None)
           else Some fmt
        | fmt -> Some fmt)
        &~~ rotateFormat 
        &~~
        (fun tf ->
           Some (false, withtree (state) (set_prooftree_fmt tree path (TreeFormat tf))))
    | HideRootCommand ->
        let rec hideit () =
          try
            let (TreeFormat (_, tff)) = get_prooftree_fmt tree path in
            Some
              (false,
               withtree
                 (state)
                  (set_prooftree_fmt tree path
                    (TreeFormat (HideRootFormat, tff))))
          with
            FollowPath_ stuff ->
              showAlert ["FollowPath_ in HIDEROOT?? "]; None
        in
        begin match leftCutParent tree path with
          Some _ ->
            begin match
              visible_subtrees !showallproofsteps (followPath tree path)
            with
              Some [_] -> hideit ()
            | _ ->
                showAlert
                  ["don't hide that conclusion: it's a cut hypothesis"];
                None
            end
        | None -> hideit ()
        end
    | ExposeParentCommand ->
        begin match parent tree path with
          None ->
            showAlert
              ["that's the root of the proof -- it doesn't have a parent!"];
            None
        | Some parentpath ->
            match get_prooftree_fmt tree parentpath with
              TreeFormat (HideRootFormat, tff) ->
                Some
                  (false,
                   withtree
                     (state)
                      (set_prooftree_fmt tree parentpath
                        (TreeFormat (SimpleFormat, tff))))
            | _ -> showAlert ["the parent isn't hidden!"]; None
        end
    | HideCutCommand ->
        match leftCutParent tree path with
          None ->
            showAlert
              ["can't hide that line -- it's not a cut hypothesis"];
            None
        | Some cutpath ->
            let (TreeFormat (_, tff)) = get_prooftree_fmt tree cutpath in
            Some
              (false,
               withtree
                 (state)
                  (set_prooftree_fmt tree cutpath
                    (TreeFormat (HideCutFormat, tff))))

let rec getLayoutPath displaystate c pathkind =
    (findLayoutSelection displaystate pathkind |~~
     (fun _ ->
        showAlert
          ["Select a single hypothesis, or a single conclusion, or a reason before ";
           match c with
             PruneCommand -> "pruning"
           | ExpandContractCommand -> "expanding/contracting"
           | HideShowCommand -> "hiding/showing"
           | BacktrackCommand -> "backtracking"
           | HideRootCommand -> "hiding conclusion"
           | ExposeParentCommand -> "exposing parent"
           | HideCutCommand -> "hiding cut hypothesis"];
        None))

let rec tryLayout displaystate c pathkind hist =
    getLayoutPath displaystate c pathkind &~~
    doLayout c (winhist_proofnow hist) &~~
    (function
       false, proof' ->
         let phist = winhist_proofhist hist in
         Some (withproofhist hist (withnow (phist) (proof')))
     | true, proof' ->
         let phist = winhist_proofhist hist in
         Some (withproofhist hist (append_step phist proof')))

let rec recorddisplayvars env =
  try 
    (fun s -> termstring (_The (Japeenv.(<@>) env s))) <* displayvars
  with
    _The_ ->
      raise
        (Catastrophe_
           ["one or more of ";
            bracketedliststring namestring ", " displayvars;
            " isn't set!"])

let rec setdisplayvars env vals =
  List.iter (fun (s, v) -> Japeenv.set (env, s, parseTactic v))
    ((displayvars ||| vals))

(* proofmove doesn't set changed *)
let proofmove =
  fun (WinHist {proofhist = proofhist} as hist) proof' ->
    withproofhist hist (forward_step proofhist proof')

(* nor does disproofmove *)
let disproofmove a1 a2 =
  match a1, a2 with
    (WinHist {disproofhist = Some d} as hist), disproof' ->
      withdisproofhist hist (Some (forward_step d disproof'))
  | (WinHist {disproofhist = None} as hist), disproof' ->
      withdisproofhist hist (Some (Hist {now = disproof'; pasts = []; futures = []}))

(* disproofevolve does set changed *)
let disproofevolve a1 a2 =
  withchanged (disproofmove a1 a2) true
  
(* this function the same as evolve_proof except for non-application of AUTO tactics,
 * and the fact that it doesn't set changed.
 *)
let rec tryForward f =
  fun (WinHist {proofhist = Hist {now = proof}} as hist) ->
    f proof &~~ (fSome <.> proofmove hist)

let rec evolve_proof_explain explain displaystate env f =
  fun (WinHist {proofhist = Hist {now = proof}} as hist) ->
    match f proof with
      None -> explain (); None
    | Some proof' ->
        Some
          (withchanged
             (proofmove hist
                (rewriteproofstate
                   (autoTactics (Some displaystate) env
                      (Proofstate.autorules ()) proof')))
              (true))

let evolve_proof = evolve_proof_explain (fun () -> ())

let rec reset () =
  resetallcachesandvariables (); savefilename := None; mbcache := empty

let rec cleanup () = ()

let rec parseargs args =
  try parseTermCOMMAList args with
    ParseError_ _ -> []
exception QuitJape

(* interpretParasFrom includes its own disQuote, so no need for one here *)
let doUse = Paragraphfuns.interpretParasFrom

(* we have a mechanism -- in mbs, set up by paragraphfuns -- for allowing the GUI to 
   control the values of variables in the engine.  We have another mechanism -- see 
   the definition of mustredisplay in newjape.sml -- for allowing the value of variables
   in the engine to control how things are displayed, and to trigger redisplay when 
   necessary.
   
   We need a mechanism to allow variables in the engine to be mirrored in the GUI, or
   else to allow the user to assign values to variables in the GUI through the engine.
   We don't have one yet, but since there is only one variable involved --
   textselectionstyle -- it's not a big deal.  I don't even try to cache it ...
   RB 3.vii.01
 *)
exception Matchinmain_ exception Exit_
exception AddConjecture_
(* moved out for OCaml *)
exception Unify_
(* moved out for OCaml *)
exception Matchinbacktrack_
(* moved out for OCaml *)
   
let rec main a1 a2 =
  match a1, a2 with
    (env, proofs, mbs), (path :: args, _) ->
      begin try
        let rec doargs =
          function
            [] -> [], []
          | "-" :: args -> [], args
          | "-tree" :: args ->
              Interaction.setdisplaystyle "tree"; doargs args
          | "-box" :: args ->
              Interaction.setdisplaystyle "box"; doargs args
          | "-proofs" :: name :: args ->
              savefilename := Some name; doargs args
          | "" :: args -> doargs args
          | name :: args ->
              if String.sub (name) (0) (1) = "-" then [], name :: args
              else let (names, args) = doargs args in name :: names, args
        in
        let (names, args) = doargs args in
        consolereport [_Title; _Version; "\n"];
        begin
          let (env, proofs, mbs) =
            try
              doUse showAlert uncurried_screenquery (env, proofs, mbs) names
            with
              ParseError_ m -> showInputError showAlert m; (env, proofs, mbs)
            | Use_          -> (env, proofs, mbs) (* already reported, we hope *)
          in
          Japeserver.sendVersion (_Title ^ _Version);
          initGUI ();
          reloadmenusandpanels Proofstore.provedordisproved (get_oplist ());
          mbcache := empty;
          rundialogue env mbs proofs
        end;
        ()
      with
        Exit_ -> ()
      | Japeserver.DeadGUI_ -> ()
      end
  | _, _ -> raise Matchinmain_


and rundialogue env mbs proofs =
  try
    let dialogue () = startcommands env mbs proofs in
    (* open RUN OCaml no like *)
    let _Int _ = observe ["[Interrupted]"]; interruptTactic () (* but don't crash *) in
    Moresys.onInterrupt _Int dialogue; terminateGUI ()
  with
    Catastrophe_ s -> reportGUIdead ("exception Catastrophe_ " :: s)
  | QuitJape -> reportGUIdead ["Jape finished\n"]
  | Japeserver.DeadGUI_ as exn -> raise exn
  | exn ->
      reportGUIdead
        ["Unexpected exception ["; Printexc.to_string exn;
         "] - Jape quitting"]

and startcommands env mbs proofs =
  (* there is an argument against checking duplication of proof titles
   * at this point: normally we will be resuscitating proofs that
   * already were duplicated. But never mind, that's the way the cookie
   * crumbles
   *)
  setComment [];
  commands (env, mbs, DontShow, addproofs false env proofs [])

and addproofs
  fromstore env (proofs : (name * proofstate * (seq * model) option) list)
    (pinfs : proofinfo list) =
  let rec f =
    fun
      ((name,
        (Proofstate {goal = goal; cxt = cxt; tree = tree; givens = givens}
           as state), disproofopt), pinfs) ->
      let n =
        List.length ((fun (Pinf {title = t, _}) -> t = name) <| pinfs)
      in
      if n <> 0 &&
         not
           (screenquery
              [if n = 1 then "There is already a proof of "
               else ("There are already " ^ string_of_int n) ^ " proofs of ";
               namestring name;
               " in progress - do you want to add another?"]
              "Add" "Cancel" 1)
      then
        pinfs
      else
        let (num, index) = biggestproofnum name pinfs in
        let num = num + 1 in
        let index = if n = 0 then 0 else index + 1 in
        let heading =
          if index = 0 then namestring name
          else ((namestring name ^ " [") ^ string_of_int index) ^ "]"
        in
        let state_cxt = selfparentprovisos cxt in
        let facts = facts (provisos state_cxt) state_cxt in
        let disproof = model2disproofstate facts tree disproofopt in
        Japeserver.openproof heading num;
        Pinf
          {title = name, index; proofnum = num; displayvarsvals = recorddisplayvars env;
           needsrefresh = false;
           displaystate =
             (let r = showFocussedProof goal cxt tree !autoselect in
                begin
                  setGivens givens;
                  setProvisos cxt;
                  match disproof with
                    None -> ()
                  | Some d -> Disproof.showdisproof d
                end; r);
           hist =
             WinHist {changed = false; proofhist = new_hist (withcxt (state) (state_cxt));
                      disproofhist = try__ new_hist disproof};
           fromstore = fromstore} :: pinfs
  in
  nj_fold f proofs pinfs

and biggestproofnum name pinfs =
  nj_fold
    (fun (Pinf {proofnum = proofnum; title = t, index}, (n, i)) ->
       max proofnum (n),
       (if t = name then max index (i) else i))
    pinfs (0, 0)

and endproof num name st dis =
  let proved = isproven st in
  let disproved = disproof_finished dis in
  Runproof.addproof showAlert uncurried_screenquery name proved st disproved (disproofstate2model dis) &&
  begin
    Japeserver.closeproof num;
    markproof (parseablenamestring name) (proved, disproved);
    true
  end

and commands (env, mbs, (showit : showstate), (pinfs : proofinfo list) as thisstate) =
  let rec findproof pinfs nstring =
    let n = atoi nstring in
    extract (fun (Pinf {proofnum = proofnum}) -> n = proofnum) pinfs
  in
  let rec newfocus (env, mbs, showit, (pinfs : proofinfo list) as state) =
    match pinfs with
      Pinf fg :: bgs ->
        setdisplayvars env (fg.displayvarsvals);
        env, mbs,
        (if fg.needsrefresh then ShowBoth else DontShow),
        pinfs
    | [] -> state
  in
  let rec addnewconjecture panel text =
    let text = respace text in
    let getpara = string2paragraph showAlert uncurried_screenquery in
    let (text, para) =
      (* praps it's just a conjecture *)
      try let t = "THEOREM INFER " ^ text in t, getpara t with
        ParseError_ rs ->
          (* praps it has params and stuff *)
          try let t = "THEOREM " ^ text in t, getpara t with
            ParseError_ rs' ->
              showAlert
                (["Cannot parse new conjecture "; text; " -- "] @
                   (if rs = rs' then rs
                    else
                      "\n\nTrying to read it as a sequent gave the error �" :: (rs @ "�.\n\nTrying to read it as a line of Japeish gave the error �" :: (rs' @ ["�."]))));
              raise AddConjecture_
    in
    let _ = (Paragraphfuns.interpret showAlert uncurried_screenquery [] []
               (env, [], [])
               (match parseablenamestring panel with
                  "" -> para
                | p ->
                    getpara
                      (((("CONJECTUREPANEL " ^ p) ^ " IS ") ^ text) ^ " END")) : 
               japeenv * (name * proofstate * (seq * model) option) list *
                         (name * (string * bool -> unit)) list)
    in
    let name = Paragraphfuns.conjecturename para in
    if namestring panel <> "" then
      Japeserver.panelentry (namestring panel) (namestring name) (parseablenamestring name);
    name
  in
  
  let rec printproof path state =
    let st = rewriteproofstate state in
    try
      let s = try Moresys.open_output_file path with exn -> raise (Io exn) in
      Interaction.printState s st true; close_out s
    with
      Io exn ->
        showAlert ["Cannot write file "; path; " ("; Printexc.to_string exn; ")"]
  in
  
  let writetonamedfile action filename =
    try
      let sfile = try Moresys.open_output_file (disQuote filename) with exn -> raise (Io exn) in
      action sfile; close_out sfile; true
    with
      Io exn -> showAlert ["Cannot write file "; filename; " ("; Printexc.to_string exn; ")"]; false
  in
  
  let rec saveproofs newfile =
    let rec doit sfile =
      let rec pf ps = (provisoactual <* (provisovisible <| ps)) in
      let rec f =
        fun (Pinf {title = t, _; hist = hist}) ->
          let (Proofstate {cxt = cxt; tree = tree; givens = givens}) = winhist_proofnow hist in
          saveproof sfile t Proofstage.InProgress tree (pf (provisos cxt)) givens
            (disproofstate2model (winhist_disproofnow hist))
      in
      Proofstore.saveproofs sfile; List.map f pinfs
    in
    match newfile, !savefilename with
      false, Some s -> 
        let _ = (writetonamedfile doit s : bool) in ()
    | _             ->
        match Japeserver.writeFileName "Save proofs as:" Japeserver.prooffiletype with
          Some s -> if writetonamedfile doit s then savefilename := Some s
        | None -> ()
  in
  let rec saveable () = not (null pinfs) || Proofstore.saveable () in
  let rec needssaving () =
    List.exists
      (function
         Pinf {hist = WinHist {changed = true}} -> true
       | _ -> false)
      pinfs ||
    not (Proofstore.saved ())
  in
  let rec proofundoable () =
    match pinfs with
      Pinf {hist = WinHist {proofhist = Hist {pasts = _ :: _}}} :: _ ->
        true
    | _ -> false
  in
  let rec proofredoable () =
    match pinfs with
      Pinf {hist = WinHist {proofhist = Hist {futures = _ :: _}}} :: _ ->
        true
    | _ -> false
  in
  let rec disproofundoable () =
    match pinfs with
      Pinf
        {hist = WinHist {disproofhist = Some (Hist {pasts = _ :: _})}} ::
      _ ->
        true
    | _ -> false
  in
  let rec disproofredoable () =
    match pinfs with
      Pinf
        {hist =
           WinHist {disproofhist = Some (Hist {futures = _ :: _})}} ::
      _ ->
        true
    | _ -> false
  in
  let rec finished proof disproof =
    isproven proof || disproof_finished disproof
  in
  let rec finishable () =
    match pinfs with
      pinf :: _ ->
        let hist = proofinfo_hist pinf in
        finished (winhist_proofnow hist) (winhist_disproofnow hist)
    | [] -> false
  in
  let rec resetable () =(* eggstolay, wormstoscratch, ... *)
   thingstodo () || saveable () in
  let rec askSave action y n cancel =
    Alert.askDangerously
      (implode ["Save your proofs before "; action; "?"])
      ("Save", (fun () -> saveproofs false; y))
      ("Don't save", (fun () -> n)) (fun () -> cancel) ()
  in
  
  let askResettheory anyway =
    if resetable () then
      if needssaving () then
        askSave "erasing the current theory" true true false
      else
        anyway ||
        screenquery ["Erase the current theory?"] "Erase" "Cancel" 1
    else begin reset (); true end
  in
  
  let doResettheory () =
    List.iter (fun (Pinf p) -> Japeserver.closeproof p.proofnum) pinfs;
    Japeserver.resettheory ();
    reset ();
    defaultenv (), [], DontShow, []
  in
  
  let default = env, mbs, DontShow, pinfs in
  
  let rec inside c f =
    match pinfs with
      [] ->
        showAlert
          ["There is no current proof, so you can't execute \"";
           respace c; "\""];
        default
    | (Pinf {displaystate = displaystate; hist = hist} as pinf) ::
      pinfs ->
        let (hist, showit) =
          match f displaystate hist with
            Some (show, hist) -> hist, show
          | None -> hist, DontShow
        in
        env, mbs, showit, withhist (pinf) (hist) :: pinfs
  in
  
  let rec outside c f =
    match pinfs with
      [] -> f ()
    | _ ->
        let n = List.length pinfs in
        showAlert
          [if n = 1 then "There is a proof "
           else ("there are " ^ string_of_int (List.length pinfs)) ^ " proofs";
           " in progress. You must close ";
           if n = 1 then "it" else "them"; " before the command \"";
           respace c; "\" can be executed."];
        default
  in
  
  (* this idea, which is an attempt to kickstart undo on disproofs, is ridiculously inelegant.
     I think I need to think ...
   *)
  let rec showproof =
    function
      Some hist -> Some (ShowProof, hist)
    | None      -> None
  in
  let rec showdisproof =
    function
      Some hist -> Some (ShowDisproof, hist)
    | None      -> None
  in
  let rec showboth =
    function
      Some hist -> Some (ShowBoth, hist)
    | None      -> None
  in
  
  let rec processcommand
    (env, mbs, (showit : showstate), (pinfs : proofinfo list) as
       thisstate)
      c =
    (* It is crucial that theorems are proved without application of arguments.
                  * Otherwise we can't say that we have proved the theorem as stated, 
                  * and then store a proof of it.
                  *)
    let rec doproof env mbs name pinfs =
      let rec doit givens seq provisos kind =
        try
          let (Proofstate {cxt = cxt} as state) =
            startstate env provisos givens seq
          in
          (* do this first, cos the cxt needs the proofId information 
           * before we can check the provisos 
           *)
          let cxt' = verifyprovisos cxt in
          env, mbs, DontShow,
          addproofs false env
            [name, withgivens (withcxt (state) (cxt')) (givens), None] pinfs
        with
          Verifyproviso_ p ->
            showAlert
              [kind; " "; namestring name; " has unsatisfiable proviso ";
               provisostring p];
            default
      in
      match freshThingtoprove name with
        Some (Theorem (_, provisos, seq)) ->
          doit [] seq ((mkvisproviso <* provisos)) "theorem"
      | Some (Rule ((_, provisos, givens, seq), ax)) ->
          if ax then
            begin
              showAlert
                [namestring name; " is an axiomatic rule, not a conjecture or a derived rule"];
              default
            end
          else
            doit givens seq ((mkvisproviso <* provisos)) "derived rule"
      | Some (Tactic _) ->
          showAlert
            [namestring name;
             " is a tactic, not a conjecture or a derived rule"];
          default
      | Some (Macro _) ->
          showAlert
            [namestring name;
             " is a tactic macro, not a conjecture or a derived rule"];
          default
      | None ->
          showAlert ["no stored conjecture named "; namestring name];
          default
    in
    
    let disproofact act move =
      inside c 
        (fun displaystate ->
           showdisproof <.> 
             (function
                WinHist {disproofhist = Some (Hist {now = d})} as hist ->
                  let proof = winhist_proofnow hist in
                  let cxt_now = proofstate_cxt proof in
                  let facts_now = facts (provisos cxt_now) cxt_now in
                  act facts_now cxt_now proof d &~~ (fSome <.> move hist)
              | _ -> raise (Catastrophe_ ["disproof action when no disproof state"])))
    in
    
    let disproofacteval act =
      disproofact
        (fun facts_now cxt_now proof d ->
           act facts_now d &~~ 
           (fSome <.> evaldisproofstate facts_now (proofstate_tree proof)))
    in
    
    let disproofuniverseact act =
      disproofacteval
        (fun facts_now d -> (act facts_now (disproofstate_universe d) &~~ (fun u -> Some (withdisproofuniverse d u))))
    in
    
    let worldlabelact act move cx cy s =
      disproofuniverseact
        (fun facts_now u -> act facts_now u (atoi cx, atoi cy) (parseTerm (disQuote s)))
        move
    in
    
    match c with
      []            -> default
    | word :: words ->
        match lowercase word, words with
          "apply", comm ->
            (let rec nosels tselopt =
               inside c
                 (fun displaystate ->
                    showproof <.> 
                      ((fun hist ->
                          evolve_proof displaystate env
                            (nohitcommand displaystate env tselopt comm
                               finishable)
                            hist)
                         ))
             in
             begin match pinfs with
               Pinf {displaystate = displaystate} :: _ ->
                 begin match findSelection displaystate with
                   Some (FormulaSel fsel) ->
                     inside c
                       (fun displaystate ->
                          showproof <.> 
                            ((fun here ->
                                evolve_proof displaystate env
                                  (pointToSequent displaystate env comm
                                     fsel)
                                  here)
                               ))
                 | Some (TextSel t) -> nosels (Some t)
                 | Some (ReasonSel _) ->
                     showAlert
                       ["only reason selection (applying "; respace comm;
                        ")"];
                     default
                 | None -> nosels None
                 end
             | _ ->
                 showAlert
                   ["no current proof (applying "; respace comm; ")"];
                 default
             end)

        | "applygiven", args ->
            processcommand thisstate
              ("apply" ::
                 parseablenamestring (namefrom !givenMenuTactic) :: args)

        | "assign", name :: value ->
            (try
              let value = parseTactic (respace value) in
              Japeenv.set (env, namefrom name, value);
              resetcaches ();
              default
            with
              Japeenv.OutOfRange_ s ->
                showAlert
                  ["error in "; respace c;
                   " --  value assigned should be "; s];
                default
            | Japeenv.NotJapeVar_ ->
                showAlert
                  ["error in "; respace c; " -- "; name;
                   " is not a Jape variable"];
                default
            | Japeenv.ReadOnly_ ->
                showAlert
                  ["error in "; respace c; " -- "; "you can't assign to ";
                   name; " at this point"];
                default
            | ParseError_ rs ->
                showAlert (["can't parse "; respace value; " -- "] @ rs);
                default)

        | "redo_disproof", [] ->
            inside c
              (fun _ ->
                 showdisproof <.> 
                   ((fun hist ->
                         (winhist_disproofhist hist &~~ redo_step &~~
                          (fSome <.> 
                             ((fun dh ->
                                 withdisproofhist (hist) (Some dh))
                                ))) |~~
                          (fun _ ->
                             showAlert ["nothing to redo!"]; None)))
                      )

        | "redo_proof", [] ->
            inside c
              (fun _ ->
                 showproof <.> 
                 (fun hist ->
                    (redo_step (winhist_proofhist hist) &~~
                     (fSome <.> (fun ph -> withproofhist hist ph))) |~~
                    (fun _ -> showAlert ["nothing to redo!"]; None)))

        | "refreshdisplay", [] ->
            begin match pinfs with
              Pinf
                {hist =
                   WinHist
                     {proofhist =
                        Hist {now = Proofstate {givens = givens}}}} ::
              _ ->
                setGivens givens
            | _ -> ()
            end;
            env, mbs, ShowBoth, pinfs

        | "steps", quota :: _ -> timestotry := atoi quota; default

        | "steps", [] ->
            (* this should be a variable in the environment *)
            showAlert ["Proof step quota is "; string_of_int !timestotry];
            default

        | "tellinterface", name :: interfacecommand ->
            (* Evaluate a variable name; construct a string for the interface *)
            let str =
              match Japeenv.(<@>) env (namefrom name) with
                Some t -> termstring t
              | None -> ""
            in
            Japeserver.showfile ((respace interfacecommand ^ " ") ^ str);
            (* This should be called -- "tellinterface" *)
            (* It's the way that an interface can do something
               with the value of a jape variable
            *)
            default

        | "undo_disproof", [] ->
            inside c
              (fun _ ->
                 showdisproof <.> 
                 (fun hist ->
                    ((winhist_disproofhist hist &~~ undo_step &~~
                      (fSome <.> (fun dh -> withdisproofhist (hist) (Some dh)))) 
                     |~~
                     (fun _ -> showAlert ["no disproof steps to undo!"]; None))))

        | "undo_proof", [] ->
            inside c
              (fun _ ->
                 showproof <.> 
                 (fun hist ->
                    (undo_step (winhist_proofhist hist) &~~
                     (fSome <.> ((fun ph -> withproofhist (hist) (ph))))) 
                    |~~
                    (fun _ -> showAlert ["no proof steps to undo!"]; None)))

        | "use", (file :: _ as files) ->
            proofsdone := false;
            begin try
              let oldfontstuff = getfontstuff () in
              (* interpretParasFrom assigns to this variable, perhaps *)
              let (env, ps, mbs) =
                doUse showAlert uncurried_screenquery (env, [], mbs) files
              in
              let proofsfound = !proofsdone || not (null ps) in
              if oldfontstuff <> getfontstuff () then initFonts ();
              Japeserver.emptymenusandpanels ();
              reloadmenusandpanels Proofstore.provedordisproved (get_oplist ());
              mbcache := empty;
              newfocus (env, mbs, DontShow, addproofs false env ps pinfs)
            with
              ParseError_ rs -> showAlert rs; default
            | Use_ -> default
            end

        | "version", [] -> showAlert [_Title; _Version]; default
        
        (* ********************* the disproof commands ************************* *)

        | "addworldlabel", [cx; cy; s] ->
            worldlabelact (fun _ -> addworldlabel) disproofevolve cx cy s

        | "deleteworldlabel", [cx; cy; s] ->
            worldlabelact (fun _ -> deleteworldlabel) disproofevolve cx cy s

        | "tileact", [s] ->
            disproofacteval (fun _ d -> Disproof.newtile d (parseTerm (disQuote s))) disproofmove

        | "addworld", [px; py; cx; cy] ->
            disproofuniverseact
              (fun _ u -> Disproof.addchild u (atoi px, atoi py) (atoi cx, atoi cy))
              disproofevolve

        | "addworldtolink", [px; py; cx; cy; lpx; lpy; lcx; lcy] ->
            disproofuniverseact
              (fun _ u -> Disproof.addchildtolink u (atoi px, atoi py) (atoi cx, atoi cy)
                                                  (atoi lpx, atoi lpy) (atoi lcx, atoi lcy))
              disproofevolve

        | "deleteworldlink", [fromx; fromy; tox; toy] ->
            disproofuniverseact
              (fun _ u -> Disproof.deletelink u (atoi fromx, atoi fromy) (atoi tox, atoi toy))
              disproofevolve

        | "deleteworld", [cx; cy] ->
            disproofacteval (fun _ d -> Disproof.deleteworld d (atoi cx, atoi cy))
            disproofevolve

        | "moveworld", [x; y; x'; y'] ->
            disproofacteval
              (fun _ d -> Disproof.moveworld d (atoi x, atoi y) (atoi x', atoi y'))
              disproofmove

        | "moveworldtolink", [x; y; x'; y'; px; py; cx; cy] ->
            disproofacteval
              (fun _ d -> Disproof.moveworldtolink d (atoi x, atoi y) (atoi x', atoi y')
                                                   (atoi px, atoi py) (atoi cx, atoi cy))
              disproofevolve
        | "splitworldlink", [fromx; fromy; tox; toy; newx; newy] ->
            disproofuniverseact
              (fun _ u -> Disproof.splitlink u (atoi fromx, atoi fromy) (atoi tox, atoi toy) (atoi newx, atoi newy))
              disproofevolve
              
        | "worldselect", cs ->
            disproofacteval
              (fun _ d ->
                 let rec pair =
                   function
                     cx :: cy :: cs -> (atoi cx, atoi cy) :: pair cs
                   | []             -> []
                   | _  -> raise (Catastrophe_
				    ["bad command (odd number of arguments): worldselect ";
				     bracketedliststring (fun s -> s) "," cs])
                 in
                 Disproof.worldselect d (pair cs))
             disproofmove

        | "disproof_selection_change", [] ->
            disproofacteval
              (fun _ d -> 
                 let selections = Interaction.findDisproofSelections() in
                 if disproofstate_selections d=selections then None else Some(withdisproofselections d selections))
              disproofmove (* i.e. selections can be undone .... hmmm. *) 
               
        | "disprove", [] ->
            inside c
              (fun displaystate ->
                 showdisproof <.> 
                   ((fun hist ->
                       let proof = winhist_proofnow hist in
                       let cxt_now = proofstate_cxt proof in
                       let facts_now = facts (provisos cxt_now) cxt_now in
                       let rec process_disproof disproof' =
                         let doit () = Some (disproofmove hist disproof') in
                         let seq' = disproofstate_seq disproof' in
                         match winhist_disproofnow hist with
                           None -> doit ()
                         | Some state ->
                             let seq = disproofstate_seq state in
                             if eqseqs (seq, seq') then
                               if issimplestuniverse (disproofstate_universe state)
                               then
                                 (showAlert ["You are already disproving "; seqstring seq];
                                  None)
                               else if
                                 screenquery
                                   ["You are already disproving "; seqstring seq; 
                                    " - do you want to wipe clean the world(s) you have built?"]
                                   "Wipe" "Cancel" 1
                               then doit ()
                               else None
                             else if
                               screenquery
                                 ["You are disproving "; seqstring seq;
                                  " - do you want to replace it with ";
                                  seqstring seq'; "?"]
                                 "Replace" "Cancel" 1
                             then doit ()
                             else None
                       in
                       match findSelection displaystate with
                         Some (FormulaSel (path, _, hyps, _, _, _)) ->
                           process_disproof
                             (disproof_start facts_now
                                (proofstate_tree proof) (Some path) hyps)
                       | _ ->
                           process_disproof
                             (disproof_start facts_now
                                (proofstate_tree proof) None []))
                      ))
        
        (* ******************* proof stuff ************************)
            
        | "unify", stuff ->
            begin try
              inside c
                (fun displaystate ->
                   let rec getsels =
                     function
                       x :: y :: zs, gs -> y :: getsels (zs, gs)
                     | _           , gs -> gs
                   in
                   let sels =
                     match findSelection displaystate with
                       Some
                         (FormulaSel (_, _, _, concsels, hypsels, givensels)) ->
                         nj_fold getsels [givensels]
                           (nj_fold getsels ((snd <* concsels))
                              (nj_fold getsels ((snd <* hypsels)) []))
                     | Some (TextSel (proofsels, givensels)) ->
                         nj_fold getsels [givensels]
                           (nj_fold getsels ((snd <* proofsels)) [])
                     | _ -> []
                   in
                   let args =
                     try (if null sels || null stuff then parseCurriedArgList
                                                     else (fun t -> [t]) <.> parseTerm)
                         (respace stuff)
                     with ParseError_ es ->
                       showAlert ("cannot parse unify " :: respace stuff :: " -- " :: es);
                       raise Unify_
                   in
                   let rec getit s =
                     try parseTerm s with
                       ParseError_ es ->
                         showAlert ("your selection " :: s :: " didn't parse -- " :: es);
                         raise Unify_
                   in
                   let selargs = (getit <* sels) in
                   if List.length args + List.length selargs < 2 then
                     begin
                       showAlert ["Unify must be given at least two things to work with!"];
                       raise Unify_
                     end
                   else
                       showproof <.> 
                         ((fun here ->
                             evolve_proof_explain
                               (fun () -> showAlert (explain ""))
                               displaystate env
                               (forceUnify (args @ selargs)) here)
                           ))
            with
              Unify_ -> default
            end

        | "saveengine", name :: _ ->
            outside c
              (fun () -> (* saverunning env mbs (disQuote name); *) default)

        | "done", [] ->
            begin match pinfs with
              [] -> showAlert ["Not in a proof"]; default
            | Pinf {title = t, _; proofnum = proofnum; hist = hist} ::
              pinfs' ->
                let proof = winhist_proofnow hist in
                let disproof = winhist_disproofnow hist in
                if finished proof disproof then
                  if endproof proofnum t proof disproof
                  then
                    newfocus (env, mbs, DontShow, pinfs')
                  else default
                else begin showAlert ["Not finished yet!"]; default end
            end

        | "showproof", stuff ->
            let name = namefrom (respace stuff) in
            begin match proofnamed name with
              None ->
                showAlert ["No stored proof of "; namestring name];
                default
            | Some (_, tree, provisos, givens, _, disproofopt) ->
                let proofstate =
                  mkstate ((mkvisproviso <* provisos)) givens tree
                in
                newfocus
                  (env, mbs, DontShow,
                   addproofs true env [name, proofstate, disproofopt]
                     pinfs)
            end

        | "print", [path] ->
            inside c
              (fun _ hist ->
                 printproof path (winhist_proofnow hist); None)

        | "quitnow", [] ->(*
          |  ("cd", [path]) =>
             (System.Directory.cd path handle _ =>
                     showAlert[path, ": No such file or directory [cd]"];
              default)
          *)
          (* used to be "QUIT" *)
          
           raise QuitJape
        
        (* ****************** the drag and drop stuff is moribund *********************** 
            | ("dragquery", []) =>
               (case pinfs of
                  [] => (raise Catastrophe_ ["dragquery not in a proof"]; default)
                | Pinf{hist=WinHist{now=Proofstate{cxt,...},...},...}::_ =>
                    let val dragees = !dropsource
                        val dNdm = draganddropmapping cxt
                        fun targets d =
                          nj_fold (fn ((s,t),ts) => 
                                  if sameresource (s,d) then elementstring t::ts else ts
                               ) dNdm []
                        fun List.concat (ts:string list,rs) = (listsub op= ts rs)@rs
                    in
                        Japeserver.dragtargets (nj_fold List.concat (targets <* dragees) []);
                        default
                    end
              )                  
            | ("dropcommand", []) =>
                 inside c
                   (fn displaystate =>
                     (fn here => evolve_proof_explain
                                 (fn () => showAlert(explain ""))
                                 displaystate env
                                 (doDropUnify (!droptarget) (!dropsource))
                                 here
                     )
                   )
            *)
            
        (* ******************** proof control *********************** *)
            
        | "backtrack", _ ->
            inside c
              (fun displaystate ->
                 showproof <.> 
                   ((fun here ->
                         (getLayoutPath displaystate BacktrackCommand
                            PrunePath &~~
                          (fun path ->
                             try
                               let there = ref here in
                               let rec stillcomposite =
                                 fun
                                   (WinHist
                                      {proofhist =
                                         Hist
                                           {now =
                                              Proofstate
                                                {tree = tree}}}) ->
                                   try let _ = (findTip tree path : seq) in false with
                                     FollowPath_ _ -> false
                                   | FindTip_ -> true
                               in
                               let rec undo_proofstep =
                                 fun
                                   (WinHist {proofhist = proofhist} as
                                      hist) ->
                                     (undo_step proofhist &~~
                                      (
                                         fSome <.> 
                                           ((fun ph ->
                                               withproofhist (hist) (ph))
                                              )))
                               in
                               while stillcomposite !there do
                                 match undo_proofstep !there with
                                   Some h -> there := h
                                 | _ ->
                                     showAlert
                                       ["can't backtrack to that point"];
                                     raise Matchinbacktrack_
                               done;
                               Some !there
                             with
                               Matchinbacktrack_ -> None)))
                      ))

        | "prune", [] ->
            (* Prune *)
            inside c
              (fun displaystate ->
                 showproof <.> 
                   ((fun here ->
                       tryLayout displaystate PruneCommand PrunePath here)
                      ))

        | "collapse", [] ->
            (* Hide/Show subproof *)
            inside c
              (fun displaystate ->
                 showproof <.> 
                   ((fun here ->
                       tryLayout displaystate HideShowCommand PrunePath
                         here)
                      ))

        | "hideroot", [] ->
            (* Hide conclusion *)
            inside c
              (fun displaystate ->
                 showproof <.> 
                   ((fun here ->
                       tryLayout displaystate HideRootCommand HitPath
                         here)
                      ))

        | "exposeparent", [] ->
            (* Show parent conclusion *)
            inside c
              (fun displaystate ->
                 showproof <.> 
                   ((fun here ->
                       tryLayout displaystate ExposeParentCommand HitPath
                         here)
                      ))

        | "hidecut", [] ->
            (* Hide cut hypothesis *)
            inside c
              (fun displaystate ->
                 showproof <.> 
                   ((fun here ->
                       tryLayout displaystate HideCutCommand PrunePath
                         here)
                      ))

        | "layout", _ ->
            (* Expand/Contract detail *)
            inside c
              (fun displaystate ->
                 showproof <.> 
                   ((fun here ->
                       tryLayout displaystate ExpandContractCommand
                         LayoutPath here)
                     ))

        | "addnewconjecture", panel :: text ->
            begin try
              let panel = namefrom panel in
              let name = addnewconjecture panel text in
              Japeserver.selectpanelentry (namestring panel) (namestring name);
              default
            with
              AddConjecture_ -> default
            end

        | "prove", comm ->
            let name = namefrom (respace comm) in
            if match proofnamed name with
                 Some (proved, tree, provisos, givens, disproved, disproofopt) ->
                   screenquery
                     [namestring name;
                      if proved then " is already a theorem"
                      else " is already disproved"; (* WRONG *)
                      ".\nDo you want to start a new proof?"]
                     "Yes" "No" 1
               | None -> true
            then
              doproof env mbs name pinfs
            else default


        | "reset", [] ->
            if askResettheory false then doResettheory () else default

        | "reset;reload", [] ->
            if askResettheory true then
              match
                Japeserver.readFileName "Load new theory from:"
                  Japeserver.toplevelfiletype
              with
                Some s -> processcommand (doResettheory ()) ["use"; s]
              | None -> default
            else default

        | "profile", ["on"] ->
            Japeenv.set (env, namefrom "profiling", parseTactic "true");
            (* achieves profileOn(), I hope *)
            default

        | "profile", ["off"] ->
            Japeenv.set (env, namefrom "profiling", parseTactic "false");
            (* achieves profileOff(), I hope *)
            default

        | "profile", ["reset"] -> profileReset (); default

        | "profile", ["report"] ->
            begin match
              Japeserver.writeFileName "Profile output to:"
                Japeserver.dbugfiletype
            with
              Some s -> let _ = (writetonamedfile profileReport s : bool) in ()
            | None -> ()
            end;
            default

        | "profile", ["report"; filename] ->
            let _ = (writetonamedfile profileReport filename : bool) in default

        | "fonts_reset", [] ->
            (* needs to do disproof as well *)
            Japeserver.resetcache ();
            let pinfs =
              match pinfs with
                fg :: bgs ->
                  fg ::
                    (if Japeserver.canbackgroundfocus then
                       let rec f pinf =
                         let hist = proofinfo_hist pinf in
                         let proof = winhist_proofnow hist in
                         let disproof = winhist_disproofnow hist in
                         Japeserver.setbackgroundfocus
                           (proofinfo_proofnum pinf);
                         withhist
                           (withdisplaystate
                              (withneedsrefresh (pinf) (false))
                               (let r = showState (proofinfo_displaystate pinf) proof !autoselect in 
                                setGivens (proofstate_givens proof); r))
                            (reparentprovisos hist)
                       in
                         (let r = f <* bgs in Japeserver.setforegroundfocus (); r)
                     else
                         (fun pinf -> withneedsrefresh (pinf) (true)) <* bgs)
              | _ -> pinfs
            in
            env, mbs, ShowBoth, pinfs

        | "showfile", [filename] ->
            (* here cos I can't work out how to get round the NOSELCOMMAND trap. RB 14/ii/94 *)
            Japeserver.showfile (disQuote filename); default

        | "saveproofs", [w] ->
            let newfile = w = "true" in
            let pinfs' =
                 (fun pinf ->
                    withhist
                      (pinf) (withchanged (proofinfo_hist pinf) (false))) <*
                 pinfs
            in
            if newfile && (* Save As... *) saveable () || not newfile && (* Save *) needssaving ()
            then
              saveproofs newfile
            else showAlert ["Nothing to save!"];
            env, mbs, DontShow, pinfs'

        | "quit", [] ->
            if needssaving () then
              askSave "quitting"
                (fun () -> Japeserver.quit (); raise QuitJape)
                (fun () -> Japeserver.quit (); raise QuitJape)
                (fun () -> Japeserver.dontquit (); default) ()
            else begin Japeserver.quit (); raise QuitJape end

        | "setfocus", [nstring] ->
            let
              (Pinf
                 {title = title;
                  proofnum = proofnum;
                  displayvarsvals = displayvarsvals;
                  needsrefresh = needsrefresh;
                  displaystate = displaystate;
                  hist = hist;
                  fromstore = fromstore})
              =
              List.hd pinfs
            in
            let pinfs =
              Pinf {title = title; proofnum = proofnum; 
                    displayvarsvals = recorddisplayvars env; (* just in case *)
                    needsrefresh = needsrefresh; displaystate = displaystate;
                    hist = hist; fromstore = fromstore} ::
                List.tl pinfs
            in
            let (fg, bgs) = findproof pinfs nstring in
            newfocus (env, mbs, DontShow, fg :: bgs)
        
        | "setfonts", fontnames ->
            (* let _ = consolereport ["font names now "; bracketedliststring (fun s -> s) "; " fontnames] in *)
            (Japeserver.setFontNames (List.map Stringfuns.disQuote fontnames); default)
            
        | "closeproof", [nstring] ->
            (* when closing proofs we must consider proof hist AND disproof hist *)
            let n = atoi nstring in
            let
              (Pinf {hist = hist; title = t, _; fromstore = fromstore},
               pinfs')
              =
              findproof pinfs nstring
            in
            let proofhist = winhist_proofhist hist in
            let disproofhist = winhist_disproofhist hist in
            let proofstate = hist_now proofhist in
            let disproof = try__ hist_now disproofhist in
            let closed () = newfocus (env, mbs, DontShow, pinfs') in
            let closeOK () = Japeserver.closeproof n; closed() in
            (* a proof can be finished in no steps.  But if it comes from store, we don't
               re-record it unless you've developed it.
             *)
            if finished proofstate disproof then
              if  not fromstore ||
                  (not (null (hist_pasts proofhist)) && isproven proofstate) ||
		  (match disproofhist with 
		     Some d -> not (null (hist_pasts d)) && disproof_finished disproof 
		   | None   -> false) 
              then
                Alert.askDangerously
                  ("The proof of " ^ namestring t ^ " is complete - do you want to record it?")
                  ("Record", (fun () -> if endproof n t proofstate disproof 
                                        then closed() else default))
                  ("Don't record", closeOK)
                  (fun () -> default) 
                  ()
              else closeOK()
            else 
              (* hist doesn't normally matter, since we don't store it ... if you are looking at an
                 undeveloped proof, we can just throw it away.
               *)
            if not fromstore &&
              (not (isTip (proofstate_tree proofstate)) || not (disproof_minimal disproof))
            then (* there is something to save *)
              if screenquery ["Abandon proof of "; namestring t; "?"] "Abandon" "Cancel" 1
              then closeOK() else default
            else (* nothing to save *)
              closeOK()

        | "createdbugfile", [] ->
            (match Japeserver.writeFileName "Write diagnostic output to:" Japeserver.dbugfiletype with
	       Some s -> let file = disQuote s in
			 (try createdbugfile file with
			   exn -> showAlert ["can't create file"; file; " ("; Printexc.to_string exn; ")"]);
			 default
	     | None  -> default
            )

        | "closedbugfile", [] -> closedbugfile (); default

        | _ -> cannotprocesscommand c; default
  
  (*processcommand*)
           
  and cannotprocesscommand command =
    showAlert
      ["Dialogue.processcommand cannot process "; respace command]
  in
  let rec domb (var, notify) =
    let setting =
      try _The (Japeenv.(<@>) env var) with
        None_ ->
          raise
            (Catastrophe_
               ["domb error: variable "; namestring var;
                " in mbs but not in env"])
    in
    match (!mbcache <@> var) with
      Some r ->
        if !r = setting then ()
        else begin notify (termstring setting, true); r := setting end
    | None ->
        mbcache := (!mbcache ++ (var |-> ref setting));
        notify (termstring setting, true)
  in
  (* for the time being, until there is effective proof/disproof focus, we have too many buttons *)
  let rec administer displayopt =
    List.iter Button.enable
      [UndoProofbutton   , proofundoable ();
       RedoProofbutton   , proofredoable ();
       UndoDisproofbutton, disproofundoable ();
       RedoDisproofbutton, disproofredoable ();
       Finishedbutton    , finishable (); 
       Resetbutton       , resetable ();
       Savebutton        , needssaving (); 
       SaveAsbutton      , saveable ();
       Disprovebutton    , not (null pinfs) && hasforcedefs ()];
    List.iter domb mbs;
    (* this is lazy -- see comment above *)
    begin try
      Japeserver.settextselectionmode
        (termstring
           (_The (Japeenv.(<@>) env (namefrom "textselectionmode"))))
    with
      None_ ->
        raise (Catastrophe_ ["textselectionmode not in environment"])
    end;
    (* explicit block so that profiler gives more helpful information *)
    let command = getCommand displayopt in
    setComment [];
    (* consolereport (("in administer; command is " :: commandstring command) @ [ "; pinfs are ",
            bracketedliststring (string_of_int:int->string) ","
              ((fn Pinf{proofnum,...}=>proofnum)  <* pinfs)
       ]);
     *)

    match command with
      TextCommand words -> processcommand thisstate words
    | HitCommand (tree, hit, sel) ->
        match hit, sel with
          FormulaHit fh, FormulaSel (path, concopt, hyps, _, _, _ as fsel) ->
            (* find the command given the click (which tells us DClickHyp or DClickConc) and the selection
             * (which may perhaps have to be augmented by a default hypothesis or conclusion selection).
             *)
            let (Seq (st, hs, gs)) =
              try sequent (followPath tree path) with
                _ ->
                  raise
                    (Catastrophe_
                       ["can't follow selpath (commands): ";
                        selstring fmtpathstring sel])
            in
            let rec trymatch sense p =
              Doubleclick.matchdoubleclick sense (mkSeq p)
            in
            let rec hypword hs ss =
              match hs with
                [h] -> "hypothesis " :: elementstring h :: ss
              | _ ->
                  "hypotheses " ::
                    liststring2 elementstring ", " "and " hs :: ss
            in
            let rec bang f = "double-click is not defined on " :: f () in
            let rec comm sense ps ss =
                (findfirst (trymatch sense) ps |~~
                 (fun _ -> showAlert (bang ss); None))
            in
            let (sense, (seqs, errf)) =
              match fh with
                ConcHit (_, (el, _)) ->
                  DClickConc,
                  (match hyps with
                     [] ->
                       begin match explodeCollection hs with
                         [h] ->
                           [st, [], [el]; st, [h], [el]],
                           (fun _ ->
                              ["conclusion "; elementstring el; " with no selected hypothesis, or with hypothesis ";
                               elementstring h])
                       | _ ->
                           [st, [], [el]],
                           (fun _ ->
                              ["conclusion "; elementstring el;
                               " with no selected hypothesis"])
                       end
                   | _ ->
                       [st, hyps, [el]],
                       (fun _ ->
                          "conclusion " :: elementstring el :: " and " ::
                            hypword hyps []))
              | HypHit (_, el) ->
                  DClickHyp,
                  (match concopt with
                     None ->
                       begin match explodeCollection gs with
                         [g] ->
                           [st, hyps, []; st, hyps, [g]],
                           (fun _ ->
                              hypword hyps
                                [" with no selected conclusion, or with conclusion ";
                                 elementstring g])
                       | _ ->
                           [st, hyps, []],
                           (fun _ ->
                              hypword hyps
                                [" with no selected conclusion"])
                       end
                   | Some (c, _) ->
                       [st, hyps, [c]],
                       (fun _ ->
                          hypword hyps
                            [" with selected conclusion ";
                             elementstring c]))
              | _ ->
                  raise
                    (Catastrophe_ ["ambiguous doubleclick (commands)"])
            in
            begin match comm sense seqs errf with
              Some c ->
                let c' = [tacticstring c] in
                inside c'
                  (fun displaystate ->
                     showproof <.> 
                       ((fun here ->
                           evolve_proof displaystate env
                             (pointToSequent displaystate env c' fsel)
                             here)
                          ))
            | None -> default
            end

        | _, ReasonSel _ ->
            inside []
              (fun displaystate ->
                 showproof <.> 
                   ((fun here ->
                       tryLayout displaystate ExpandContractCommand
                         LayoutPath here)
                      ))

        | _ ->
            raise
              (Catastrophe_
                 ["funny hit (commands): "; commandstring command])
  in
  (* assignment to displayvars can force redisplay *)
  let nextargs =
    try
      match pinfs with
        (Pinf
           {displayvarsvals = displayvarsvals;
            displaystate = displaystate;
            hist = hist} as pinf) :: rest ->
          let rec doShowProof state show =
            env, mbs, show,
            withhist
              (withdisplaystate
                 (withneedsrefresh
                    (withdisplayvarsvals (pinf) (recorddisplayvars env))
                     (false))
                  (state))
               (reparentprovisos hist) ::
              rest
          in
          let (Proofstate {tree = tree; cxt = cxt; goal = goal} as proofstate) =
                      winhist_proofnow hist
          in
          let disproof = winhist_disproofnow hist in
          (* how does disproof fit into this?  I think it doesn't *)
          if mustredisplay env displayvarsvals then
            let target =
                ( (findSelection displaystate &~~ selpath) |~~
                 (fun () -> goal))
            in
            doShowProof
              (let r = showFocussedProof target cxt tree !autoselect in
               setProvisos cxt; r)
              ShowDisproof
          else
            begin match showit with
              ShowProof ->
                doShowProof
                  (showState displaystate proofstate !autoselect) DontShow
            | ShowDisproof ->
                begin match disproof with
                  Some d -> Disproof.showdisproof d
                | None -> Disproof.cleardisproof ()
                end;
                env, mbs, DontShow, pinfs
            | ShowBoth ->
                doShowProof
                  (showState displaystate proofstate !autoselect)
                  ShowDisproof
            | DontShow -> administer (Some displaystate)
            end
      | [] -> administer None
    with
      Catastrophe_ ss ->
        showAlert ("catastrophic engine error: " :: ss);
        env, mbs, DontShow, pinfs
    | CompileThing_ ss -> showAlert ss; env, mbs, DontShow, pinfs
    | Tacastrophe_ ss ->
        showAlert
          ("Tacastrophe_ in commands (shouldn't happen)!! -- " :: ss);
        env, mbs, DontShow, pinfs
    | Japeserver.DeadGUI_ as exn -> raise exn
    | QuitJape as exn -> raise exn
    | exn ->
        showAlert
          ["unexpected exception "; Printexc.to_string exn; " in commands"];
        env, mbs, DontShow, pinfs
  in
  commands nextargs

and start () =
  (* Japeserver.quit ();
  Interaction.terminateGUI ();
  cleanup (); *)
  initButtons ();
  main (defaultenv (), [], []) (Array.to_list Sys.argv, []); (* empty environment ... ?? *)
  ()

