(* $Id$ *)

module type Dialogue =
  sig
    val reset : unit -> unit
    val cleanup : unit -> unit
    val save : string -> unit
    (* shouldn't that be 'a? *)
    val start : unit -> unit
  end

(* $Id$ *)

(* This is decaying. Behaviour of the command loop, except when recently tested, is becoming 
 * unpredictably bizarre.  This is perhaps due to the separation of concerns between interaction
 * (which deals with some commands from the interface, and translates the rest) and the command
 * loop.  pointToSequent is where a good deal of the confusion centres ...
 * RB 6/vii/2000
 *)
module
  Dialogue
  (AAA :
    sig
      module alert : Alert
      module answer : Answer
      module button : Button
      module context : Context
      module disproof : Disproof
      module doubleclick : DoubleClick
      module env : env
      module hit : Hit
      module interaction : Interaction
      module japeenv : Japeenv
      module japeserver : Japeserver
      module listfuns : Listfuns
      module mappingfuns : Mappingfuns
      module name : Name
      module paragraphfuns : Paragraphfuns
      module proofstate : Proofstate
      module proofstore : Proofstore
      module prooftree : Prooftree
      module rewrite : Rewrite
      module RUN : RUN
      module runproof : Runproof
      module sequent :
        sig include Sequenttype include Sequentreset include Sequent end
      module stringfuns : Stringfuns
      module tacticfuns : Tacticfuns
      module thing : Thing
      module treeformat : TreeFormat
      exception AtoI_
      exception Catastrophe_ of string list
      exception ParseError_ of string list
      exception Tacastrophe_ of string list
      exception UnSOME_
      exception Use_
      exception Verifyproviso_ of thing.proviso
      val resetallcachesandvariables : unit -> unit
      val andthenr : 'a option * ('a -> 'b option) -> 'b option
      val atoi : string -> int
      val autoselect : bool ref
      val clearbindingdirectives : unit -> unit
      val closedbugfile : unit -> unit
      val consolereport : string list -> unit
      val consolequery : string list * string * string * int -> bool
      val createdbugfile : string -> unit
      val defaultenv : unit -> japeenv.japeenv
      val displayvars : japeenv.name list
      (* val draganddropmapping 
                         : context.cxt -> (sequent.element*sequent.element) list
       *)
      val elementstring : hit.element -> string
      val explodeCollection : sequent.term -> sequent.element list
      val facts : context.visproviso list -> context.cxt -> disproof.facts
      val findfirst : ('a -> 'b option) -> 'a list -> 'b option
      val get_oplist : unit -> string list
      val givenMenuTactic : string ref
      val initGUI : unit -> unit
      val InProgress : proofstore.proofstage
      val isCutStep :
        prooftree.treeformat prooftree.prooftree -> prooftree.fmtpath -> bool
      val mustredisplay : japeenv.japeenv -> string list -> bool
      val mkvisproviso : bool * context.proviso -> context.visproviso
      val observe : string list -> unit
      val optionstring : ('a -> string) -> 'a option -> string
      val ortryr : 'a option * (unit -> 'a option) -> 'a option
      val string2paragraph :
        (string list -> unit) ->
          (string list * string * string * int -> bool) -> string ->
          paragraphfuns.paragraph
      val parseCurriedArgList : string -> thing.term list
      val parseTactic : string -> thing.term
      val parseTerm : string -> thing.term
      val parseTermCOMMAList : string -> thing.term list
      val profileReport : Pervasives.out_channel -> unit
      val profileReset : unit -> unit
      val proofsdone : bool ref
      val provisoactual : context.visproviso -> context.proviso
      val provisostring : thing.proviso -> string
      val provisovisible : context.visproviso -> bool
      val sameresource : hit.element * hit.element -> bool
      val seektipselection : bool ref
      val showInputError : (string list -> unit) -> string list -> unit
      val tacticstring : tacticfuns.tactic -> string
      val termstring : thing.term -> string
      val try__ : ('a -> 'b) -> 'a option -> 'b option
      val unSOME : 'a option -> 'a
      val uncurry2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
      val verifyprovisos : context.cxt -> context.cxt
      val Title : string
      val Version : string
      
    end)
  :
  Dialogue =
  struct
    open AAA
    open answer
    open button
    open context
    open disproof
    open doubleclick
    open hit
    open interaction
    open listfuns
    open mappingfuns
    open name
    open proofstate
    open prooftree
    open prooftree.fmtprooftree
    open proofstore
    open rewrite
    open runproof
    open sequent
    open stringfuns
    open tacticfuns
    open thing
    open treeformat
    open IO
    
    let rec disproof_finished =
      function
        Some state ->
          let res =
            disproofstate_countermodel state && disproofstate_conclusive state
          in
          (* consolereport ["disproof_finished ", disproofstatestring state, " => ", string_of_int res]; *)
          res
      | None -> false
    type 'a hist = Hist of < now : 'a; pasts : 'a list; futures : 'a list >
    type winhist =
        WinHist of
          < changed : bool; proofhist : proofstate hist;
            disproofhist : disproofstate hist option >
    let rec winhist_changed = fun (WinHist {changed = changed}) -> changed
    let rec winhist_proofhist =
      fun (WinHist {proofhist = proofhist}) -> proofhist
    let rec winhist_disproofhist =
      fun (WinHist {disproofhist = disproofhist}) -> disproofhist
    let rec winhist_proofnow =
      fun (WinHist {proofhist = Hist {now = now}}) -> now
    let rec winhist_disproofnow =
      function
        WinHist {disproofhist = Some (Hist {now = now})} -> Some now
      | _ -> None
    let rec hist_now = fun (Hist {now = now}) -> now
    let rec hist_pasts = fun (Hist {pasts = pasts}) -> pasts
    let rec hist_futures = fun (Hist {futures = futures}) -> futures
    
    let rec withchanged =
      fun
        (WinHist {proofhist = proofhist; disproofhist = disproofhist},
         changed) ->
        WinHist
          (let module M =
             struct
               class a =
                 object
                   val changed = changed
                   val proofhist = proofhist
                   val disproofhist = disproofhist
                   method changed = changed
                   method proofhist = proofhist
                   method disproofhist = disproofhist
                 end
             end
           in
           new M.a)
    let rec withproofhist =
      fun
        (WinHist {changed = changed; disproofhist = disproofhist},
         proofhist) ->
        WinHist
          (let module M =
             struct
               class a =
                 object
                   val changed = changed
                   val proofhist = proofhist
                   val disproofhist = disproofhist
                   method changed = changed
                   method proofhist = proofhist
                   method disproofhist = disproofhist
                 end
             end
           in
           new M.a)
    let rec withdisproofhist =
      fun
        (WinHist {changed = changed; proofhist = proofhist}, disproofhist) ->
        WinHist
          (let module M =
             struct
               class a =
                 object
                   val changed = changed
                   val proofhist = proofhist
                   val disproofhist = disproofhist
                   method changed = changed
                   method proofhist = proofhist
                   method disproofhist = disproofhist
                 end
             end
           in
           new M.a)
    
    let rec new_hist now =
      Hist
        (let module M =
           struct
             class a =
               object
                 val now = now
                 val pasts = []
                 val futures = []
                 method now = now
                 method pasts = pasts
                 method futures = futures
               end
           end
         in
         new M.a)
    let rec withnow =
      fun (Hist {pasts = pasts; futures = futures}, now) ->
        Hist
          (let module M =
             struct
               class a =
                 object
                   val now = now
                   val pasts = pasts
                   val futures = futures
                   method now = now
                   method pasts = pasts
                   method futures = futures
                 end
             end
           in
           new M.a)
    let rec withpasts =
      fun (Hist {now = now; futures = futures}, pasts) ->
        Hist
          (let module M =
             struct
               class a =
                 object
                   val now = now
                   val pasts = pasts
                   val futures = futures
                   method now = now
                   method pasts = pasts
                   method futures = futures
                 end
             end
           in
           new M.a)
    let rec withfutures =
      fun (Hist {now = now; pasts = pasts}, futures) ->
        Hist
          (let module M =
             struct
               class a =
                 object
                   val now = now
                   val pasts = pasts
                   val futures = futures
                   method now = now
                   method pasts = pasts
                   method futures = futures
                 end
             end
           in
           new M.a)
    (* there are several things we can do with a history *)
    
    let rec forward_step =
      fun (Hist {now = now; pasts = pasts}) now' ->
        Hist
          (let module M =
             struct
               class a =
                 object
                   val now = now'
                   val pasts = now :: pasts
                   val futures = []
                   method now = now
                   method pasts = pasts
                   method futures = futures
                 end
             end
           in
           new M.a)
    let rec insert_step =
      fun (Hist {now = now; pasts = pasts; futures = futures}) now' ->
        Hist
          (let module M =
             struct
               class a =
                 object
                   val now = now'
                   val pasts = pasts
                   val futures = now :: futures
                   method now = now
                   method pasts = pasts
                   method futures = futures
                 end
             end
           in
           new M.a)
    let rec append_step =
      fun (Hist {now = now; pasts = pasts; futures = futures}) now' ->
        Hist
          (let module M =
             struct
               class a =
                 object
                   val now = now'
                   val pasts = now :: pasts
                   val futures = futures
                   method now = now
                   method pasts = pasts
                   method futures = futures
                 end
             end
           in
           new M.a)
    let rec redo_step =
      function
        Hist {now = now; pasts = pasts; futures = now' :: futures} ->
          Some
            (Hist
               (let module M =
                  struct
                    class a =
                      object
                        val now = now'
                        val pasts = now :: pasts
                        val futures = futures
                        method now = now
                        method pasts = pasts
                        method futures = futures
                      end
                  end
                in
                new M.a))
      | _ -> None
    let rec undo_step =
      function
        Hist {now = now; pasts = now' :: pasts; futures = futures} ->
          Some
            (Hist
               (let module M =
                  struct
                    class a =
                      object
                        val now = now'
                        val pasts = pasts
                        val futures = now :: futures
                        method now = now
                        method pasts = pasts
                        method futures = futures
                      end
                  end
                in
                new M.a))
      | _ -> None
    let rec reparentprovisos hist =
      let phist = winhist_proofhist hist in
      let proof = hist_now phist in
      let cxt = selfparentprovisos (proofstate_cxt proof) in
      withproofhist (hist, withnow (phist, withcxt (proof, cxt)))
    type proofinfo =
        Pinf of
          < title : name * int; proofnum : int; displayvarsvals : string list;
            needsrefresh : bool; displaystate : displaystate; hist : winhist;
            fromstore : bool >
    let rec proofinfo_title = fun (Pinf {title = title}) -> title
    let rec proofinfo_proofnum = fun (Pinf {proofnum = proofnum}) -> proofnum
    let rec proofinfo_displayvarsvals =
      fun (Pinf {displayvarsvals = displayvarsvals}) -> displayvarsvals
    let rec proofinfo_needsrefresh =
      fun (Pinf {needsrefresh = needsrefresh}) -> needsrefresh
    let rec proofinfo_displaystate =
      fun (Pinf {displaystate = displaystate}) -> displaystate
    let rec proofinfo_hist = fun (Pinf {hist = hist}) -> hist
    let rec proofinfo_fromstore =
      fun (Pinf {fromstore = fromstore}) -> fromstore
    
    let rec withtitle =
      fun
        (Pinf
           {proofnum = proofnum;
            displayvarsvals = displayvarsvals;
            needsrefresh = needsrefresh;
            displaystate = displaystate;
            hist = hist;
            fromstore = fromstore}, title) ->
        Pinf
          (let module M =
             struct
               class a =
                 object
                   val title = title
                   val proofnum = proofnum
                   val displayvarsvals = displayvarsvals
                   val needsrefresh = needsrefresh
                   val displaystate = displaystate
                   val hist = hist
                   val fromstore = fromstore
                   method title = title
                   method proofnum = proofnum
                   method displayvarsvals = displayvarsvals
                   method needsrefresh = needsrefresh
                   method displaystate = displaystate
                   method hist = hist
                   method fromstore = fromstore
                 end
             end
           in
           new M.a)
    let rec withproofnum =
      fun
        (Pinf
           {title = title;
            displayvarsvals = displayvarsvals;
            needsrefresh = needsrefresh;
            displaystate = displaystate;
            hist = hist;
            fromstore = fromstore}, proofnum) ->
        Pinf
          (let module M =
             struct
               class a =
                 object
                   val title = title
                   val proofnum = proofnum
                   val displayvarsvals = displayvarsvals
                   val needsrefresh = needsrefresh
                   val displaystate = displaystate
                   val hist = hist
                   val fromstore = fromstore
                   method title = title
                   method proofnum = proofnum
                   method displayvarsvals = displayvarsvals
                   method needsrefresh = needsrefresh
                   method displaystate = displaystate
                   method hist = hist
                   method fromstore = fromstore
                 end
             end
           in
           new M.a)
    let rec withdisplayvarsvals =
      fun
        (Pinf
           {title = title;
            proofnum = proofnum;
            needsrefresh = needsrefresh;
            displaystate = displaystate;
            hist = hist;
            fromstore = fromstore}, displayvarsvals) ->
        Pinf
          (let module M =
             struct
               class a =
                 object
                   val title = title
                   val proofnum = proofnum
                   val displayvarsvals = displayvarsvals
                   val needsrefresh = needsrefresh
                   val displaystate = displaystate
                   val hist = hist
                   val fromstore = fromstore
                   method title = title
                   method proofnum = proofnum
                   method displayvarsvals = displayvarsvals
                   method needsrefresh = needsrefresh
                   method displaystate = displaystate
                   method hist = hist
                   method fromstore = fromstore
                 end
             end
           in
           new M.a)
    let rec withneedsrefresh =
      fun
        (Pinf
           {title = title;
            proofnum = proofnum;
            displayvarsvals = displayvarsvals;
            displaystate = displaystate;
            hist = hist;
            fromstore = fromstore}, needsrefresh) ->
        Pinf
          (let module M =
             struct
               class a =
                 object
                   val title = title
                   val proofnum = proofnum
                   val displayvarsvals = displayvarsvals
                   val needsrefresh = needsrefresh
                   val displaystate = displaystate
                   val hist = hist
                   val fromstore = fromstore
                   method title = title
                   method proofnum = proofnum
                   method displayvarsvals = displayvarsvals
                   method needsrefresh = needsrefresh
                   method displaystate = displaystate
                   method hist = hist
                   method fromstore = fromstore
                 end
             end
           in
           new M.a)
    let rec withdisplaystate =
      fun
        (Pinf
           {title = title;
            proofnum = proofnum;
            displayvarsvals = displayvarsvals;
            needsrefresh = needsrefresh;
            hist = hist;
            fromstore = fromstore}, displaystate) ->
        Pinf
          (let module M =
             struct
               class a =
                 object
                   val title = title
                   val proofnum = proofnum
                   val displayvarsvals = displayvarsvals
                   val needsrefresh = needsrefresh
                   val displaystate = displaystate
                   val hist = hist
                   val fromstore = fromstore
                   method title = title
                   method proofnum = proofnum
                   method displayvarsvals = displayvarsvals
                   method needsrefresh = needsrefresh
                   method displaystate = displaystate
                   method hist = hist
                   method fromstore = fromstore
                 end
             end
           in
           new M.a)
    let rec withhist =
      fun
        (Pinf
           {title = title;
            proofnum = proofnum;
            displayvarsvals = displayvarsvals;
            needsrefresh = needsrefresh;
            displaystate = displaystate;
            fromstore = fromstore}, hist) ->
        Pinf
          (let module M =
             struct
               class a =
                 object
                   val title = title
                   val proofnum = proofnum
                   val displayvarsvals = displayvarsvals
                   val needsrefresh = needsrefresh
                   val displaystate = displaystate
                   val hist = hist
                   val fromstore = fromstore
                   method title = title
                   method proofnum = proofnum
                   method displayvarsvals = displayvarsvals
                   method needsrefresh = needsrefresh
                   method displaystate = displaystate
                   method hist = hist
                   method fromstore = fromstore
                 end
             end
           in
           new M.a)
    let rec withfromstore =
      fun
        (Pinf
           {title = title;
            proofnum = proofnum;
            displayvarsvals = displayvarsvals;
            needsrefresh = needsrefresh;
            displaystate = displaystate;
            hist = hist}, fromstore) ->
        Pinf
          (let module M =
             struct
               class a =
                 object
                   val title = title
                   val proofnum = proofnum
                   val displayvarsvals = displayvarsvals
                   val needsrefresh = needsrefresh
                   val displaystate = displaystate
                   val hist = hist
                   val fromstore = fromstore
                   method title = title
                   method proofnum = proofnum
                   method displayvarsvals = displayvarsvals
                   method needsrefresh = needsrefresh
                   method displaystate = displaystate
                   method hist = hist
                   method fromstore = fromstore
                 end
             end
           in
           new M.a)
    type showstate = ShowProof | ShowDisproof | ShowBoth | DontShow
    let setComment = alert.setComment <*> implode
    let showAlert =
      alert.showAlert alert.defaultseverity_alert <*> implode
    let rec screenquery ss y n def =
      let bs = [y, true; n, false] in
      alert.ask (alert.defaultseverity bs) (implode ss) bs def
    let rec uncurried_screenquery (ss, y, n, def) = screenquery ss y n def
    let savefilename : string option ref = ref None
    let mbcache : (name, term ref) mappingfuns.mapping ref =
      ref mappingfuns.empty
    let rec apply_cleanup () = tacticfuns.selections := None
    exception applycommand_ of proofstate option
    let rec docommand displaystate env target comm =
      fun (Proofstate {cxt = cxt; tree = tree; givens = givens} as state) ->
        let r =
		  (let state =
			 withroot
			   (withtarget (withgoal (state, Some target), Some target), None)
		   in
		   tacticfuns.applyLiteralTactic (Some displaystate) env (respace comm)
			 state)
        in apply_cleanup (); r
    let rec badsel ss = showAlert ss; raise (applycommand_ None)
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
            tacticfuns.selections :=
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
                                             \conclusions to which it is relevant."]
                  | _ ->
                      badsel
                        ["there is more than one unproved conclusion which \
                                              \corresponds to that hypothesis - you must \
                                              \select one of them as well as the hypothesis."]
                  end
              | Some _ -> badsel ["that conclusion is already proved."]
          else doit target
        with
          applycommand_ sopt -> apply_cleanup (); sopt
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
                                      \Please select the one you want to work on."]
            | true, [] ->
                badsel
                  [if done__ () then "The proof is finished!"
                   else "There are no remaining unproved conclusions!"]
            | _ -> rootPath tree
          in
          tacticfuns.selections :=
            begin match textselopt with
              Some (proofsels, givensels) ->
                (* this should be in interaction.sml *)
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
          applycommand_ sopt -> apply_cleanup (); sopt
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
             (try siblingPath tree path true; false with
                _ -> true)
          then
            Some parentpath
          else None
      | _ -> None
    type layoutcommand =
        BacktrackCommand
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
			getfmt (fun () -> "can't hide subproofs there") tree path &~~
			(function tfk, RotatingFormat (i, nfs) ->
			   (findfirst
				  (fun (i, nf) -> if nf = foldstuff then Some i else None)
				  (numbered nfs) &~~
				(fun i' ->
				   Some (tfk, RotatingFormat ((if i = i' then i + 1 else i'), nfs)))) 
			   |~~
			   (fun _ -> Some (tfk, RotatingFormat (0, foldstuff :: nfs)))
			 | tfk, DefaultFormat -> Some (tfk, defaultfolded))) 
			 &~~
			 (fun fmt' ->
				Some (false, withtree
					   (state, set_prooftree_fmt tree path (TreeFormat fmt')))))
        | ExpandContractCommand ->
            getfmt
			  (fun () -> raise (Catastrophe_ ["getLayout sees EXPAND/CONTRACT on a Tip!!!"]))
              tree path &~~
			(function  _, DefaultFormat as fmt ->
			   if null (subtrees (followPath tree path)) then
				  (showAlert ["no point double-clicking that -- it doesn't have any subproofs"];
				   None)
			   else Some fmt
			| fmt -> Some fmt)) &~~
			rotateFormat &~~
			(fun tf ->
			   Some (false, withtree
					 (state, set_prooftree_fmt tree path (TreeFormat tf)))))
        | HideRootCommand ->
            let rec hideit () =
              try
                let (TreeFormat (_, tff)) = get_prooftree_fmt tree path in
                Some
                  (false,
                   withtree
                     (state,
                      set_prooftree_fmt tree path
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
                         (state,
                          set_prooftree_fmt tree parentpath
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
                     (state,
                      set_prooftree_fmt tree cutpath
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
        doLayout c (winhist_proofnow hist)) &~~
		(function
		   false, proof' ->
			 let phist = winhist_proofhist hist in
			 Some (withproofhist (hist, withnow (phist, proof')))
		 | true, proof' ->
			 let phist = winhist_proofhist hist in
			 Some (withproofhist (hist, append_step phist proof'))))
    let rec recorddisplayvars env =
      try 
        (fun s -> termstring (unSOME (japeenv.at (env, s)))) <* displayvars
      with
        unSOME_ ->
          raise
            (Catastrophe_
               ["one or more of ";
                bracketedliststring namestring ", " displayvars;
                " isn't set!"])
    let rec setdisplayvars env vals =
      List.iter (fun (s, v) -> japeenv.set (env, s, parseTactic v))
        ((displayvars ||| vals))
    (* proofmove doesn't set changed *)
    let rec proofmove =
      fun (WinHist {proofhist = proofhist} as hist) proof' ->
        withproofhist (hist, forward_step proofhist proof')
    (* nor does disproofmove *)
    let rec disproofmove a1 a2 =
      match a1, a2 with
        (WinHist {disproofhist = Some d} as hist), disproof' ->
          withdisproofhist (hist, Some (forward_step d disproof'))
      | (WinHist {disproofhist = None} as hist), disproof' ->
          withdisproofhist
            (hist,
             Some
               (Hist
                  (let module M =
                     struct
                       class a =
                         object
                           val now = disproof'
                           val pasts = []
                           val futures = []
                           method now = now
                           method pasts = pasts
                           method futures = futures
                         end
                     end
                   in
                   new M.a)))
    (* this function the same as evolve except for non-application of AUTO tactics,
     * and the fact that it doesn't set changed.
     *)
    let rec tryForward f =
      fun (WinHist {proofhist = Hist {now = proof}} as hist) ->
        f proof &~~ (fSome <*> proofmove hist)
    let rec evolvewithexplanation explain displaystate env f =
      fun (WinHist {proofhist = Hist {now = proof}} as hist) ->
        match f proof with
          None -> explain (); None
        | Some proof' ->
            Some
              (withchanged
                 (proofmove hist
                    (rewriteproofstate
                       (autoTactics (Some displaystate) env
                          (proofstate.autorules ()) proof')),
                  true))
    let evolve = evolvewithexplanation (fun () -> ())
    let rec reset () =
      resetallcachesandvariables (); savefilename := None; mbcache := empty
    let rec cleanup () = ()
    let rec parseargs args =
      try parseTermCOMMAList args with
        ParseError_ _ -> []
    exception QuitJape
    (* interpretParasFrom includes its own unQuote, so no need for one here *)
    let doUse = paragraphfuns.interpretParasFrom
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
            let server = env.getenv (path ^ "server") "JAPESERVER" in
            let rec doargs =
              function
                [] -> [], []
              | "-" :: args -> [], args
              | "-tree" :: args ->
                  interaction.setdisplaystyle "tree"; doargs args
              | "-box" :: args ->
                  interaction.setdisplaystyle "box"; doargs args
              | "-proofs" :: name :: args ->
                  savefilename := Some name; doargs args
              | "" :: args -> doargs args
              | name :: args ->
                  if String.sub (name) (0) (1) = "-" then [], name :: args
                  else let (names, args) = doargs args in name :: names, args
            in
            let (names, args) = doargs args in
            consolereport [Title; Version; " ["; server; "]\n"];
            begin
              let (env, proofs, mbs) =
                try
                  doUse consolereport consolequery (env, proofs, mbs) names
                with
                  ParseError_ m -> showInputError consolereport m; raise Exit_
                | Use_ -> raise Exit_
              in
              startServer (server, server :: args);
              japeserver.sendVersion (Title ^ Version);
              initGUI ();
              reloadmenusandpanels proofstore.provedordisproved
                (get_oplist ());
              mbcache := empty;
              rundialogue env mbs proofs
            end;
            ()
          with
            Exit_ -> ()
          end
      | _, _ -> raise Matchinmain_
    and rundialogue env mbs proofs =
      try
        let rec dialogue () = startcommands env mbs proofs in
        (* open RUN OCaml no like *)
        let rec Int () = observe ["[Interrupted]"]; interruptTactic () in
        onInterrupt Int dialogue; abandonServer ()
      with
        QuitJape -> deadServer ["Jape finished\n"]
      | Catastrophe_ s -> deadServer ("exception Catastrophe_ " :: s)
      | exn ->
          deadServer
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
            japeserver.openproof (heading, num);
            Pinf
              (let module M =
                 struct
                   class a =
                     object
                       val title = name, index
                       val proofnum = num
                       val displayvarsvals = recorddisplayvars env
                       val needsrefresh = false
                       val displaystate =
                         let r = showFocussedProof goal cxt tree !autoselect in
                           begin
                             setGivens givens;
                             setProvisos cxt;
                             match disproof with
                               None -> ()
                             | Some d -> disproof.showdisproof d
                           end; r
                       val hist =
                         WinHist
                           (let module M =
                              struct
                                class a =
                                  object
                                    val changed = false
                                    val proofhist =
                                      new_hist (withcxt (state, state_cxt))
                                    val disproofhist = try__ new_hist disproof
                                    method changed = changed
                                    method proofhist = proofhist
                                    method disproofhist = disproofhist
                                  end
                              end
                            in
                            new M.a)
                       val fromstore = fromstore
                       method title = title
                       method proofnum = proofnum
                       method displayvarsvals = displayvarsvals
                       method needsrefresh = needsrefresh
                       method displaystate = displaystate
                       method hist = hist
                       method fromstore = fromstore
                     end
                 end
               in
               new M.a) ::
              pinfs
      in
      nj_fold f proofs pinfs
    and biggestproofnum name pinfs =
      nj_fold
        (fun (Pinf {proofnum = proofnum; title = t, index}, (n, i)) ->
           max proofnum (n),
           (if t = name then max index (i) else i))
        pinfs (0, 0)
    and endproof num name proved st dis =
      runproof.addproof showAlert uncurried_screenquery name proved st
        (disproofstate2model dis) &&
      begin
        japeserver.closeproof num;
        markproof proved (parseablenamestring name);
        true
      end
    and commands
      (env, mbs, (showit : showstate), (pinfs : proofinfo list) as
         thisstate) =
      let rec findproof pinfs nstring =
        let n = atoi nstring in
        extract (fun (Pinf {proofnum = proofnum}) -> n = proofnum) pinfs
      in
      let rec newfocus (env, mbs, showit, (pinfs : proofinfo list) as state) =
        match pinfs with
          Pinf fg :: bgs ->
            setdisplayvars env ((fun ttt -> ttt#displayvarsvals) fg);
            env, mbs,
            (if (fun ttt -> ttt#needsrefresh) fg then ShowBoth else DontShow),
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
                          "\n\nTrying to read it as a sequent gave the error Ô" :: (rs @ "Õ.\n\nTrying to read it as a line of Japeish gave the error Ô" :: (rs' @ ["Õ."]))));
                  raise AddConjecture_
        in
        paragraphfuns.interpret showAlert uncurried_screenquery [] []
          (env, [], [])
          (match parseablenamestring panel with
             "" -> para
           | p ->
               getpara
                 (((("CONJECTUREPANEL " ^ p) ^ " IS ") ^ text) ^ " END"));
        let name = paragraphfuns.conjecturename para in
        if namestring panel <> "" then
          japeserver.panelentry
            (namestring panel, namestring name, parseablenamestring name);
        name
      in
      let rec printproof path state =
        let st = rewriteproofstate state in
        try
          let s = open_out path in
          interaction.printState s st true; close_out s
        with
          Io err ->
            showAlert ["Cannot write file "; path; " ("; Io_explain err; ")"]
      in
      let rec writetonamedfile action filename =
        try
          let sfile = open_out (unQuote filename) in
          action sfile; close_out sfile; true
        with
          Io err ->
            showAlert
              ["Cannot write file "; filename; " ("; Io_explain err; ")"];
            false
      in
      let rec saveproofs newfile =
        let rec doit sfile =
          let rec pf ps = (provisoactual <* (provisovisible <| ps)) in
          let rec f =
            fun (Pinf {title = t, _; hist = hist}) ->
              let (Proofstate {cxt = cxt; tree = tree; givens = givens}) =
                winhist_proofnow hist
              in
              saveproof sfile t InProgress tree (pf (provisos cxt)) givens
                (disproofstate2model (winhist_disproofnow hist))
          in
          proofstore.saveproofs sfile; List.map f pinfs
        in
        match newfile, !savefilename with
          false, Some s -> writetonamedfile doit s; ()
        | _ ->
            match
              japeserver.writeFileName "Save proofs as:"
                japeserver.prooffiletype
            with
              Some s -> if writetonamedfile doit s then savefilename := Some s
            | None -> ()
      in
      let rec saveable () = not (null pinfs) || proofstore.saveable () in
      let rec needssaving () =
        List.exists
          (function
             Pinf {hist = WinHist {changed = true}} -> true
           | _ -> false)
          pinfs ||
        not (proofstore.saved ())
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
        alert.askDangerously
          (implode ["Save your proofs before "; action; "?"])
          ("Save", (fun () -> saveproofs false; y))
          ("Don't save", (fun () -> n)) (fun () -> cancel) ()
      in
      let rec askResettheory anyway =
        if resetable () then
          if needssaving () then
            askSave "erasing the current theory" true true false
          else
            anyway ||
            screenquery ["Erase the current theory?"] "Erase" "Cancel" 1
        else begin reset (); true end
      in
      let rec doResettheory () =
        List.iter
          (fun (Pinf {proofnum = proofnum}) -> japeserver.closeproof proofnum)
          pinfs;
        japeserver.cancelmenusandpanels ();
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
            env, mbs, showit, withhist (pinf, hist) :: pinfs
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
        | None -> None
      in
      let rec showdisproof =
        function
          Some hist -> Some (ShowDisproof, hist)
        | None -> None
      in
      let rec showboth =
        function
          Some hist -> Some (ShowBoth, hist)
        | None -> None
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
                [name, withgivens (withcxt (state, cxt'), givens), None] pinfs
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
        let rec disproofstateact act =
          inside c
            (fun displaystate ->
               showdisproof <*> 
                 ((function
                     WinHist {disproofhist = Some (Hist {now = d})} as hist ->
                       let proof = winhist_proofnow hist in
                       let cxt_now = proofstate_cxt proof in
                       let facts_now = facts (provisos cxt_now) cxt_now in
                         (act d &~~
                          (fun d' ->
                             Some
                               (disproofmove hist
                                  (evaldisproofstate facts_now
                                     (proofstate_tree proof) d'))))
                   | _ ->
                       raise
                         (Catastrophe_
                            ["disproof action when no disproof state"]))
                   ))
        in
        let rec disproofuniverseact act =
          disproofstateact
            (fun d ->
                 (act (disproofstate_universe d) &~~
                  (fun u -> Some (withdisproofuniverse (d, u)))))
        in
        let rec worldlabelact act cx cy s =
          disproofuniverseact
            (fun u -> act u (atoi cx, atoi cy) (parseTerm (unQuote s)))
        in
        match c with
          [] -> default
        | word :: words ->
            match lowercase word, words with
              "apply", comm ->
                let rec nosels tselopt =
                  inside c
                    (fun displaystate ->
                       showproof <*> 
                         ((fun hist ->
                             evolve displaystate env
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
                             showproof <*> 
                               ((fun here ->
                                   evolve displaystate env
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
                end
            | "applygiven", args ->
                processcommand thisstate
                  ("apply" ::
                     parseablenamestring (namefrom !givenMenuTactic) :: args)
            | "assign", name :: value ->
                begin try
                  let value = parseTactic (respace value) in
                  japeenv.set (env, namefrom name, value);
                  tacticfuns.resetcaches ();
                  default
                with
                  japeenv.OutOfRange_ s ->
                    showAlert
                      ["error in "; respace c;
                       " --  value assigned should be "; s];
                    default
                | japeenv.NotJapeVar_ ->
                    showAlert
                      ["error in "; respace c; " -- "; name;
                       " is not a Jape variable"];
                    default
                | japeenv.ReadOnly_ ->
                    showAlert
                      ["error in "; respace c; " -- "; "you can't assign to ";
                       name; " at this point"];
                    default
                | ParseError_ rs ->
                    showAlert (["can't parse "; respace value; " -- "] @ rs);
                    default
                end
            | "redo_disproof", [] ->
                inside c
                  (fun _ ->
                     showdisproof <*> 
                       ((fun hist ->
                             (winhist_disproofhist hist &~~ redo_step &~~
							  (fSome <*> 
								 ((fun dh ->
									 withdisproofhist (hist, Some dh))
									))) |~~
                              (fun _ ->
                                 showAlert ["nothing to redo!"]; None)))
                          )
            | "redo_proof", [] ->
                inside c
                  (fun _ ->
                     showproof <*> 
                       (fun hist ->
                                (redo_step (winhist_proofhist hist) &~~
                                 (
                                    fSome <*> 
                                      ((fun ph -> withproofhist (hist, ph))
                                        ))) |~~
                              (fun _ ->
                                 showAlert ["nothing to redo!"]; None))
                          )
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
                  match japeenv.at (env, namefrom name) with
                    Some t -> termstring t
                  | None -> ""
                in
                japeserver.showfile ((respace interfacecommand ^ " ") ^ str);
                (* This should be called -- "tellinterface" *)
                (* It's the way that an interface can do something
                   with the value of a jape variable
                *)
                default
            | "undo_disproof", [] ->
                inside c
                  (fun _ ->
                     showdisproof <*> 
					 (fun hist ->
						((winhist_disproofhist hist &~~ undo_step &~~
						  (fSome <*> (fun dh -> withdisproofhist (hist, Some dh)))) 
						 |~~
						 (fun _ -> showAlert ["no disproof steps to undo!"]; None))))
            | "undo_proof", [] ->
                inside c
                  (fun _ ->
                     showproof <*> 
					 (fun hist ->
						(undo_step (winhist_proofhist hist) &~~
						 (fSome <*> ((fun ph -> withproofhist (hist, ph))))) 
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
                  japeserver.emptymenusandpanels ();
                  reloadmenusandpanels proofstore.provedordisproved
                    (get_oplist ());
                  mbcache := empty;
                  newfocus (env, mbs, DontShow, addproofs false env ps pinfs)
                with
                  ParseError_ rs -> showAlert rs; default
                | Use_ -> default
                end
            | "version", [] -> showAlert [Title; Version]; default
            | "addworldlabel", [cx; cy; s] ->
                (* ********************* the disproof commands ************************* *)
                
                worldlabelact addworldlabel cx cy s
            | "deleteworldlabel", [cx; cy; s] ->
                worldlabelact deleteworldlabel cx cy s
            | "tileact", [s] ->
                disproofstateact
                  (fun d -> disproof.newtile d (parseTerm (unQuote s)))
            | "addworld", [px; py; cx; cy] ->
                disproofuniverseact
                  (fun u ->
                     disproof.addchild u (atoi px, atoi py)
                       (atoi cx, atoi cy))
            | "deleteworldlink", [fromx; fromy; tox; toy] ->
                disproofuniverseact
                  (fun u ->
                     disproof.deletelink u (atoi fromx, atoi fromy)
                       (atoi tox, atoi toy))
            | "deleteworld", [cx; cy] ->
                disproofstateact
                  (fun d -> disproof.deleteworld d (atoi cx, atoi cy))
            | "moveworld", [x; y; x'; y'] ->
                disproofstateact
                  (fun d ->
                     disproof.moveworld d (atoi x, atoi y) (atoi x', atoi y'))
            | "worldselect", cs ->
                disproofstateact
                  (fun d ->
                     let rec pair =
                       function
                         cx :: cy :: cs -> (atoi cx, atoi cy) :: pair cs
                       | [] -> []
                       | _ ->
                           raise
                             (Catastrophe_
                                ["bad command (odd number of arguments): worldselect ";
                                 bracketedliststring (fun s -> s) "," cs])
                     in
                     disproof.worldselect d (pair cs))
            | "disprove", [] ->
                inside c
                  (fun displaystate ->
                     showdisproof <*> 
                       ((fun hist ->
                           let proof = winhist_proofnow hist in
                           let cxt_now = proofstate_cxt proof in
                           let facts_now = facts (provisos cxt_now) cxt_now in
                           let rec process_disproof disproof' =
                             let rec doit () =
                               Some (disproofmove hist disproof')
                             in
                             let seq' = disproofstate_seq disproof' in
                             match winhist_disproofnow hist with
                               None -> doit ()
                             | Some state ->
                                 let seq = disproofstate_seq state in
                                 if eqseqs (seq, seq') then
                                   if isemptyworld
                                        (disproofstate_universe state)
                                   then
                                     begin
                                       showAlert
                                         ["You are already disproving ";
                                          seqstring seq];
                                       None
                                     end
                                   else if
                                     screenquery
                                       ["You are already disproving ";
                                        seqstring seq; " - do you want to wipe clean the world(s) you have built?"]
                                       "Wipe" "Cancel" 1
                                   then
                                     doit ()
                                   else None
                                 else if
                                   screenquery
                                     ["You are disproving "; seqstring seq;
                                      " - do you want to replace it with ";
                                      seqstring seq'; "?"]
                                     "Replace" "Cancel" 1
                                 then
                                   doit ()
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
            | "unify", stuff ->
                (* ******************* proof stuff ************************)
                
                begin try
                  inside c
                    (fun displaystate ->
                       let rec getsels =
                         function
                           x :: y :: zs, gs -> y :: getsels (zs, gs)
                         | _, gs -> gs
                       in
                       let sels =
                         match findSelection displaystate with
                           Some
                             (FormulaSel
                                (_, _, _, concsels, hypsels, givensels)) ->
                             nj_fold getsels [givensels]
                               (nj_fold getsels ((snd <* concsels))
                                  (nj_fold getsels ((snd <* hypsels))
                                     []))
                         | Some (TextSel (proofsels, givensels)) ->
                             nj_fold getsels [givensels]
                               (nj_fold getsels ((snd <* proofsels))
                                  [])
                         | _ -> []
                       in
                       let args =
                         try
                           (if null sels || null stuff then
                              parseCurriedArgList
                            else (fun t -> [t]) <*> parseTerm)
                             (respace stuff)
                         with
                           ParseError_ es ->
                             showAlert
                               ("cannot parse unify " :: respace stuff ::
                                  " -- " :: es);
                             raise Unify_
                       in
                       let rec getit s =
                         try parseTerm s with
                           ParseError_ es ->
                             showAlert
                               ("your selection " :: s ::
                                  " didn't parse -- " :: es);
                             raise Unify_
                       in
                       let selargs = (getit <* sels) in
                       if List.length args + List.length selargs < 2 then
                         begin
                           showAlert
                             ["Unify must be given at least two things to work with!"];
                           raise Unify_
                         end
                       else
                           showproof <*> 
                             ((fun here ->
                                 evolvewithexplanation
                                   (fun () -> showAlert (explain ""))
                                   displaystate env
                                   (forceUnify (args @ selargs)) here)
                               ))
                with
                  Unify_ -> default
                end
            | "saveengine", name :: _ ->
                outside c
                  (fun () -> saverunning env mbs (unQuote name); default)
            | "done", [] ->
                begin match pinfs with
                  [] -> showAlert ["Not in a proof"]; default
                | Pinf {title = t, _; proofnum = proofnum; hist = hist} ::
                  pinfs' ->
                    let proof = winhist_proofnow hist in
                    let disproof = winhist_disproofnow hist in
                    if finished proof disproof then
                      if endproof proofnum t (isproven proof) proof disproof
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
                | Some (_, tree, provisos, givens, disproofopt) ->
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
            | "backtrack", _ ->
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
                            japeserver.dragtargets (nj_fold List.concat (targets <* dragees) []);
                            default
                        end
                  )                  
                | ("dropcommand", []) =>
                     inside c
                       (fn displaystate =>
                         (fn here => evolvewithexplanation
                                     (fn () => showAlert(explain ""))
                                     displaystate env
                                     (doDropUnify (!droptarget) (!dropsource))
                                     here
                         )
                       )
                *)
                
                (* ******************** proof control *********************** *)
                
                inside c
                  (fun displaystate ->
                     showproof <*> 
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
                                       try findTip tree path; false with
                                         FollowPath_ _ -> false
                                       | FindTip_ -> true
                                   in
                                   let rec undo_proofstep =
                                     fun
                                       (WinHist {proofhist = proofhist} as
                                          hist) ->
                                         (undo_step proofhist &~~
                                          (
                                             fSome <*> 
                                               ((fun ph ->
                                                   withproofhist (hist, ph))
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
                     showproof <*> 
                       ((fun here ->
                           tryLayout displaystate PruneCommand PrunePath here)
                          ))
            | "collapse", [] ->
                (* Hide/Show subproof *)
                inside c
                  (fun displaystate ->
                     showproof <*> 
                       ((fun here ->
                           tryLayout displaystate HideShowCommand PrunePath
                             here)
                          ))
            | "hideroot", [] ->
                (* Hide conclusion *)
                inside c
                  (fun displaystate ->
                     showproof <*> 
                       ((fun here ->
                           tryLayout displaystate HideRootCommand HitPath
                             here)
                          ))
            | "exposeparent", [] ->
                (* Show parent conclusion *)
                inside c
                  (fun displaystate ->
                     showproof <*> 
                       ((fun here ->
                           tryLayout displaystate ExposeParentCommand HitPath
                             here)
                          ))
            | "hidecut", [] ->
                (* Hide cut hypothesis *)
                inside c
                  (fun displaystate ->
                     showproof <*> 
                       ((fun here ->
                           tryLayout displaystate HideCutCommand PrunePath
                             here)
                          ))
            | "layout", _ ->
                (* Expand/Contract detail *)
                inside c
                  (fun displaystate ->
                     showproof <*> 
                       ((fun here ->
                           tryLayout displaystate ExpandContractCommand
                             LayoutPath here)
                         ))
            | "addnewconjecture", panel :: text ->
                begin try
                  let panel = namefrom panel in
                  let name = addnewconjecture panel text in
                  japeserver.selectpanelentry
                    (namestring panel, namestring name);
                  default
                with
                  AddConjecture_ -> default
                end
            | "lemma", panel :: text ->
                processcommand thisstate ("addnewconjecture" :: panel :: text)
            | "prove", comm ->
                let name = namefrom (respace comm) in
                if match proofnamed name with
                     Some (v, tree, provisos, givens, disproofopt) ->
                       screenquery
                         [namestring name;
                          if v then " is already a theorem"
                          else " is already disproved";
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
                    japeserver.readFileName "Load new theory from:"
                      japeserver.toplevelfiletype
                  with
                    Some s -> processcommand (doResettheory ()) ["use"; s]
                  | None -> default
                else default
            | "profile", ["on"] ->
                japeenv.set (env, namefrom "profiling", parseTactic "true");
                (* achieves profileOn(), I hope *)
                default
            | "profile", ["off"] ->
                japeenv.set (env, namefrom "profiling", parseTactic "false");
                (* achieves profileOff(), I hope *)
                default
            | "profile", ["reset"] -> profileReset (); default
            | "profile", ["report"] ->
                begin match
                  japeserver.writeFileName "Profile output to:"
                    japeserver.dbugfiletype
                with
                  Some s -> writetonamedfile profileReport s; ()
                | None -> ()
                end;
                default
            | "profile", ["report"; filename] ->
                writetonamedfile profileReport filename; default
            | "fonts_reset", [] ->
                (* needs to do disproof as well *)
                japeserver.resetcache ();
                let pinfs =
                  match pinfs with
                    fg :: bgs ->
                      fg ::
                        (if japeserver.canbackgroundfocus then
                           let rec f pinf =
                             let hist = proofinfo_hist pinf in
                             let proof = winhist_proofnow hist in
                             let disproof = winhist_disproofnow hist in
                             japeserver.setbackgroundfocus
                               (proofinfo_proofnum pinf);
                             withhist
                               (withdisplaystate
                                  (withneedsrefresh (pinf, false),
                                   showState (proofinfo_displaystate pinf)
                                     (let r = proof !autoselect in 
                                      setGivens (proofstate_givens proof); r)),
                                reparentprovisos hist)
                           in
                             f <* (let r = bgs in japeserver.setforegroundfocus (); r)
                         else
                             (fun pinf -> withneedsrefresh (pinf, true)) <* bgs)
                  | _ -> pinfs
                in
                env, mbs, ShowBoth, pinfs
            | "showfile", [filename] ->
                (* here cos I can't work out how to get round the NOSELCOMMAND trap. RB 14/ii/94 *)
                japeserver.showfile (unQuote filename); default
            | "saveproofs", [w] ->
                let newfile = w = "true" in
                let pinfs' =
                     (fun pinf ->
                        withhist
                          (pinf, withchanged (proofinfo_hist pinf, false))) <*
                     pinfs
                in
                if newfile &&(* Save As .. *)  saveable () || not newfile &&(* Save *)  needssaving ()
                then
                  saveproofs newfile
                else showAlert ["Nothing to save!"];
                env, mbs, DontShow, pinfs'
            | "quit", [] ->
                if needssaving () then
                  askSave "quitting"
                    (fun () -> japeserver.quit (); raise QuitJape)
                    (fun () -> japeserver.quit (); raise QuitJape)
                    (fun () -> japeserver.dontquit (); default) ()
                else begin japeserver.quit (); raise QuitJape end
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
                  Pinf
                    (let module M =
                       struct
                         class a =
                           object
                             val title = title
                             val proofnum = proofnum
                             val displayvarsvals = recorddisplayvars env
                             (* just in case *)
                             val needsrefresh = needsrefresh
                             val displaystate = displaystate
                             val hist = hist
                             val fromstore = fromstore
                             method title = title
                             method proofnum = proofnum
                             method displayvarsvals = displayvarsvals
                             method needsrefresh = needsrefresh
                             method displaystate = displaystate
                             method hist = hist
                             method fromstore = fromstore
                           end
                       end
                     in
                     new M.a) ::
                    List.tl pinfs
                in
                let (fg, bgs) = findproof pinfs nstring in
                newfocus (env, mbs, DontShow, fg :: bgs)
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
                let rec closed () = newfocus (env, mbs, DontShow, pinfs') in
                (* a proof can be finished in no steps.  But if it comes from store, we don't
                   re-record it unless you've developed it.
                 *)
                if finished proofstate disproof then
                  if (not fromstore ||
                      not
                        (null (hist_pasts proofhist) &&
                         isproven proofstate)) ||
                     (match disproofhist with
                        Some d -> not (null (hist_pasts d))
                      | None -> false) &&
                     disproof_finished disproof
                  then
                    alert.askDangerously
                      (("The proof of " ^ namestring t) ^
                         " is complete - do you want to record it?")
                      ("Record",
                       (fun () ->
                          if endproof n t (isproven proofstate) proofstate
                               disproof
                          then
                            closed ()
                          else default))
                      ("Don't record",
                       (fun () -> japeserver.closeproof n; closed ()))
                      (fun () -> default) ()
                  else default
                else if
                  (* hist doesn't normally matter, since we don't store it ... if you are looking at an
                     undeveloped proof, we can just throw it away.
                   *)
                  not fromstore &&
                  (not (isTip (proofstate_tree proofstate)) ||
                   not (disproof_minimal disproof))
                then
                  (* there is something to save *)
                  if screenquery ["Abandon proof of "; namestring t; "?"]
                       "Abandon" "Cancel" 1
                  then
                    begin japeserver.closeproof n; closed () end
                  else default
                else(* nothing to save *)
                 begin japeserver.closeproof n;(*  consolereport ["closing proof ", string_of_int n, 
                       " -- now ", string_of_int (List.length pinfs'), " proofs left"
                   ]; 
                 *)
                 closed () end
            | "createdbugfile", [] ->
                begin match
                  japeserver.writeFileName "Write diagnostic output to:"
                    japeserver.dbugfiletype
                with
                  Some s ->
                    let file = unQuote s in
                    begin try createdbugfile file with
                      Io err ->
                        showAlert
                          ["can't create file"; file; " ("; Io_explain err;
                           ")"]
                    end;
                    default
                | None -> default
                end
            | "closedbugfile", [] -> closedbugfile (); default
            | _ -> cannotprocesscommand c; default
      (*processcommand*)
               
      and cannotprocesscommand command =
        showAlert
          ["dialogue.sml processcommand cannot process "; respace command]
      in
      let rec domb (var, notify) =
        let setting =
          try unSOME (japeenv.at (env, var)) with
            UnSOME_ ->
              raise
                (Catastrophe_
                   ["domb error: variable "; namestring var;
                    " in mbs but not in env"])
        in
        match at (!mbcache, var) with
          Some r ->
            if !r = setting then ()
            else begin notify (termstring setting, true); r := setting end
        | None ->
            mbcache := ( ++ ) (!mbcache, ( |-> ) (var, ref setting));
            notify (termstring setting, true)
      in
      (* for the time being, until there is effective proof/disproof focus, we have too many buttons *)
      let rec administer displayopt =
        List.iter button.enable
          [UndoProofbutton, proofundoable ();
           RedoProofbutton, proofredoable ();
           UndoDisproofbutton, disproofundoable ();
           RedoDisproofbutton, disproofredoable ();
           Finishedbutton, finishable (); Resetbutton, resetable ();
           Savebutton, needssaving (); SaveAsbutton, saveable ();
           Disprovebutton, not (null pinfs) && hasforcedefs ()];
        List.iter domb mbs;
        (* this is lazy -- see comment above *)
        begin try
          japeserver.settextselectionmode
            (termstring
               (unSOME (japeenv.at (env, namefrom "textselectionmode"))))
        with
          UnSOME_ ->
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
              FormulaHit fh,
              FormulaSel (path, concopt, hyps, _, _, _ as fsel) ->
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
                  doubleclick.matchdoubleclick sense (mkSeq p)
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
                         showproof <*> 
                           ((fun here ->
                               evolve displaystate env
                                 (pointToSequent displaystate env c' fsel)
                                 here)
                              ))
                | None -> default
                end
            | _, ReasonSel _ ->
                inside []
                  (fun displaystate ->
                     showproof <*> 
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
                        (withdisplayvarsvals (pinf, recorddisplayvars env),
                         false),
                      state),
                   reparentprovisos hist) ::
                  rest
              in
              let
                (Proofstate {tree = tree; cxt = cxt; goal = goal} as
                   proofstate)
                =
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
                      Some d -> disproof.showdisproof d
                    | None -> disproof.cleardisproof ()
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
        | QuitJape -> raise QuitJape
        | Tacastrophe_ ss ->
            showAlert
              ("Tacastrophe_ in commands (shouldn't happen)!! -- " :: ss);
            env, mbs, DontShow, pinfs
        | exn ->
            showAlert
              ["unexpected exception "; Printexc.to_string exn; " in commands"];
            env, mbs, DontShow, pinfs
      in
      commands nextargs
    and save file = GCsave file
    and GCsave file =
      japeserver.quit ();
      interaction.abandonServer ();
      cleanup ();
      initButtons ();
      exportFn (file, main (defaultenv (), [], []))
    and saverunning env mbs file =
      japeserver.quit ();
      interaction.abandonServer ();
      cleanup ();
      exportFn (file, main (env, [], mbs))
    and start () =
      japeserver.quit ();
      interaction.abandonServer ();
      cleanup ();
      initButtons ();
      main (defaultenv (), [], []) ([""], []);
      (* Bernard, perhaps you could tell me what to put here ... *)
      ()
  end








