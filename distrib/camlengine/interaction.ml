(* $Id$ *)

module type T =
  sig
    type prooftree and proofstate
    and 'a hit and 'a sel and hitkind
    and cxt and displaystate
    and element and seq and path
    
    type command =
        TextCommand of string list
      | HitCommand of (prooftree * path hit * path sel)
    
    val commandstring : command -> string
    val startServer : string * string list -> unit
    val abandonServer : unit -> unit
    val killServer : unit -> unit
    val deadServer : string list -> unit
    val runningServer : unit -> bool
    val setdisplaystyle : string -> unit
    val getdisplaystyle : unit -> string
    val showProof :
      displaystate -> path option -> path option -> cxt ->
        prooftree -> bool -> displaystate
    val showFocussedProof :
      path option -> cxt -> prooftree -> bool -> displaystate
    val refreshProof : displaystate -> unit
    val setProvisos : cxt -> unit
    val setGivens : seq list -> unit
    val showallprovisos : bool ref
    val getCommand : displaystate option -> command
    val findSelection : displaystate -> path sel option
    val findLayoutSelection : displaystate -> hitkind -> path option
    (* Drag n drop is moribund, as currently implemented.  Will be redone! *)
    val dropsource : element list ref
    val droptarget : element list ref
    val setComment : string list -> unit
    val showState : displaystate -> proofstate -> bool -> displaystate
    val printState : Pervasives.out_channel -> proofstate -> bool -> unit
    val alterTip :
      displaystate -> cxt -> path -> prooftree ->
        (prooftree * path) option ->
        (bool * path * element) * string list ->
        cxt * element * prooftree
  end



(* $Id$ *)

module M : T with type prooftree = Prooftree.Tree.Fmttree.prooftree
			  and type proofstate = Proofstate.M.proofstate
			  and type 'a hit = 'a Hit.M.hit
			  and type 'a sel = 'a Hit.M.sel
			  and type hitkind = Hit.M.hitkind
			  and type cxt = Context.Cxt.cxt
			  and type displaystate = Displaystate.M.displaystate
			  and type element = Hit.M.element
			  and type seq = Sequent.Funs.seq
			  and type path = Prooftree.Tree.Fmttree.path
=
  struct
    open Answer.M
    open Box.M
    open Displayclass.M
    open Displayfont.M
    open Displaystate.M
    open Hit.M
    open Proofstate.M
    open Prooftree.Tree.Fmttree
    open Sequent.Funs
    open Sequent.Type
    open Sml.M
    open Stringfuns.M
    open Treeformat.Fmt

	let ( <| ) = Listfuns.M.( <| )
	let (&~~) = Optionfuns.M.(&~~)
	let (|~~) = Optionfuns.M.(|~~)
	let _The = Optionfuns.M._The
	let atoi = Miscellaneous.M.atoi
	let bracketedliststring = Listfuns.M.bracketedliststring
	let consolereport = Miscellaneous.M.consolereport
	let dont_rewrite_with_this = Context.Cxt.dont_rewrite_with_this
	let elementstring = Term.Termstring.elementstring
	let findfirst = Optionfuns.M.findfirst
	let interpolate = Listfuns.M.interpolate
	let invisible = Miscellaneous.M.invisible
	let lowercase = Stringfuns.M.lowercase
	let member = Listfuns.M.member
	let numbered = Listfuns.M.numbered
	let optionfilter = Optionfuns.M.optionfilter
	let optionstring = Optionfuns.M.optionstring
	let provisos = Context.Cxt.provisos
	let replaceelement = Term.Funs.replaceelement
	let rewritecxt = Rewrite.Funs.rewritecxt
	let seektipselection = Miscellaneous.M.seektipselection
	let selection2Subst = Selection.M.selection2Subst
	let setComment = Alert.M.setComment
	let showAlert = Alert.M.showAlert Alert.M.defaultseverity_alert
	let smlelementstring = Term.Termstring.smlelementstring
	let sort = Listfuns.M.sort
	let take = Listfuns.M.take
	let termstring = Term.Termstring.termstring
	let try__ = Optionfuns.M.try__
	
	exception Catastrophe_ = Miscellaneous.M.Catastrophe_
	exception Selection_ = Selection.M.Selection_
	exception None_ = Optionfuns.M.None_
    
    type prooftree = Prooftree.Tree.Fmttree.prooftree
	 and proofstate = Proofstate.M.proofstate
	 and 'a hit = 'a Hit.M.hit
	 and 'a sel = 'a Hit.M.sel
	 and hitkind = Hit.M.hitkind
	 and cxt = Context.Cxt.cxt
	 and displayclass 
	 and displaystate = Displaystate.M.displaystate
	 and element = Hit.M.element
	 and seq = Sequent.Funs.seq
	 and path = Prooftree.Tree.Fmttree.path

    type command =
        TextCommand of string list
      | HitCommand of (prooftree * path hit * path sel)
    let rec commandstring =
      function
        TextCommand ws -> "TextCommand" ^ bracketedliststring enQuote ", " ws
      | HitCommand hc ->
          "HitCommand" ^
            triplestring (fun _ -> "....") (hitstring pathstring)
              (selstring pathstring) "," hc
    let intliststring = bracketedliststring (string_of_int : int -> string) ","
    let abandonServer = Japeserver.stopserver
    let killServer = Japeserver.killserver
    let setComment = setComment <*> implode
    let rec deadServer strings = consolereport strings; abandonServer ()
    let rec startServer (serverpath, args) =
      try
        Japeserver.startserver serverpath args;
        if Japeserver.idlsignature <> Japeserver.getSignature () then
          begin
            consolereport ["Incompatible japeserver: "; serverpath];
            Japeserver.killserver ()
          end
      with
        server_input_terminated ->
          deadServer ["Cannot find japeserver: "; serverpath]
    let rec runningServer () = !(Japeserver.running)
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
    let rec showProof =
      fun (DisplayState {showProof = sp}) target goal cxt tree withgoal ->
        sp tree target (if withgoal then goal else None)
    let rec showFocussedProof goal cxt tree withgoal =
      match !currentstyle with
        DisplayState {showFocussedProof = sfp} ->
          sfp tree (if withgoal then goal else None)
    let rec refreshProof = fun (DisplayState {refreshProof = rp}) -> rp ()
    let rec locateHit =
      fun (DisplayState {locateHit = lh}) p class__ kind ->
        (lh p class__ kind : path hit option)
    let rec notifyselect =
      fun (DisplayState {notifyselect = nsel}) bpcopt sels ->
        (nsel bpcopt sels : unit)
    let rec storedProof = fun (DisplayState {storedProof = sp}) -> sp ()
    let rec refineSelection = fun (DisplayState {refineSelection = rS}) -> rS
    let rec printProof outstream target goal cxt tree withgoal =
      match !currentstyle with
        DisplayState {printProof = pp} ->
          pp outstream tree target (if withgoal then goal else None)
    (* one way of telling that I have the interface and datatypes wrong is all these blasted Catastrophe_ exceptions ... *)
    let rec sortoutSelection state pathkind =
      let (fsels, textsels, givensel) = Japeserver.getAllSelections () in
      (* remove invisbra/kets from any text selections we see *)
      let rec deinvis s =
        implode ((fun c -> not (invisible c)) <| explode s)
      in
      let textsels = List.map (fun (p, ss) -> p, List.map deinvis ss) textsels in
      let givensel = List.map deinvis givensel in
      let rec pos2hit pos copt pathkind =
        match locateHit state pos copt pathkind with
          Some h -> h
        | None ->
            raise
              (Catastrophe_
                 ["sortoutSelection (interaction) can't locate ";
                  posstring pos; ", "; optionstring displayclassstring copt])
      in
      let rec hit2fhit a1 a2 =
        match a1, a2 with
          s, FormulaHit fh -> fh
        | s, h ->
            raise
              (Catastrophe_
                 ["sortoutSelection (interaction) sees "; s; " hit ";
                  hitstring pathstring h])
      in
      let fhits =
        List.map (fun (pos, class__) -> pos, pos2hit pos (Some class__) pathkind)
          fsels
      in
      let thits =
        List.map
          (fun (pos, strings) ->
             (match
                findfirst
                  (fun (pos', hit) -> if pos = pos' then Some hit else None)
                  fhits
              with
                Some h -> hit2fhit "textsel" h
              | None -> hit2fhit "textsel" (pos2hit pos None pathkind)),
             strings)
          textsels
      in
      List.map snd fhits, thits, givensel
    let rec findSelection state =
      let (fhits, thits, givensel) = sortoutSelection state HitPath in
      (* only path that makes sense for what we are trying to do ... *)
      let showstrings = bracketedliststring enQuote "," in
      (* val _ = consolereport ["findSelection sees ", bracketedliststring (hitstring pathstring) "," fhits, "; ",
                                                       bracketedliststring (pairstring (fhitstring pathstring) showstrings ",") "," thits, "; ",
                                                       showstrings givensel]
       *)
      let (conchits, hyphits, reasonhits) =
        nj_fold
          (function
             FormulaHit (ConcHit c), (cs, hs, rs) -> c :: cs, hs, rs
           | FormulaHit (HypHit h), (cs, hs, rs) -> cs, h :: hs, rs
           | ReasonHit r, (cs, hs, rs) -> cs, hs, r :: rs
           | h, _ ->
               raise
                 (Catastrophe_
                    ["findSelection (interaction) sees hit ";
                     hitstring pathstring h]))
          fhits ([], [], [])
      in
      (* it gets too hard not to trust the interface here ... I'm just going to take the hyp interpretation of 
       * each ambiguous text selection.
       *)
      let (tcs, ths) =
        nj_fold
          (function
             (AmbigHit (_, (_, h)), ss), (tcs, ths) -> tcs, (h, ss) :: ths
           | (ConcHit (_, c), ss), (tcs, ths) -> (c, ss) :: tcs, ths
           | (HypHit (_, h), ss), (tcs, ths) -> tcs, (h, ss) :: ths)
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
                 else
                   raise
                     (Catastrophe_
                        ["incompatible double hit in findSelection (interaction): ";
                         pathstring hpath; " ";
                         smlelementstring termstring hypel; "; ";
                         pathstring cpath; " ";
                         pairstring (smlelementstring termstring)
                           (optionstring sidestring) "," conc]))
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
                 (not  <*> (fun el -> Prooftree.Tree.Fmttree.validhyp tree el path))
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
                    bracketedliststring
                      (pairstring pathstring (smlelementstring termstring)
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
          | _ -> Some (TextSel (thits, givensel))
          end
      | _ ->
          raise
            (Catastrophe_
               ["findSelection (interaction) sees too many hits: ";
                bracketedliststring (hitstring pathstring) "," fhits])
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
                      hitstring pathstring h])
      in
      getpath None fhits
    let rec formulahit2els where h =
      match h with
        Some (FormulaHit fh) ->
          begin match fh with
            ConcHit (_, (c, _)) -> [c]
          | HypHit (_, h) -> [h]
          | AmbigHit ((_, (c, _)), (_, h)) -> [c; h]
          end
      | _ ->
          raise
            (Catastrophe_
               ["formulahit2els (in "; where; ") can't handle ";
                optionstring (hitstring pathstring) h])
    let dropsource : element list ref = ref []
    let droptarget : element list ref = ref []
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
        try pos (atoi x, atoi y) with
          _ ->
            raise
              (Catastrophe_ ["bad pos in getCommand (interaction): "; text])
      in
      let rec mkclass c =
        try int2displayclass (atoi c) with
          _ ->
            raise
              (Catastrophe_ ["bad class in getCommand (interaction): "; text])
      in
      let rec parseselections =
        function
          x :: y :: c :: others ->
            (mkpos x y, mkclass c) :: parseselections others
        | [] -> []
        | _ ->
            raise
              (Catastrophe_
                 ["bad selection tail in getCommand (interaction): "; text])
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
                  raise
                    (Catastrophe_
                       ["getCommand (interaction) sees hit but not selection: ";
                        text])
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
          let class__ = mkclass c in
          let sels =
               (match class__ with
                  DisplayReason ->
                    (function
                      _, DisplayReason -> true
                    | pos, _ -> Japeserver.highlight pos None; false)
                | _ ->
                    (function
                      pos, DisplayReason ->
                        Japeserver.highlight pos None; false
                    | _ -> true)) <|
               parseselections others
          in
          notifyselect (getdisplay ()) (Some (mkpos x y, class__)) sels;
          getCommand displayopt
      | "DESELECT" :: sels ->
          (* DESELECT sels means something has been taken away, and this is the selection now. *)
          notifyselect (getdisplay ()) None (parseselections sels);
          getCommand displayopt
      | "DRAGQ" :: x :: y :: _ ->
          dropsource :=
            formulahit2els "getCommand DRAGQ"
              (locateHit (getdisplay ()) (mkpos x y) None HitPath);
          TextCommand ["DRAGQUERY"]
      | "DROP" :: tx :: ty :: sx :: sy :: _ ->
          let rec decode x y =
            formulahit2els "getCommand DROP"
              (locateHit (getdisplay ()) (mkpos x y) None HitPath)
          in
          dropsource := decode sx sy;
          droptarget := decode tx ty;
          TextCommand ["DROPCOMMAND"]
      | "COMMAND" :: comm -> TextCommand comm
      | _ ->
          showAlert ("getCommand (interaction) cannot understand " ^ text);
          getCommand displayopt
    let showallprovisos = ref false
    let rec filterprovisos ps =
      if !showallprovisos then ps else Proviso.M.provisovisible <| ps
    let rec sortprovisos ps =
      sort
        (fun p1 p2 ->
           let b1 = Proviso.M.provisovisible p1 in
           let b2 = Proviso.M.provisovisible p2 in
           (not b1 && b2) ||
           (b1 = b2 &&
			Proviso.M.earlierproviso
			  (Proviso.M.provisoactual p1) (Proviso.M.provisoactual p2)))
        ps
    let rec setProvisos cxt =
      let ps = sortprovisos (provisos cxt) in
      Japeserver.setProvisos
        (ProvisoFont, List.map Proviso.M.visprovisostring (filterprovisos ps))
    let rec setGivens givens =
      Japeserver.setGivens (numbered (List.map seqstring givens))
    let rec printProvisos outstream cxt =
      let ps = sortprovisos (provisos (rewritecxt cxt)) in
      match ps with
        [] -> ()
      | ps ->
          output_string outstream "(PROVIDED ";
          List.iter
            (fun p ->
               output_string outstream
                 (("\"" ^ Proviso.M.visprovisostring p) ^ "\" ");
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
               output_string outstream (("\"" ^ seqstring g) ^ "\" ");
               output_string outstream "\n")
            gs;
          output_string outstream ")"
    let rec showState displaystate =
      fun
        (Proofstate {cxt = cxt; tree = tree; goal = goal; target = target} as
           state)
        withgoal ->
        let ds = showProof displaystate target goal cxt tree withgoal in
        setProvisos cxt; ds
    let rec printState outstream =
      fun
        (Proofstate
           {cxt = cxt;
            tree = tree;
            givens = givens;
            goal = goal;
            target = target} as state)
        withgoal ->
        printProof outstream target goal cxt tree withgoal;
        printGivens outstream givens;
        printProvisos outstream cxt
    let rec alterTip
      displaystate cxt gpath tree root ((selishyp, selpath, selel), ss) =
      let (wholepath, wholetree) = Prooftree.Tree.makewhole cxt root tree gpath in
      if Prooftree.Tree.Fmttree.validelement selishyp wholetree selel wholepath then
        let (cxt, subst) = selection2Subst false ss cxt in
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
             ["your selection "; pathstring selpath;
              " wasn't on the path to the goal "; pathstring wholepath])
  end
