(* $Id$ *)

module type Interaction =
  sig
    type 'a prooftree
    and proofstate
    and term
    and 'a hit
    and 'a fhit
    and 'a sel
    and hitkind
    and pos
    and cxt
    and displayclass
    and displaystate
    and element
    and seq
    and side
    type treeformat and fmtpath
    type command =
        TextCommand of string list
      | HitCommand of (treeformat prooftree * fmtpath hit * fmtpath sel)
    val commandstring : command -> string
    val startServer : string * string list -> unit
    val abandonServer : unit -> unit
    val killServer : unit -> unit
    val deadServer : string list -> unit
    val runningServer : unit -> bool
    val setdisplaystyle : string -> unit
    val getdisplaystyle : unit -> string
    val showProof :
      displaystate -> fmtpath option -> fmtpath option -> cxt ->
        treeformat prooftree -> bool -> displaystate
    val showFocussedProof :
      fmtpath option -> cxt -> treeformat prooftree -> bool -> displaystate
    val refreshProof : displaystate -> unit
    val setProvisos : cxt -> unit
    val setGivens : seq list -> unit
    val showallprovisos : bool ref
    val getCommand : displaystate option -> command
    val findSelection : displaystate -> fmtpath sel option
    val findLayoutSelection : displaystate -> hitkind -> fmtpath option
    (* Drag n drop is moribund, as currently implemented.  Will be redone! *)
    val dropsource : element list ref
    val droptarget : element list ref
    val setComment : string list -> unit
    val showState : displaystate -> proofstate -> bool -> displaystate
    val printState : Pervasives.out_channel -> proofstate -> bool -> unit
    val alterTip :
      displaystate -> cxt -> fmtpath -> treeformat prooftree ->
        (treeformat prooftree * fmtpath) option ->
        (bool * fmtpath * element) * string list ->
        cxt * element * treeformat prooftree
  end



(* $Id$ *)

module
  Interaction
  (AAA :
    sig
      module stringfuns : Stringfuns
      module answer : Answer
      module box : Box
      module sequent : sig include Sequenttype include Sequent end
      module proviso : Proviso
      module prooftree : Prooftree
      module proofstate : Proofstate
      module japeserver : Japeserver
      module treedraw : Displaystyle
      module boxdraw : Displaystyle
      module displayclass : Displayclass
      module displayfont : Displayfont
      module hit : Hit
      module displaystate : Displaystate
      module treeformat : TreeFormat
      exception Catastrophe_ of string list
      exception Selection_ of string list
      exception UnSOME_
      val ( <| ) : ('a -> bool) * 'a list -> 'a list
      val andthenr : 'a option * ('a -> 'b option) -> 'b option
      val atoi : string -> int
      val bracketedliststring : ('a -> string) -> string -> 'a list -> string
      val consolereport : string list -> unit
      val dont_rewrite_with_this : proofstate.cxt
      val elementstring : sequent.element -> string
      val findfirst : ('a -> 'b option) -> 'a list -> 'b option
      val interpolate : 'a -> 'a list -> 'a list
      val invisible : string -> bool
      val lowercase : string -> string
      val member : 'a * 'a list -> bool
      val numbered : 'a list -> (int * 'a) list
      val optionfilter : ('a -> 'b option) -> 'a list -> 'b list
      val optionstring : ('a -> string) -> 'a option -> string
      val ortryr : 'a option * (unit -> 'a option) -> 'a option
      val provisos : proofstate.cxt -> proviso.visproviso list
      val replaceelement :
        sequent.term -> sequent.element -> sequent.term ->
          sequent.element * sequent.term
      val rewritecxt : proofstate.cxt -> proofstate.cxt
      val seektipselection : bool ref
      val selection2Subst :
        bool -> string list -> proofstate.cxt -> proofstate.cxt * sequent.term
      val setComment : string -> unit
      val showAlert : string -> unit
      val smlelementstring :
        (sequent.term -> string) -> sequent.element -> string
      val sort : ('a * 'a -> bool) -> 'a list -> 'a list
      val termstring : sequent.term -> string
      val try__ : ('a -> 'b) -> 'a option -> 'b option
      val unSOME : 'a option -> 'a
      
    end)
  :
  Interaction =
  struct
    open AAA
    open stringfuns
    open answer
    open box
    open sequent
    open prooftree
    open proofstate
    open treeformat
    open displayclass
    open displayfont
    open hit
    open displaystate
    open IO
    
    
    
    
    type command =
        TextCommand of string list
      | HitCommand of (treeformat prooftree * fmtpath hit * fmtpath sel)
    let rec commandstring =
      function
        TextCommand ws -> "TextCommand" ^ bracketedliststring enQuote ", " ws
      | HitCommand hc ->
          "HitCommand" ^
            triplestring (fun _ -> "....") (hitstring fmtpathstring)
              (selstring fmtpathstring) "," hc
    let intliststring = bracketedliststring (string_of_int : int -> string) ","
    let abandonServer = japeserver.stopserver
    let killServer = japeserver.killserver
    let setComment ooo = setComment (implode ooo)
    let rec deadServer strings = consolereport strings; abandonServer ()
    let rec startServer (serverpath, args) =
      try
        (* open japeserver OCaml no like *) startserver serverpath args;
        if idlsignature <> getSignature () then
          begin
            consolereport ["Incompatible japeserver: "; serverpath; "\007\n"];
            killserver ()
          end
      with
        server_input_terminated ->
          deadServer ["Cannot find japeserver: "; serverpath]
    let rec runningServer () = !(japeserver.running)
    let treestyle = treedraw.style
    let boxstyle = boxdraw.style
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
        (lh p class__ kind : fmtpath hit option)
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
      let (fsels, textsels, givensel) = japeserver.getAllSelections () in
      (* remove invisbra/kets from any text selections we see *)
      let rec deinvis s =
        implode (( <| ) ((fun c -> not (invisible c)), explode s))
      in
      let textsels = map (fun (p, ss) -> p, map deinvis ss) textsels in
      let givensel = map deinvis givensel in
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
                  hitstring fmtpathstring h])
      in
      let fhits =
        map (fun (pos, class__) -> pos, pos2hit pos (Some class__) pathkind)
          fsels
      in
      let thits =
        map
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
      map (fun(_,hash2)->hash2) fhits, thits, givensel
    let rec findSelection state =
      let (fhits, thits, givensel) = sortoutSelection state HitPath in
      (* only path that makes sense for what we are trying to do ... *)
      let showstrings = bracketedliststring enQuote "," in
      (* val _ = consolereport ["findSelection sees ", bracketedliststring (hitstring fmtpathstring) "," fhits, "; ",
                                                       bracketedliststring (pairstring (fhitstring fmtpathstring) showstrings ",") "," thits, "; ",
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
                     hitstring fmtpathstring h]))
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
            map
              (fun (hpath, hypel) ->
                 if fmtprooftree.validhyp tree hypel cpath then hypel
                 else
                   raise
                     (Catastrophe_
                        ["incompatible double hit in findSelection (interaction): ";
                         fmtpathstring hpath; " ";
                         smlelementstring termstring hypel; "; ";
                         fmtpathstring cpath; " ";
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
                 if fmtprooftree.validhyp tree hypel path then path
                 else hpath)
              hs hpath
          in
          let hypels = map (fun(_,hash2)->hash2) hyphits in
          let rec ok path =
            not
              (List.exists
                 (fun ooo ->
                    not ((fun el -> fmtprooftree.validhyp tree el path) ooo))
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
                      (pairstring fmtpathstring (smlelementstring termstring)
                         ",")
                      "," hyphits])
            else if !seektipselection && refineSelection state then
              (* is there a single conclusion above us, and are all our hypotheses still valid at that point? *)
              match
                fmtprooftree.allTipPaths (fmtprooftree.followPath tree path)
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
                bracketedliststring (hitstring fmtpathstring) "," fhits])
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
                      hitstring fmtpathstring h])
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
                optionstring (hitstring fmtpathstring) h])
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
      let text = japeserver.listen () in
      let rec getdisplay () =
        try unSOME displayopt with
          UnSOME_ ->
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
                  HitCommand (unSOME (storedProof (getdisplay ())), h, s)
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
            ( <| )
              ((match class__ with
                  DisplayReason ->
                    begin function
                      _, DisplayReason -> true
                    | pos, _ -> japeserver.highlight pos None; false
                    end
                | _ ->
                    function
                      pos, DisplayReason ->
                        japeserver.highlight pos None; false
                    | _ -> true),
               parseselections others)
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
      if !showallprovisos then ps else ( <| ) (proviso.provisovisible, ps)
    let rec sortprovisos ps =
      sort
        (fun (p1, p2) ->
           let b1 = proviso.provisovisible p1 in
           let b2 = proviso.provisovisible p2 in
           not b1 && b2 ||
           b1 = b2 &&
           proviso.earlierproviso
             (proviso.provisoactual p1, proviso.provisoactual p2))
        ps
    let rec setProvisos cxt =
      let ps = sortprovisos (provisos cxt) in
      japeserver.setProvisos
        (ProvisoFont, map proviso.visprovisostring (filterprovisos ps))
    let rec setGivens givens =
      japeserver.setGivens (numbered (map seqstring givens))
    let rec printProvisos outstream cxt =
      let ps = sortprovisos (provisos (rewritecxt cxt)) in
      match ps with
        [] -> ()
      | ps ->
          output_string outstream "(PROVIDED ";
          List.iter
            (fun p ->
               output_string outstream
                 (("\"" ^ proviso.visprovisostring p) ^ "\" ");
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
      let (wholepath, wholetree) = makewhole cxt root tree gpath in
      if fmtprooftree.validelement selishyp wholetree selel wholepath then
        let (cxt, subst) = selection2Subst false ss cxt in
        let (Seq (st, hs, cs)) =
          try fmtprooftree.findTip tree gpath with
            _ -> raise (Catastrophe_ ["findTip failed in alterTip"])
        in
        let (newel, side) =
          replaceelement (if selishyp then hs else cs) selel subst
        in
        (* safety first: we use a dummy context in replaceTip *) 
        cxt, newel,
        replaceTip dont_rewrite_with_this gpath tree
          (Seq (if selishyp then st, side, cs else st, hs, side))
      else
        raise
          (Selection_
             ["your selection "; fmtpathstring selpath;
              " wasn't on the path to the goal "; fmtpathstring wholepath])
  end
