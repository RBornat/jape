(* $Id$ *)

module type Disproof =
  sig
    type element
    and facts
    and fmtpath
    and forcedef
    and model
    and 'a prooftree
    and seq
    and term
    and treeformat
    type universe
    val catelim_universestring :
      string -> universe -> string list -> string list
    val universestring : string -> universe -> string
    val emptyworld : unit -> universe
    val isemptyworld : universe -> bool
    val addworldlabel : universe -> int * int -> term -> universe option
    val deleteworldlabel : universe -> int * int -> term -> universe option
    (* semantics *)
    val addforcedef : term * forcedef -> unit
    val clearforcedefs : unit -> unit
    val hasforcedefs : unit -> bool
    (* and when I can work out how to fix termstring ...
       val analyse : term -> world -> string
       val seq_analyse : seq -> world -> string list * string * string list
     *)
    
    (* states of the interaction *)
    type disproofstate
    val catelim_disproofstatestring :
      disproofstate -> string list -> string list
    val disproofstatestring : disproofstate -> string
    val disproofstate_seq : disproofstate -> seq
    val disproofstate_universe : disproofstate -> universe
    val disproofstate_selected : disproofstate -> (int * int) list
    val disproofstate_conclusive : disproofstate -> bool
    val disproofstate_countermodel : disproofstate -> bool
    (* infix withdisproofseq withdisproofworld withdisprooftiles withdisproofselected 
             withdisproofconclusive
     *)
    
    (* because of the need for facts when evaluating, these functions don't evaluate.
     * So you should use evaldisproofstate once you've set it up
     *)
    val withdisproofuniverse : disproofstate * universe -> disproofstate
    val newtile : disproofstate -> term -> disproofstate option
    val deleteworld : disproofstate -> int * int -> disproofstate option
    val worldselect :
      disproofstate -> (int * int) list -> disproofstate option
    val addchild : universe -> int * int -> int * int -> universe option
    val moveworld :
      disproofstate -> int * int -> int * int -> disproofstate option
    val deletelink : universe -> int * int -> int * int -> universe option
    val evaldisproofstate :
      facts -> treeformat prooftree -> disproofstate -> disproofstate
    val disproof_start :
      facts -> treeformat prooftree -> fmtpath option -> element list ->
        disproofstate
    val disproof_minimal : disproofstate option -> bool
    (* models and disproofstates *)
    val disproofstate2model : disproofstate option -> (seq * model) option
    val model2disproofstate :
      facts -> treeformat prooftree -> (seq * model) option ->
        disproofstate option
    val checkdisproof :
      facts -> treeformat prooftree -> (seq * model) option -> bool
    exception Disproof_ of string list
    (* stuff to display disproofs *)
    val showdisproof : disproofstate -> unit
    val cleardisproof : unit -> unit
    val disproofdebug : bool ref
  end
(* $Id$*)

(* The disproof calculator.  Commenced 4.vii.01. RB *)
(* It implements disproof in Kripke semantics (made classical by restriction to a single
   world, if necessary).  Predicates operate only on individuals, mostly because I don't
   yet understand terms.  It seems reasonable to me that if there is a disproof, then there
   is one which only uses individuals, so I'm not too unhappy yet.
   RB 
 *)

module
  Disproof
  (AAA :
    sig
      module box : Box
      module forcedef : Forcedef
      module idclass : Idclass
      module japeserver : Japeserver
      module listfuns : Listfuns
      module mappingfuns : Mappingfuns
      module optionfuns : Optionfuns
      module predicate : Predicate
      module seqdraw : Seqdraw
      module term : sig include Termstore include Term end
      type facts and fmtpath and 'a prooftree and treeformat
      val ask : string list -> (string * 'a) list -> int -> 'a
      val askChoice : string * string list list -> int option
      val atoi : string -> int
      val base_sequent : treeformat prooftree -> seqdraw.seq
      val catelim_seqstring : seqdraw.seq -> string list -> string list
      val consolereport : string list -> unit
      val drawindisproofpane : unit -> unit
      val getsemanticturnstile : string -> string option
      val isdigit : string -> bool
      val isextensibleID : term.vid -> bool
      val lowercase : string -> string
      val matchdebug : bool ref
      val matchvars :
        bool -> (term.term -> bool) -> term.term -> term.term ->
          (term.term, term.term) mappingfuns.mapping ->
          (term.term, term.term) mappingfuns.mapping option
      val mkSeq :
        string * term.element list * term.element list -> seqdraw.seq
      val onbraket : string * string
      val offbraket : string * string
      val outbraket : string * string
      val lockbraket : string * string
      val option_remapterm :
        (term.term, term.term) mappingfuns.mapping -> term.term ->
          term.term option
      val pairstring :
        ('a -> string) -> ('b -> string) -> string -> 'a * 'b -> string
      val parseTerm : string -> term.term
      val planinfo : 'a seqdraw.plan -> 'a
      val seqexplode : seqdraw.seq -> string * term.term * term.term
      val seqstring : seqdraw.seq -> string
      val seqvars : seqdraw.seq -> term.term list
      val showAlert : string list -> unit
      val simplifySubst :
        facts -> (term.term * term.term) list -> term.term -> term.term option
      val smlseqstring : seqdraw.seq -> string
      val subtree : treeformat prooftree -> fmtpath -> treeformat prooftree
      val term2binding : term.term -> term.term option
      val triplestring :
        ('a -> string) -> ('b -> string) -> ('c -> string) -> string ->
          'a * 'b * 'c -> string
      val catelim2stringfn :
        ('a -> string list -> string list) -> 'a -> string
      val catelim_arraystring :
        ('a -> string list -> string list) -> string -> 'a array ->
          string list -> string list
      val catelim_bracketedliststring :
        ('a -> string list -> string list) -> string -> 'a list ->
          string list -> string list
      val catelim_pairstring :
        ('a -> string list -> string list) ->
          ('b -> string list -> string list) -> string -> 'a * 'b ->
          string list -> string list
      val catelim_triplestring :
        ('a -> string list -> string list) ->
          ('b -> string list -> string list) ->
          ('c -> string list -> string list) -> string -> 'a * 'b * 'c ->
          string list -> string list
      exception Catastrophe_ of string list
      exception ParseError_ of string list
      exception Tacastrophe_ of string list
      
    end)
  :
  Disproof =
  struct
    open AAA
    open box
    open idclass
    open forcedef
    open japeserver
    open listfuns
    open mappingfuns
    open optionfuns
    open predicate
    open seqdraw
    open term
    type 'a prooftree = 'a prooftree
    and treeformat = treeformat
    and fmtpath = fmtpath
    and facts = facts
    
    
    
    
    
    
    
    
    let rec catelim_intstring i ss = (makestring : int -> string) i :: ss
    let rec catelim_boolstring b ss = (makestring : bool -> string) b :: ss
    let disproofdebug = ref false
    let sameelement = eqelements eqterms
    let rec dosubst facts =
      option_mapterm
        (andthen
           (decodeSubst, (fun (_, P, vts) -> simplifySubst facts vts P)))
    let rec patvar t = (isleaf t && isId t) && isextensibleID (vartoVID t)
    let rec patvars t = ( <| ) (patvar, termvars t)
    let rec matchit pat vs t =
      let res = matchvars true (fun v -> member (v, vs)) pat t empty in
      if !matchdebug then
        consolereport
          ["matchit (disproof) matching "; termstring pat; " ";
           bracketedliststring
             (fun v ->
                pairstring termstring (fun ooo -> idclassstring (idclass ooo))
                  "," (v, v))
             ", " vs;
           " against "; termstring t; " => ";
           optionstring (mappingstring termstring termstring) res];
      res
    let rec isBagClass t =
      match collectionkind t with
        Some (BagClass _) -> true
      | _ -> false
    let rec my_seqexplode s =
      let (st, h, c) = seqexplode s in
      st, isBagClass h, explodeCollection h, isBagClass c, explodeCollection c
    (* *********************** worlds and situations *********************** *)

    (* a world has coordinates, labels (terms), and a list of child worlds.  
     * This defines a DAG.  We can do DAGs with refs, but printing it is
     * hard, so it's simpler to do it with a table, and use indices rather than pointers.  
     * I started with an array, but because we need to be able (for example) to
     * keep histories of disproof attempts, I had to use a mapping.  Oh well.
     *)
    
    type coord = int * int
    type world = term list * coord list
    (* labels, children *)
       
    type universe = (coord, world) mapping
    let catelim_coordstring =
      catelim_pairstring catelim_intstring catelim_intstring ","
    let rec coordstring c = implode (catelim_coordstring c [])
    let catelim_worldstring =
      catelim_pairstring (catelim_bracketedliststring catelim_termstring ",")
        (catelim_bracketedliststring catelim_coordstring ",") ", "
    let rec worldstring w = implode (catelim_worldstring w [])
    let rec catelim_universestring sep u ss =
      "Universe " ::
        catelim_mappingstring catelim_coordstring catelim_worldstring
          ("++" ^ sep) u ss
    let rec universestring sep u = implode (catelim_universestring sep u [])
    let rec universe2list u = map (fun (c, (ts, cs)) -> c, ts, cs) (aslist u)
    let rec emptyworld () = ( |-> ) ((0, 0), ([], []))
    let rec isemptyworld u =
      match ran u with
        [[], []] -> true
      | _ -> false
    let rec getworld u c =
      try unSOME (at (u, c)) with
        _ ->
          raise
            (Catastrophe_
               ["(getworld) no world at "; coordstring c; "; ";
                universestring "" u])
    (* enforce monotonicity *)
    let rec domono pts u c =
      let (ts, cs) = getworld u c in
      match listsub eqterms pts ts with
        [] -> u
      | ts' -> foldl (domono ts') (( ++ ) (u, ( |-> ) (c, (ts' @ ts, cs)))) cs
    let rec addworldlabel u c t =
      match at (u, c) with
        None ->
          raise
            (Tacastrophe_
               ["(addworldlabel) no world at "; coordstring c; "; ";
                universestring "" u])
      | Some (pts, pcs) ->
          if member (t, pts) then None
          else
            Some
              (foldl (domono [t]) (( ++ ) (u, ( |-> ) (c, (t :: pts, pcs))))
                 pcs)
    let rec deleteworldlabel u c t =
      match at (u, c) with
        None ->
          raise
            (Tacastrophe_
               ["(deleteworldlabel) no world at "; coordstring c; "; ";
                universestring "" u])
      | Some (pts, pcs) ->
          if not (member (t, pts)) then None
          else
            let rec islabelledparent u c c' =
              let (ts, cs) = getworld u c' in member (c, cs) && member (t, ts)
            in
            let rec doself u c =
              let (ts, cs) = getworld u c in
              if member (t, ts) then
                ( ++ ) (u, ( |-> ) (c, (listsub eqterms ts [t], cs)))
              else u
            in
            let rec doparents u c =
              foldl doparents (doself u c)
                (( <| ) (islabelledparent u c, dom u))
            in
            Some (doparents u c)
    (* memoisation -- here only temporarily *)
    (*
       val fix: ('a -> 'a) -> 'a
       val memofix: (''a,'b) mapping ref -> ((''a -> 'b) -> ''a -> 'b) -> ''a -> 'b
     *)

    let rec fix ff = ff (fix ff)
    let rec memofix mem ff =
      (* mf *) let rec proxy a =
        match at (!mem, a) with
          Some v -> v
        | None ->
            (* (consolereport ["found ", mf a v]; v) *)
            let v = ff proxy a in(* consolereport["recording ", mf a v]; *)
             mem := ( ++ ) (!mem, ( |-> ) (a, v)); v
      in
      proxy
    (* *********************** semantics *********************** *)
    
    type definition = term * term list * bool * forcedef
    (* lhs, args, hassubst, rhs *)
       
       (* for 'occurrence' read 'individual', I think: things like actual i and so on *)
    let forcedefs : definition list ref = ref []
    let occurrences : (term * term list) list ref = ref []
    (* occurrence forms *)
       
    let rec isoccurrence t =
      List.exists
        (fun ooo -> opt2bool ((fun (occ, occvs) -> matchit occ occvs t) ooo))
        !occurrences
    let rec newoccurrence v =
      match
        ( <| )
          ((function
              t, [v] -> true
            | _ -> false),
           !occurrences)
      with
        (occ, [occv]) :: _ ->
          unSOME (option_remapterm (( |-> ) (v, occv)) occ)
      | _ ->
          raise
            (Catastrophe_
               ["(newoccurrence) no single-variable occurrence forms recorded"])
    (* filters out previous versions of t -- so we only keep actual i, for example,
       instead of actual i, actual j, actual k etc.
     *)
    let rec nodups t vs patf xs =
      t,
      ( <| )
        ((fun ooo ->
            (fun ooo -> (fun ooo -> not (opt2bool ooo)) (matchit t vs ooo))
              (patf ooo)),
         xs)
    let rec newocc t vs os =
      let rec patf (t, vs) = t in
      let (t, os') = nodups t vs patf os in (t, vs) :: os'
    let rec variables t = ( <| ) (isVariable, termvars t)
    let rec findoccs f os =
      match f with
        ForcePrim _ -> os
      | ForceBracket f -> findoccs f os
      | ForceAnd (f1, f2) -> findoccs f1 (findoccs f2 os)
      | ForceOr (f1, f2) -> findoccs f1 (findoccs f2 os)
      | ForceImplies (f1, f2) -> findoccs f1 (findoccs f2 os)
      | ForceEverywhere f -> findoccs f os
      | ForceNowhere f -> findoccs f os
      | ForceAll (t, vs, f) -> findoccs f (newocc t vs os)
      | ForceSome (t, vs, f) -> findoccs f (newocc t vs os)
    (* translate predicates, avoid duplicates *)
    let rec addforcedef (t, fd) =
      try
        let rec notabst _ = false in
        let pbs =
          discardzeroarities (foldterm (findpredicates notabst []) [] t)
        in
        let rec pvars =
          fun (P, abss) ->
            match findpredicatevars abss with
              Some vs -> P, vs
            | None ->
                raise
                  (Predicate_
                     ["Semantics must be simple! In the semantic pattern ";
                      termstring t; ", predicate "; termstring P;
                      " doesn't have bound variables as arguments"])
        in
        let env = mkmap (map pvars pbs) in
        let comp = compilepredicate notabst (fun v -> at (env, v)) in
        let (t, fd) = mapterm comp t, mapforcedefterms comp fd in
        let vs = patvars t in
        let (t, fds) = nodups t vs (fun (t', _, _, _) -> t') !forcedefs in
        let rec hassubst ooo = opt2bool (findinforcedef decodeSubst ooo) in
        forcedefs := (t, vs, hassubst fd, fd) :: fds;
        occurrences := findoccs fd !occurrences
      with
        Predicate_ ss -> raise (ParseError_ ss)
    let rec clearforcedefs () = forcedefs := []; occurrences := []
    let rec hasforcedefs () = not (null !forcedefs)
    (* matching a formula against the definitions *)
    let rec semantics facts t =
      match
        findfirst
          (fun (pat, vs, hassubst, fd) ->
             andthenr
               (matchit pat vs t, (fun env -> Some (env, hassubst, fd))))
          !forcedefs
      with
        Some (env, hassubst, fd) ->
          let fd =
            mapforcedefterms (option_remapterm env) (uniquebinders t fd)
          in
          Some (if hassubst then mapforcedefterms (dosubst facts) fd else fd)
      | None -> None
    (* avoid variable capture in semantics function *)
    (* really this ought to look inside the forcedef, but one step at a time ... *)
    and uniquebinders t fd =
      let rec newoccenv ocvs =
        let vids = orderVIDs (map vartoVID (variables t)) in
        let rec newvar (ocvids, env) ocv =
          let ocvid = vartoVID ocv in
          let ocvid' = uniqueVID VariableClass vids ocvids ocvid in
          ocvid' :: ocvids,
          (if ocvid = ocvid' then env
           else
             ( ++ ) (( |-> ) (ocv, registerId (ocvid', VariableClass)), env))
        in
        let r = sml__hash__2 (foldl newvar ([], empty) ocvs) in
        if !disproofdebug then
          consolereport
            ["semantics matched "; termstring t; " and "; forcedefstring fd;
             "; rewrite with "; mappingstring termstring termstring r];
        r
      in
      let rec doit ocvs =
        mapforcedefterms (option_remapterm (newoccenv ocvs)) fd
      in
      match fd with
        ForceAll (_, ocvs, _) -> doit ocvs
      | ForceSome (_, ocvs, _) -> doit ocvs
      | _ -> fd
    (* finding the subformulae which aren't semantically defined. Designed to be foldl'd *)
    let rec semanticfringe facts ts t =
      let rec tf ts t =
        match semantics facts t with
          None ->
            begin match decodeBracketed t with
              Some t' -> tf ts t'
            | None -> t :: ts
            end
        | Some fd -> ff ts fd
      and ff ts fd =
        match fd with
          ForcePrim t -> tf ts t
        | ForceBracket fd -> ff ts fd
        | ForceAnd pair -> pf ts pair
        | ForceOr pair -> pf ts pair
        | ForceImplies pair -> pf ts pair
        | ForceEverywhere fd -> ff ts fd
        | ForceNowhere fd -> ff ts fd
        | ForceAll (t, _, fd) -> ff (t :: ts) fd
        | ForceSome (t, _, fd) -> ff (t :: ts) fd
      and pf ts (fd1, fd2) = ff (ff ts fd1) fd2 in
      tf ts t
    (* is it forced? *)
    (* rewritten to be memoised, both for 'efficiency' and for de-emphasising irrelevant subformulae
       -- i.e. quantified subformulae
     *)
    let rec unfixedforced facts u =
      let rec ff f (c, t) =
        let rec labels c = sml__hash__1 (getworld u c) in
        let rec children c = sml__hash__2 (getworld u c) in
        let rec lookup c t = member (t, labels c), true in
        (* emphasis locked *)
                       (* forced logic -- we force evaluation of subformulae because otherwise things go grey which shouldn't *)
        let rec logNot =
          function
            true, _ -> false, false
          | _ -> true, false
        in
        let rec logAnd a1 a2 =
          match a1, a2 with
            (true, _), (true, _) -> true, false
          | _, _ -> false, false
        in
        let rec logOr a1 a2 =
          match a1, a2 with
            (false, _), (false, _) -> false, false
          | _, _ -> true, false
        in
        let rec logImp a1 a2 =
          match a1, a2 with
            (true, _), (false, _) -> false, false
          | _, _ -> true, false
        in
        let rec logAll f ooo = foldl logAnd (true, false) (map f ooo) in
        let rec logExists f ooo = foldl logOr (false, false) (map f ooo) in
        let rec indiv_fd (oc, vs, fd) i =
          (* fun remap env t = (* written with andthenr, ortryr, because I don't trust 0.93 with the functional version *)
                                 option_remapterm env t andthenr 
                                 (fn t' => dosubst facts t' ortryr (fn () => Some t'))
                                *)
          let r =
            andthenr
              (matchit oc vs i,
               (fun env ->
                  Some
                    (mapforcedefterms
                       (andthen (option_remapterm env, somef (dosubst facts)))
                       fd)))
          in
          if !disproofdebug then
            begin
              consolereport
                ["indiv_fd ";
                 triplestring termstring (bracketedliststring termstring ",")
                   forcedefstring "," (oc, vs, fd);
                 " "; termstring i; " => "; optionstring forcedefstring r];
              consolereport
                ["matchit oc vs i => ";
                 optionstring (mappingstring termstring termstring)
                   (matchit oc vs i)];
              match matchit oc vs i with
                None -> ()
              | Some env ->
                  consolereport
                    ["mapforcedefterms (option_remapterm env) fd => ";
                     forcedefstring
                       (mapforcedefterms (option_remapterm env) fd)];
                  consolereport
                    ["mapforcedefterms (option_remapterm env andthen (somef (dosubst facts))) fd => ";
                     forcedefstring
                       (mapforcedefterms
                          (andthen
                             (option_remapterm env, somef (dosubst facts)))
                          fd)]
            end;
          r
        in
        let rec interp fd c =
          match fd with
            ForcePrim t' -> f (c, t')
          | ForceBracket fd' -> interp fd' c
          | ForceAnd (fd1, fd2) -> logAnd (interp fd1 c) (interp fd2 c)
          | ForceOr (fd1, fd2) -> logOr (interp fd1 c) (interp fd2 c)
          | ForceImplies (fd1, fd2) -> logImp (interp fd1 c) (interp fd2 c)
          | ForceEverywhere fd' ->
              logAnd (interp fd' c) (logAll (interp fd) (children c))
          | ForceNowhere fd' ->
              logAnd (logNot (interp fd' c)) (logAll (interp fd) (children c))
          | ForceAll tvsfd ->
              logAll
                (fun lab ->
                   match indiv_fd tvsfd lab with
                     Some fd' -> interp fd' c
                   | _ -> true, false)
                (labels c)
          | ForceSome tvsfd ->
              logExists
                (fun lab ->
                   match indiv_fd tvsfd lab with
                     Some fd' -> interp fd' c
                   | _ -> false, false)
                (labels c)
        in
        let _ =
          if !disproofdebug then
            consolereport ["evaluating "; coordstring c; "; "; termstring t]
        in
        let result =
          match semantics facts t with
            None ->
              begin match decodeBracketed t with
                Some t' -> f (c, t')
              | None -> lookup c t
              end
          | Some fd -> interp fd c
        in
        if !disproofdebug then
          consolereport
            ["forced ("; coordstring c; ", "; termstring t; ") => ";
             pairstring makestring makestring "," result];
        result
      in
      ff
    let rec seq_forced forced c s =
      let rec doit e =
        match element2term e with
          None -> false, false
        | Some t -> forced (c, t)
      in
      let (_, _, hyps, _, concs) = my_seqexplode s in
      map doit hyps, map doit concs
    (* *********************** interaction states *********************** *)
    
    type disproofstate =
        Disproofstate of
          < seq : seq; universe : universe; tiles : term list;
            selected : coord list;
            forcemap : ((coord * term), (bool * bool)) mapping;
            conclusive : bool; countermodel : bool >
    let catelim_forcedstring =
      catelim_pairstring catelim_boolstring catelim_boolstring ","
    let rec catelim_disproofstatestring =
      fun
        (Disproofstate
           {seq = seq;
            universe = universe;
            selected = selected;
            tiles = tiles;
            forcemap = forcemap;
            conclusive = conclusive;
            countermodel = countermodel})
        ss ->
        let cbs = catelim_bracketedliststring catelim_boolstring "," in
        "Disproofstate{seq = " ::
          catelim_seqstring seq
            (",\nworld = " ::
               catelim_universestring "" universe
                 (",\nselected = " ::
                    catelim_bracketedliststring catelim_coordstring ","
                      selected
                      (",\ntiles = " ::
                         catelim_bracketedliststring catelim_termstring ","
                           tiles
                           (", forcemap = " ::
                              catelim_mappingstring
                                (catelim_pairstring catelim_coordstring
                                   catelim_termstring ",")
                                catelim_forcedstring "++" forcemap
                                (", conclusive = " ::
                                   catelim_boolstring conclusive
                                     (", countermodel = " ::
                                        catelim_boolstring countermodel
                                          ("}" :: ss)))))))
    let rec disproofstatestring d = implode (catelim_disproofstatestring d [])
    let rec disproofstate_seq = fun (Disproofstate {seq = seq}) -> seq
    let rec disproofstate_universe =
      fun (Disproofstate {universe = universe}) -> universe
    let rec disproofstate_selected =
      fun (Disproofstate {selected = selected}) -> selected
    let rec disproofstate_conclusive =
      fun (Disproofstate {conclusive = conclusive}) -> conclusive
    let rec disproofstate_countermodel =
      fun (Disproofstate {countermodel = countermodel}) -> countermodel
    
    (* because of the need for facts, these don't evaluate! *)
    let rec withdisproofuniverse =
      fun
        (Disproofstate
           {seq = seq;
            tiles = tiles;
            selected = selected;
            forcemap = forcemap;
            conclusive = conclusive;
            countermodel = countermodel}, universe) ->
        Disproofstate
          (let module M =
             struct
               class a =
                 object
                   val seq = seq
                   val universe = universe
                   val tiles = tiles
                   val selected = selected
                   val conclusive = conclusive
                   val forcemap = forcemap
                   val countermodel = countermodel
                   method seq = seq
                   method universe = universe
                   method tiles = tiles
                   method selected = selected
                   method conclusive = conclusive
                   method forcemap = forcemap
                   method countermodel = countermodel
                 end
             end
           in
           new M.a)
    let rec withdisproofselected =
      fun
        (Disproofstate
           {seq = seq;
            universe = universe;
            tiles = tiles;
            forcemap = forcemap;
            conclusive = conclusive;
            countermodel = countermodel}, selected) ->
        Disproofstate
          (let module M =
             struct
               class a =
                 object
                   val seq = seq
                   val universe = universe
                   val tiles = tiles
                   val selected = selected
                   val conclusive = conclusive
                   val forcemap = forcemap
                   val countermodel = countermodel
                   method seq = seq
                   method universe = universe
                   method tiles = tiles
                   method selected = selected
                   method conclusive = conclusive
                   method forcemap = forcemap
                   method countermodel = countermodel
                 end
             end
           in
           new M.a)
    let rec withdisprooftiles =
      fun
        (Disproofstate
           {seq = seq;
            universe = universe;
            selected = selected;
            forcemap = forcemap;
            conclusive = conclusive;
            countermodel = countermodel}, tiles) ->
        Disproofstate
          (let module M =
             struct
               class a =
                 object
                   val seq = seq
                   val universe = universe
                   val tiles = tiles
                   val selected = selected
                   val conclusive = conclusive
                   val forcemap = forcemap
                   val countermodel = countermodel
                   method seq = seq
                   method universe = universe
                   method tiles = tiles
                   method selected = selected
                   method conclusive = conclusive
                   method forcemap = forcemap
                   method countermodel = countermodel
                 end
             end
           in
           new M.a)
    let rec disproof_minimal =
      function
        None -> true
      | Some (Disproofstate {universe = universe}) -> isemptyworld universe
    let rec evaldisproofstate facts tree =
      fun
        (Disproofstate
           {seq = seq;
            universe = universe;
            selected = selected;
            tiles = tiles}) ->
        let (basestyle, hypsbag, basehyps, concsbag, baseconcs) =
          my_seqexplode (base_sequent tree)
        in
        let (style, _, hyps, _, concs) = my_seqexplode seq in
        let conclusive =
          ((match getsemanticturnstile basestyle with
              Some semstyle -> semstyle = style
            | None -> basestyle = style) &&
           (* since styles are equal, so is baggishness *)
           (eqlists sameelement (basehyps, hyps) ||
            hypsbag && null (listsub sameelement basehyps hyps))) &&
          (eqlists sameelement (baseconcs, concs) ||
           concsbag && null (listsub sameelement baseconcs concs))
        in
        match selected with
          [root] ->
            let forcemap : ((coord * term), (bool * bool)) mapping ref =
              ref empty
            in
            (* fun mf a v = pairstring coordstring smltermstring "," a^"|->"^(catelim2stringfn catelim_forcedstring) v *)
            let forced = memofix forcemap (unfixedforced facts universe) in
            let (hs, cs) = seq_forced forced root seq in
            let countermodel =
              All (sml__hash__1, hs) && not (List.exists sml__hash__1 cs)
            in
            Disproofstate
              (let module M =
                 struct
                   class a =
                     object
                       val seq = seq
                       val universe = universe
                       val tiles = tiles
                       val selected = selected
                       val forcemap = !forcemap
                       val conclusive = conclusive
                       val countermodel = countermodel
                       method seq = seq
                       method universe = universe
                       method tiles = tiles
                       method selected = selected
                       method forcemap = forcemap
                       method conclusive = conclusive
                       method countermodel = countermodel
                     end
                 end
               in
               new M.a)
        | _ ->
            Disproofstate
              (let module M =
                 struct
                   class a =
                     object
                       val seq = seq
                       val universe = universe
                       val tiles = tiles
                       val selected = selected
                       val forcemap = empty
                       val conclusive = conclusive
                       val countermodel = false
                       method seq = seq
                       method universe = universe
                       method tiles = tiles
                       method selected = selected
                       method forcemap = forcemap
                       method conclusive = conclusive
                       method countermodel = countermodel
                     end
                 end
               in
               new M.a)
    exception Disproof_ of string list
    (* sort, eliminating duplicates.  This sort is no longer case sensitive *)
    let rec alphasort sel ts =
      map sel
        (sortunique (fun ((s1, _), (s2, _)) -> lowercase s1 < lowercase s2)
           (( ||| ) (map termstring ts, ts)))
    let tilesort ooo = rev (alphasort sml__hash__2 ooo)
    let labelsort = alphasort sml__hash__1
    let rec seq2tiles facts seq =
      let (_, _, hyps, _, concs) = my_seqexplode seq in
      (* check for an absence of silliness *)
      let rec existselement f e =
        match element2term e with
          None -> false
        | Some t -> existsterm f t
      in
      let _ =
        if List.exists
             (fun e ->
                if issegvar e then
                  raise
                    (Disproof_
                       ["Forcing semantics can only deal with actual formulae in sequents.";
                        " Segment variables (e.g. "; elementstring e;
                        ") aren't allowed "; " in disproof sequents."])
                else false)
             (hyps @ concs)
        then
          ()
        else if
          (* can't happen *)
          List.exists
            (existselement
               (fun t ->
                  if isUnknown t then
                    raise
                      (Disproof_
                         ["Unknowns (e.g. "; termstring t; " in sequent ";
                          seqstring seq; ") make disproof somewhat tricky.  Try again when you have resolved the ";
                          " unknown formulae."])
                  else false))
            (concs @ hyps)
        then
          ()
        else if
          (* can't happen *)
          List.exists
            (existselement
               (fun t ->
                  match decodeSubst t with
                    Some _ ->
                      raise
                        (Disproof_
                           ["Substitutions (e.g. "; termstring t;
                            " in sequent "; seqstring seq; ") aren't part of the forcing semantics.  Try again when you have ";
                            " simplified the substitution(s)."])
                  | _ -> false))
            (concs @ hyps)
        then
          ()
      in
      (* find the fringe *)
      let hypterms = optionfilter element2term hyps in
      let concterms = optionfilter element2term concs in
      let ts =
        foldl (semanticfringe facts)
          (foldl (semanticfringe facts) [] hypterms) concterms
      in
      (* add occurrence formulae if necessary *)
      let ts =
        (if List.exists (fun ooo -> opt2bool (decodeBinding ooo)) hypterms ||
            List.exists (fun ooo -> opt2bool (decodeBinding ooo)) concterms
         then
           map sml__hash__1
             (( <| )
                ((fun (occ, vs) ->
                    not
                      (List.exists (fun ooo -> opt2bool (matchit occ vs ooo)) ts)),
                 !occurrences))
         else []) @
          ts
      in
      (* eliminate all individuals which don't appear free in the sequent, 
       * but if there aren't any such individuals, keep just one
       *)
      let svs = seqvars seq in
      match ( <| ) (isVariable, NJfold (tmerge, map patvars ts, [])) with
        [] -> ts
      | tvs ->
          let tvs' =
            match ( <| ) ((fun i -> member (i, svs)), tvs) with
              [] -> [List.hd tvs]
            | vs -> vs
          in
          let badvs = listsub (fun (x, y) -> x = y) tvs tvs' in
          let rec changevars i =
            if member (i, badvs) then Some (List.hd tvs') else None
          in
          map (mapterm changevars) ts
    let rec disproofstate2model =
      function
        None -> None
      | Some (Disproofstate {seq = seq; universe = universe}) ->
          Some
            (seq,
             Model
               (map (fun (c, (ts, chs)) -> World (Coord c, map Coord chs, ts))
                  (aslist universe)))
    let rec model2disproofstate a1 a2 a3 =
      match a1, a2, a3 with
        facts, tree, None -> None
      | facts, tree, Some (seq, model) ->
          let rec coord = fun (Coord c) -> c in
          let (Model worlds) = model in
          let ulist =
            map (fun (World (c, chs, ts)) -> coord c, (ts, map coord chs))
              worlds
          in
          let utiles =
            NJfold
              ((fun (x, y) -> x @ y),
               map (fun ooo -> sml__hash__1 (sml__hash__2 ooo)) ulist, [])
          in
          let uvars = NJfold (tmerge, map variables utiles, []) in
          let tiles =
            (map newoccurrence uvars @ seq2tiles facts seq) @ utiles
          in
          let minc =
            foldr
              (fun ((_, cy as c), _) (_, miny as minc) ->
                 if cy < miny then c else minc)
              (0, 0) ulist
          in
          let u = mkmap ulist in
          let ws = map sml__hash__1 ulist in
          List.iter
            (fun ((_, cy as c), (ts, chs)) ->
               List.iter
                 (fun (_, chy as ch) ->
                    if chy <= cy then
                      raise
                        (ParseError_
                           ["(model2disproofstate) non-ascending link ";
                            coordstring c; " to "; coordstring ch; " in ";
                            universestring "" u])
                    else
                      match at (u, ch) with
                        Some (chts, _) ->
                          if not
                               (null (listsub (fun (x, y) -> x = y) ts chts))
                          then
                            raise
                              (ParseError_
                                 ["(model2disproofstate) non-monotonic link ";
                                  coordstring c; " to "; coordstring ch;
                                  " in "; universestring "" u])
                      | None ->
                          raise
                            (ParseError_
                               ["(model2disproofstate) broken link ";
                                coordstring c; " to "; coordstring ch; " in ";
                                universestring "" u]))
                 chs)
            ulist;
          Some
            (evaldisproofstate facts tree
               (Disproofstate
                  (let module M =
                     struct
                       class a =
                         object
                           val seq = seq
                           val universe = mkmap ulist
                           val tiles = tilesort tiles
                           val selected = [minc]
                           val conclusive = false
                           val forcemap = empty
                           val countermodel = false
                           method seq = seq
                           method universe = universe
                           method tiles = tiles
                           method selected = selected
                           method conclusive = conclusive
                           method forcemap = forcemap
                           method countermodel = countermodel
                         end
                     end
                   in
                   new M.a)))
    let rec checkdisproof facts tree disproofopt =
      match model2disproofstate facts tree disproofopt with
        Some (Disproofstate {countermodel = countermodel}) -> countermodel
      | _ -> false
    let rec disproof_start facts tree pathopt hyps =
      let rec replace_hyps s hyps =
        let (style, _, _, _, concs) = my_seqexplode s in
        mkSeq (style, hyps, concs)
      in
      let rec res s' =
        let (syn, _, hyps, _, concs) = my_seqexplode s' in
        let tiles = seq2tiles facts s' in
        let seq =
          match getsemanticturnstile syn with
            Some sem -> mkSeq (sem, hyps, concs)
          | None -> s'
        in
        evaldisproofstate facts tree
          (Disproofstate
             (let module M =
                struct
                  class a =
                    object
                      val seq = seq
                      val universe = emptyworld ()
                      val tiles = tilesort tiles
                      val selected = [0, 0]
                      val conclusive = false
                      val forcemap = empty
                      val countermodel = false
                      method seq = seq
                      method universe = universe
                      method tiles = tiles
                      method selected = selected
                      method conclusive = conclusive
                      method forcemap = forcemap
                      method countermodel = countermodel
                    end
                end
              in
              new M.a))
      in
      match pathopt with
        None ->
          let s = base_sequent tree in
          begin match hyps with
            [] -> res s
          | hs -> res (replace_hyps s hs)
          end
      | Some path ->
          let tree' = subtree tree path in
          let s' = base_sequent tree' in
          res (if null hyps then s' else replace_hyps s' hyps)
    let rec newtile =
      fun (Disproofstate {tiles = tiles} as d) t ->
        let rec indivs t =
          let varstrings = map termstring (variables t) in
          let longest =
            foldl
              (fun long v ->
                 if String.length v > String.length long then v else long)
              "" varstrings
          in
          let appendix = ref "" in
          let template =
            mapterm
              (fun t ->
                 if not (bracketed t) && isVariable t then
                   begin
                     appendix := !appendix ^ "'";
                     Some (parseTerm (longest ^ !appendix))
                   end
                 else None)
              t
          in
          template, variables template
        in
        let rec newtile v v' =
          andthenr
            (option_remapterm (( |-> ) (v, v')) t,
             (fun t' -> if member (t', tiles) then None else Some t'))
        in
        let rec newoccurrence v =
          let cs = explode (termstring v) in
          let ds = rev (takewhile isdigit (rev cs)) in
          let stem = implode (take (length cs - length ds) cs) in
          let stern = if null ds then 1 else atoi (implode ds) + 1 in
          let rec freshtile stem stern =
            let v' = parseTerm (stem ^ makestring stern) in
            ortryr (newtile v v', (fun _ -> freshtile stem (stern + 1)))
          in
          freshtile stem stern
        in
        let (template, tvs) = indivs t in
        let occs = ( <| ) (isoccurrence, tiles) in
        let occvs = NJfold (tmerge, map variables occs, []) in
        if null tvs then None
        else
          andthenr
            ((if member (t, occs) then
                match variables t with
                  [v] -> newoccurrence v
                | vs ->
                    raise
                      (Catastrophe_
                         ["(newtile) occurrence tile "; termstring t;
                          " has termvars ";
                          bracketedliststring termstring "," vs])
              else
                let rec nlists a1 a2 =
                  match a1, a2 with
                    xs, 0 -> ([[]] : term list list)
                  | (xs : term list), n ->
                      (let ys : term list list = nlists xs (n - 1) in
                       let rec insert a1 a2 =
                         match a1, a2 with
                           i, [] -> [[i]]
                         | i, j :: js ->
                             (i :: j :: js) ::
                               map (fun js -> j :: js) (insert i js)
                       in
                       NJfold
                         ((fun (x, y) -> x @ y),
                          NJfold
                            ((fun (x, y) -> x @ y),
                             (map
                                (fun y ->
                                   (map (fun x -> insert x y) xs :
                                    term list list list))
                                ys :
                              term list list list list),
                             []),
                          []) :
                       term list list)
                in
                let args = set (nlists occvs (length tvs)) in
                let possibles =
                  ( <| )
                    ((fun t -> not (member (t, tiles))),
                     map
                       (fun vs ->
                          unSOME
                            (option_remapterm (mkmap (( ||| ) (tvs, vs)))
                               template))
                       args)
                in
                match possibles with
                  [] ->
                    showAlert
                      ["Can't make a new tile from "; termstring t;
                       ", because you can only use ";
                       liststring2 termstring ", " " and " occs];
                    None
                | [p] -> Some p
                | _ ->
                    try
                      andthenr
                        (askChoice
                           ("Choose your new tile",
                            map (fun t -> [t])
                              (sort (fun (x, y) -> x < y)
                                 (map termstring possibles))),
                         (fun i -> Some (nth (possibles, i))))
                    with
                      Nth -> raise (Catastrophe_ ["(newtile) Nth ..."])),
             (fun tile ->
                Some (withdisprooftiles (d, tilesort (tile :: tiles)))))
    let rec addchild u (_, py as pc) (_, cy as cc) =
      if py >= cy then None
      else
        match at (u, pc) with
          None ->
            raise
              (Catastrophe_
                 ["(addchild) no parent world at "; coordstring pc])
        | Some (pts, pcs) ->
            if member (cc, pcs) then None
            else
              (* already there *)
              let u' =
                ( ++ )
                  (u,
                   ( |-> ) (pc, (pts, (if py < cy then cc :: pcs else pcs))))
              in
              match at (u', cc) with
                None -> Some (( ++ ) (u', ( |-> ) (cc, (pts, []))))
              | Some _ -> Some (domono pts u' cc)
    let rec deleteworld =
      fun (Disproofstate {universe = universe; selected = selected} as state)
        c ->
        match at (universe, c) with
          None -> None
        | Some _ ->
            (* a garbage-collection job! But actually we just leave the floaters floating ... *)
            let ws = listsub (fun (x, y) -> x = y) (dom universe) [c] in
            match ws with
              [] -> None
            | _ ->
                (* we don't delete the last world *)
                let rec doit w =
                  let (ts, cs) = getworld universe w in
                  w, (ts, listsub (fun (x, y) -> x = y) cs [w])
                in
                let u' = mkmap (map doit ws) in
                Some
                  (withdisproofselected
                     (withdisproofuniverse (state, u'),
                      listsub (fun (x, y) -> x = y) selected [c]))
    let rec worldselect =
      fun (Disproofstate {selected = selected} as d) cs ->
        if null (listsub (fun (x, y) -> x = y) selected cs) &&
           null (listsub (fun (x, y) -> x = y) cs selected)
        then
          None
        else Some (withdisproofselected (d, cs))
    let rec addlink u from to__ =
      let (ts, cs) = getworld u from in
      if member (to__, cs) then u
      else ( ++ ) (u, ( |-> ) (from, (ts, to__ :: cs)))
    let rec deletelink u from to__ =
      let (ts, cs) = getworld u from in
      if member (to__, cs) then
        ( ++ )
          (u, ( |-> ) (from, (ts, listsub (fun (x, y) -> x = y) cs [to__])))
      else u
    let rec children u c = sml__hash__2 (getworld u c)
    let rec parents u c = ( <| ) ((fun p -> member (c, children u p)), dom u)
    let rec moveworld =
      fun (Disproofstate {universe = u; selected = sels} as d) from
        (_, toy as to__) ->
        if from = to__ then None
        else
          match at (u, from) with
            None ->
              raise
                (Catastrophe_
                   ["(moveworld) no world at "; coordstring from; " in ";
                    universestring "" u])
          | Some (fromts, fromcs) ->
              (* split all (c->from) contrary links to point parent->from and from->child *)
              let rec splitlink fromcs u (_, cy as c) =
                let rec linkparent u (_, py as p) =
                  if py < toy then addlink u p from
                  else foldl linkparent u (parents u p)
                in
                let rec linkchild u ch = addlink u from ch in
                if cy >= toy && member (from, children u c) then
                  foldl linkchild
                    (deletelink (foldl linkparent u (parents u c)) c from)
                    fromcs
                else u
              in
              let u =
                foldl (splitlink fromcs) u
                  (listsub (fun (x, y) -> x = y) (dom u) [from])
              in
              (* all (from->c) contrary links split into parent->c and c->child *)
              let rec splitlink2 fromps (u, cs) (_, cy as c) =
                if c = to__ then u, cs
                else if(* no self links, please *)  toy >= cy then
                  let rec linkparent u p = addlink u p c in
                  let rec linkchild u (_, chy as ch) =
                    if chy < toy then addlink u c ch
                    else foldl linkchild u (children u ch)
                  in
                  foldl linkchild (foldl linkparent u fromps) (children u c),
                  cs
                else u, c :: cs
              in
              let (u, fromcs) =
                foldl (splitlink2 (parents u from)) (( -- ) (u, [from]), [])
                  fromcs
              in
              let rec swing c = if c = from then to__ else c in
              let ulist = ( <| ) ((fun (c, _) -> c <> from), aslist u) in
              let u =
                match at (u, to__) with
                  None ->
                    (* it's a new position: swing links from->to *)
                    ( ++ )
                      (mkmap
                         (map (fun (c, (ts, cs)) -> c, (ts, map swing cs))
                            ulist),
                       ( |-> ) (to__, (fromts, fromcs)))
                | Some (tots, tocs) ->
                    (* it's already there; delete unnecessary ->from links and unite the worlds *)
                    let u =
                      mkmap
                        (map
                           (fun (c, (ts, cs)) ->
                              c,
                              (ts,
                               (if member (to__, cs) then
                                  ( <| ) ((fun c -> c <> from), cs)
                                else map swing cs)))
                           ulist)
                    in
                    domono fromts
                      (( ++ )
                         (u,
                          ( |-> )
                            (to__,
                             (tots,
                              tocs @
                                listsub (fun (x, y) -> x = y) fromcs tocs))))
                      to__
              in
              let sels =
                if member (to__, sels) then
                  listsub (fun (x, y) -> x = y) sels [from]
                else map swing sels
              in
              Some (withdisproofselected (withdisproofuniverse (d, u), sels))
    let rec showdisproof =
      fun
        (Disproofstate
           {seq = seq;
            universe = universe;
            tiles = tiles;
            selected = selected;
            forcemap = forcemap}) ->
        (*  val _ = consolereport["showdisproof: seq is ", smlseqstring seq, 
                              "\nforcemap is ", mappingstring (pairstring coordstring smltermstring ",") 
                                                              (catelim2stringfn catelim_forcedstring) forcemap
                         ]
                  *)
        let root =
          match selected with
            [root] -> root
          | _ -> 0, 0
        in
        (* the value doesn't matter: forcemap will be empty *)
        let rec realterm t =
          match term2binding t with
            Some t' -> t'
          | _ -> t
        in
        let rec ivb t =
          let t = realterm t in
          (* consolereport ["ivb ", smltermstring t, " => ", 
               optionstring (catelim2stringfn catelim_forcedstring) (forcemap at (root,t))]; 
           *)
          match at (forcemap, (root, t)) with
            Some (on, lock) ->
              sml__hash__1 (if on then onbraket else offbraket) ^
                (if lock then sml__hash__1 lockbraket else "")
          | None -> sml__hash__1 outbraket
        in
        let rec ivk t =
          let t = realterm t in
          (* consolereport ["ivk ", smltermstring t, " => ", makestring (forced facts universe root t)]; *)
          match at (forcemap, (root, t)) with
            Some (on, lock) ->
              (if lock then sml__hash__2 lockbraket else "") ^
                sml__hash__2 (if on then onbraket else offbraket)
          | None -> sml__hash__2 outbraket
        in
        let (seqplan, seqbox) =
          makeseqplan (elementstring_invischoose ivb ivk) true origin seq
        in
        let seqsize = tbSize seqbox in
        let seqboxpos = pos (- (tsW seqsize / 2), - tsD seqsize) in
        drawindisproofpane ();
        setdisproofseqbox (textbox2box (tbOffset seqbox seqboxpos));
        seqdraw seqboxpos seqbox seqplan;
        setdisprooftiles (map termstring tiles);
        setdisproofworlds selected
          (map (fun (c, labels, ws) -> c, labelsort labels, ws)
             (universe2list universe));
        (* put the emphasis in *)
        List.iter
          (fun plan ->
             match planinfo plan with
               ElementClass (el, _) ->
                 let emph =
                   match element2term el with
                     None -> false
                   | Some t ->
                       match at (forcemap, (root, t)) with
                         Some (v, _) -> v
                       | None -> false
                 in
                 emphasise (seqelementpos seqboxpos seqbox plan) emph
             | _ -> ())
          seqplan
    let cleardisproof = clearDisproofPane
    (* for export, with slightly altered semantics *)
    let rec deletelink u from to__ =
      let (ts, cs) = getworld u from in
      if member (to__, cs) then
        Some
          (( ++ )
             (u,
              ( |-> ) (from, (ts, listsub (fun (x, y) -> x = y) cs [to__]))))
      else None
  end
