(* $Id$ *)

module type Provisofuns =
  sig
    type cxt
    and term
    and resnum
    and proviso
    and visproviso
    and element
    and ('a, 'b) mapping
    val checkprovisos : cxt -> cxt option
    val verifyprovisos : cxt -> cxt
    val expandFreshProviso :
      bool -> bool * bool * bool * term -> term -> term -> visproviso list ->
        visproviso list
    exception Verifyproviso_ of proviso
    val groundedprovisos :
      term list -> visproviso list -> visproviso list option
    val deferrable : cxt -> term * term -> bool
    val remapproviso : (term, term) mapping -> proviso -> proviso
  end
(* $Id$ *)

module
  Provisofuns
  (AAA :
    sig
      module listfuns : Listfuns
      module optionfuns : Optionfuns
      module answer : Answer
      module idclass : Idclass
      module term : sig include Termtype include Termstore include Term end
      module match : Match
      module proviso : sig include Provisotype include Proviso end
      module facts : Facts
      module substmapfuns : Substmapfuns
      val at : ('a, 'b) match.mapping * 'a -> 'b option
      val baseseqsides :
        facts.cxt ->
          term.term * term.term * (term.term list * term.term list) option
      val consolereport : string list -> unit
      val conVariableClass : term.idclass
      val cxtstring : facts.cxt -> string
      val empty : ('a, 'b) match.mapping
      val isdigit : string -> bool
      val pairstring :
        ('a -> string) -> ('b -> string) -> string -> 'a * 'b -> string
      val provisos : facts.cxt -> proviso.visproviso list
      val rewritecxt : facts.cxt -> facts.cxt
      val setprovisos : facts.cxt -> proviso.visproviso list -> facts.cxt
      exception Catastrophe_ of string list
      
    end)
  :
  Provisofuns =
  struct
    open AAA
    open listfuns
    open optionfuns
    open answer
    open idclass
    open term
    open match
    open proviso
    open substmapfuns
    open facts
    
    (* from listfuns *)
    
    
    
    
    (* from optionfuns *)
    
    (* from term *)
    
    let vv = bracketedliststring visprovisostring " AND "
    (* just turn a proviso into a list of simpler provisos 
       (function designed for folding over proviso list) 
     *)
    (* oh, and by the way, uncurried constructors and fold in SML are a PAIN. RB *)
    let rec simplifyProviso facts (p, cats) =
      let parent = provisoparent p in
      let vis = provisovisible p in
      let rec new__ p' = mkparentedvisproviso parent (vis, p') in
      let rec np v (t, cats) =
        let rec addone p' ps = new__ p' :: ps in
        let rec def () = Some (addone (NotinProviso (v, t)) cats) in
        let rec fnp (t, cats) = foldterm (np v) cats t in
        match t with
          Id _ -> def ()
        | Unknown _ -> def ()
        | Binding (_, (bs, ss, us), _, _) ->
            let rs = m_a_p (substeqvarsq facts v, bs) in
            if List.exists qDEF rs then Some (nj_fold fnp us cats)
            else if not (List.exists qUNSURE rs) then
              Some (nj_fold fnp ss (nj_fold fnp us cats))
            else def ()
        | Subst (_, r, P, vts) ->
            let rs = m_a_p (substeqvarsq facts v, m_a_p ((fun(hash1,_)->hash1), vts)) in
            if List.exists qDEF rs then
              Some (nj_fold fnp (m_a_p ((fun(_,hash2)->hash2), vts)) cats)
            else if not (List.exists qUNSURE rs) then
              match vts with
                [v', t'] ->
                  begin match varoccursinq facts v t' with
                    Yes -> Some (foldterm (np v') (fnp (P, cats)) P)
                  | No -> Some (fnp (P, cats))
                  | Maybe -> def ()
                  end
              | _ ->
                  Some
                    (nj_fold fnp
                       (m_a_p ((fun vt -> registerSubst (true, P, [vt])), vts))
                       cats)
            else def ()
        | Collection (_, c, es) ->
            let rec npe (e, cats) =
              match e with
                Segvar (_, _, v') ->
                  addone (NotinProviso (v, registerCollection (c, [e]))) cats
              | Element (_, _, t) -> fnp (t, cats)
            in
            Some (nj_fold npe es cats)
        | _ -> None
      in
      let rec nop =
        fun (vs, pat, C as n) ->
          let rec expand env (v, cats) =
            match at (env, v) with
              Some v' ->
                simplifyProviso facts (new__ (NotinProviso (v, v')), cats)
            | None -> raise (Catastrophe_ ["simplifyProviso nop"])
          in
          let rec simp t cats =
            match match3 false pat t (Certain empty) with
              Some (Certain env) -> Some (nj_fold (expand env) vs cats)
            | Some (Uncertain env) -> None
            | None -> Some cats
          in
          match C with
            Collection (_, class__, es) ->
              let rec doel (e, (defers, cats)) =
                match e with
                  Element (_, _, t) ->
                    begin match simp t cats with
                      Some cats' -> defers, cats'
                    | None -> e :: defers, cats
                    end
                | _ -> e :: defers, cats
              in
              let (defers, cats') = nj_fold doel es ([], cats) in
              if null defers then cats'
              else
                new__
                  (NotoneofProviso
                     (vs, pat, registerCollection (class__, defers))) ::
                  cats'
          | t ->
              match simp t cats with
                Some cats' -> cats'
              | _ -> new__ (NotoneofProviso n) :: cats
      in
      match provisoactual p with
        NotinProviso (v, t) -> foldterm (np v) cats t
      | NotoneofProviso n -> nop n
      | _ -> p :: cats
    let rec deferrable cxt (t1, t2) =
      simterms (t1, t2) &&
      (match t1, t2 with
         Subst _, Subst _ -> true
       | Subst (_, _, (Unknown _ as P1), vts), _ ->
           not (qDEF (varoccursinq cxt P1 t2)) ||
           specialisesto (idclass P1, conVariableClass) &&
           List.exists (fun t1' -> deferrable cxt (t1', t2))
             (m_a_p ((fun(_,hash2)->hash2), vts))
       | _, Subst _ -> deferrable cxt (t2, t1)
       | _ -> true)
    (* Is this proviso obviously satisfied (Yes) or obviously violated (No)
     * or can't we tell (Maybe)?
     * Make use of other, more basic, provisos as facts to help you decide, 
     * where appropriate - but be sure that you don't eliminate two mutually 
     * equivalent provisos (e.g. at this point don't use NOTINs to remove other NOTINs).
     *)
    let rec PROVISOq facts p =
      match p with
        FreshProviso _ ->(* can occur in a derived rule *)  Maybe
      | NotinProviso (v, t) -> notq (varoccursinq facts v t)
      | NotoneofProviso _ -> Maybe
      | UnifiesProviso (P1, P2) ->
          if eqterms (P1, P2) then Yes
          else if not (deferrable facts (P1, P2)) then No
          else(* if termoccursin (debracket P2) P1 
             orelse termoccursin (debracket P1) P2 then No
          else *)  Maybe
    (* the list of names must be sorted, which it will be if it comes out of termvars *)
    (* because of hidden provisos, this thing now gets a visproviso list *)
    (* iso means isolated, I think; so isot is an isolated term, isop an isolated proviso *)
    let rec groundedprovisos names provisos =
      let rec isot t =
        let vs = ( <| ) (ismetav, termvars t) in
        let rec diff a1 a2 =
          match a1, a2 with
            x :: xs, y :: ys ->
              earliervar (x, y) && diff xs (y :: ys) ||
              earliervar (y, x) && diff (x :: xs) ys
          | _, _ -> true
        in
        let r = diff vs names in
        if !provisodebug then
          consolereport
            ["isot checking "; termstring t; " against ";
             termliststring names; " => "; makestring r];
        r
      in
      let rec isop p =
        let r =
          match p with
            FreshProviso (_, _, _, v) -> ismetav v && isot v
          | NotinProviso (v, t) -> ismetav v && isot v || isot t
          | NotoneofProviso (vs, pat, C) ->
              not (List.exists (fun ooo -> not (isot ooo)) vs) || isot C
          | UnifiesProviso (P1, P2) -> isot P1 && isot P2
        in
        if !provisodebug then
          consolereport
            ["isop checking "; provisostring p; " => "; makestring r];
        r
      in
      let r =
        if null provisos then None
        else
          (* keep them? throw them away? *) match split (fun ooo -> isop (provisoactual ooo)) provisos with
            [], _ -> None
          | _, [] ->(* we kept them all *)
             Some []
          | isos, uses ->
              (* we threw them all away *)
              match
                groundedprovisos
                  (nj_fold tmerge
                     (m_a_p
                        ((fun ooo ->
                            provisovars termvars tmerge (provisoactual ooo)),
                         uses))
                     names)
                  isos
              with
                Some more -> Some (uses @ more)
              | None ->(* we kept some *)
                 None
      in
      if !provisodebug then
        consolereport
          ["groundedprovisos "; termliststring names; " ";
           bracketedliststring visprovisostring " AND " provisos; " => ";
           optionstring vv r];
      r
    let rec expandFreshProviso b (h, g, r, v) left right ps =
      nj_fold
        (function
           (true, side), ps -> mkvisproviso (b, NotinProviso (v, side)) :: ps
         | (false, side), ps -> ps)
        [h, left; g, right] ps
    exception Verifyproviso_ of proviso
    
    (* take out the first occurrence *)
    let rec ( -- ) =
      function
        x :: xs, y -> if x = y then xs else x :: ( -- ) (xs, y)
      | [], y -> []
    (* We find out which provisos from the set ps are independent of the set qs.
     * If op-- is the function above, p is deleted from the set qs before we start,
     * but there are other things we might want to do ...
     *)
    let rec checker cxt ( -- ) ps qs =
      let rec ch a1 a2 =
        match a1, a2 with
          [], qs -> []
        | p :: ps, qs ->
            let pp = provisoactual p in
            let qs' = ( -- ) (qs, p) in
            if List.exists (fun p' -> pp = provisoactual p') qs' then ch ps qs'
            else
              match PROVISOq (facts qs' cxt, pp) with
                Yes -> ch ps qs'
              | No -> raise (Verifyproviso_ (provisoparent p))
              | Maybe -> p :: ch ps qs
      in
      ch ps qs
    (* The correct interpretation of a FRESH proviso is given by expandFreshProviso above.  
     * But elsewhere -- see exterioreqvarsq -- I have taken FRESH to mean: doesn't appear 
     * free in the base sequent.  That second interpretation applies when proving theorems:
     * it's over-enthusiastic (too restrictive), but mixing them won't do: it causes us to
     * often have to add extra, strictly necessary, provisos to bolster FRESH.  Using the 
     * restrictive version includes those extra provisos all the time. 
     *
     * So now I use the restrictive version in verifyprovisos.
     * RB 20/i/00
     *)

    let rec verifyprovisos cxt =
      try
        let cxt = rewritecxt cxt in
        let (left, right, fvopt) = baseseqsides cxt in
        let rec efp (h, g, r, v as f) =
          match fvopt with
            Some (bhfvs, bcfvs) ->
              let rec notins a1 a2 a3 =
                match a1, a2, a3 with
                  true, fvs, ps ->
                    nj_fold
                      (fun (fv, ps) ->
                         mkvisproviso (true, NotinProviso (v, fv)) :: ps)
                      fvs ps
                | false, _, ps -> ps
              in
              notins h bhfvs (notins g bcfvs [])
          | None -> expandFreshProviso true f left right []
        in
        (* the checker function above is wierd.  But it checks the list of provisos
         * left-to-right, eliminating the ones that are a consequence of what is left,
         * so we want the _visible_ provisos first
         *)
        let (vis, invis) = split provisovisible (provisos cxt) in
        (* FreshProvisos are never invisible, I hope, so I just look at vis *)
        let (fresh, unfresh) =
          nj_fold
            (fun (p, (fs, us)) ->
               match provisoactual p with
                 FreshProviso f -> let ns = efp f in (f, ns) :: fs, ns @ us
               | _ -> fs, p :: us)
            vis ([], [])
        in
        let pros =
          let ps = unfresh @ invis in
          let simpleps = nj_fold (simplifyProviso (facts ps cxt)) ps [] in
          let _ =
            if !provisodebug then consolereport ["simpleps = "; vv simpleps]
          in
          checker cxt (fun (x, y) -> ( -- ) x y) simpleps simpleps
        in
        (* FreshProviso now interacts with other provisos in a number of ways.
         * If we have FRESH/IMPFRESH z, but z doesn't appear in the base sequent or the givens,
         * then we can replace it by just the provisos which derive from it.
         * If after that we still have FRESH z, then we can strip out all the provisos which 
         * derive from it.
         * If we still have IMPFRESH z, and we have all the provisos which derive from
         * it, then we can strip them out and use FRESH z.
         * If we already have a FRESH/IMPFRESH proviso then we can augment it to allow this one
         *)
        let ps =
          let rec dofresh (((h, g, r, v as f), ns), pros) =
            let news = nj_fold (simplifyProviso (facts (pros @ ns) cxt)) ns [] in
            let rec mm (xs, y) = xs in
            let rec def () = checker cxt mm pros news in
            let rec push f ps =
              if List.exists
                   (fun ooo ->
                      (function
                         FreshProviso (_, _, _, v') -> v = v'
                       | _ -> false)
                        (provisoactual ooo))
                   ps
              then
                m_a_p
                  ((fun vp ->
                      match provisoactual vp with
                        FreshProviso (h', g', r', v') ->
                          if v = v' then
                            mkvisproviso
                              (true,
                               FreshProviso (h || h', g || g', r && r', v))
                          else vp
                      | _ -> vp),
                   ps)
              else mkvisproviso (true, FreshProviso f) :: ps
            in
            if knownproofvar (facts pros cxt) v then def ()
            else if(* no need for it at all *)
             r then
              (* IMPFRESH - check if news are needed *)
              let news' =
                try checker cxt mm news pros with
                  Verifyproviso_ _ -> news
              in
              if null news' then(* we have the lot: just say FRESH *)
               push (h, g, false, v) (def ())
              else(* something missing: use IMPFRESH *)
               push f pros
            else(* FRESH *)
                                   (* take out copies of the new ones *)
             push f (def ())
          in
          nj_fold dofresh fresh pros
        in
        if !provisodebug then
          consolereport
            ["verifyprovisos "; vv (provisos cxt); " (pros = "; vv pros; ") ";
             " (vis = "; vv vis; ") "; " (invis = "; vv invis; ") ";
             " (fresh = ";
             bracketedliststring
               (fun (f, ns) ->
                  pairstring provisostring vv "," (FreshProviso f, ns))
               "," fresh;
             ") "; " (unfresh = "; vv unfresh; ") "; " => "; vv ps];
        rewritecxt (setprovisos cxt ps)
      with
        Verifyproviso_ p ->
          if !provisodebug then
            consolereport
              ["proviso "; provisostring p; " failed in verifyprovisos"];
          raise (Verifyproviso_ p)
    let rec checkprovisos cxt =
      try Some (verifyprovisos cxt) with
        Verifyproviso_ _ -> None
    (* this is for renaming provisos when a rule/theorem is instantiated *)
    let rec remapproviso env p =
      let T = remapterm env in
      match p with
        FreshProviso (h, g, r, v) -> FreshProviso (h, g, r, T v)
      | NotinProviso (v, t) -> NotinProviso (T v, T t)
      | NotoneofProviso (vs, pat, C) ->
          NotoneofProviso (m_a_p (T, vs), T pat, T C)
      | UnifiesProviso (P1, P2) -> UnifiesProviso (T P1, T P2)
    (* the drag and drop mapping is a list of (source,target) pairs, derived from
     * UnifiesProvisos thus:
     * if we have to unify two bag collections, either of which includes an unknown 
     * bag variable, then each such variable is a target and each of the formulae on 
     * the other side of the proviso is a source for that target.
     *)
    
    (* and currently it is a dead duck.  RB 30/vii/01
    fun draganddropmapping ps =
      let fun istarget (Segvar(_,_,Unknown _)) = true
          |   istarget _                       = false
          fun getsts (UnifiesProviso(Collection(_,BagClass FormulaClass, xs),
                                     Collection(_,BagClass FormulaClass, ys)
                                    ),
                      rs
                     ) = (xs,istarget<|ys)::(ys,istarget<|xs)::rs
          |   getsts (_,rs) = rs
          val sts = (not o null o #2) <| nj_fold getsts ps []
          (* We need to run Warshall's algorithm to get the transitive closure.
           * This implementation is slow, but these collections don't get large 
           *)
          val ists = (not o null o #1) <| ((fn (xs,ys) => (istarget<|xs,ys)) m_a_p sts)
          val ists = nj_fold (fn ((ss,ts),rs) => nj_fold (fn (s,rs) => (s,ts)::rs) ss rs) ists []
          val sts = nj_fold (fn ((s,ts),sts) => 
                             (fn (ss',ts') => 
                                 (ss',if List.exists (fn t' => s=t') ts' then ts'@ts else ts')
                             ) m_a_p sts
                         ) ists sts
      in 
          nj_fold (fn ((ss,ts),rs) => (ss><ts)@rs) sts []
      end
    *)
     
   (* for export *)
    let deferrable cxt = deferrable (facts (provisos cxt) cxt)
  end

