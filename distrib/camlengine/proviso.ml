(* $Id$ *)

module type Provisotype =
  sig
    type term
    type proviso =
        FreshProviso of (bool * bool * bool * term)
      | UnifiesProviso of (term * term)
      | NotinProviso of (term * term)
      | NotoneofProviso of (term list * term * term)
  end

module type Proviso =
  sig
    type term and vid and proviso
    val parseProvisos : unit -> proviso list
    (* yes, really a list - it has to translate x,y NOTIN A, B into
     * x NOTIN A AND x NOTIN B AND y NOTIN A AND y NOTIN B; similarly
     * FRESH and all its derivatives
     *)

    val provisostring : proviso -> string
    val catelim_provisostring : proviso -> string list -> string list
    val earlierproviso : proviso * proviso -> bool
    val provisovars : (term -> 'a) -> ('a * 'a -> 'a) -> proviso -> 'a
    val provisoVIDs : proviso -> vid list
    val isFreshProviso : proviso -> bool
    val maxprovisoresnum : proviso -> int
    val provisodebug : bool ref
    (* now an abstract datatype *)
    type visproviso
    val mkvisproviso : bool * proviso -> visproviso
    val mkparentedvisproviso : proviso -> bool * proviso -> visproviso
    val provisovisible : visproviso -> bool
    val provisoparent : visproviso -> proviso
    val provisoactual : visproviso -> proviso
    val provisoresetactual : visproviso -> proviso -> visproviso
    val provisoselfparent : visproviso -> visproviso
    val visprovisostring : visproviso -> string
    val visprovisostringall : visproviso -> string
  end
(* $Id$ *)

module
  Proviso
  (AAA :
    sig
      module listfuns : Listfuns
      module symboltype : Symboltype
      module term : sig include Termtype include Termstore include Term end
      module termparse : Termparse
      exception ParseError_ of string list
      exception Catastrophe_ of string list
      val commasymbol : termparse.symbol
      val currsymb : unit -> termparse.symbol
      val listclass : term.idclass
      val mkBag : termparse.element list -> termparse.term
      val scansymb : unit -> termparse.symbol
      val smlsymbolstring : termparse.symbol -> string
      
    end)
  :
  sig include Provisotype include Proviso end =
  struct
    open AAA
    open listfuns open symboltype open term open termparse
    (* from listfuns *)
    
    
    
    
    let provisodebug = ref false
    type proviso =
        FreshProviso of (bool * bool * bool * term)
      | UnifiesProviso of (term * term)
      | NotinProviso of (term * term)
      | NotoneofProviso of (term list * term * term)
    (* Meaning of provisos at present :
       FreshProviso (h, g, r, v)    : Variable v doesn't occur free in hypotheses (if h), 
                                      conclusions (if g); non-principal formulae only (if r)
       UnifiesProviso (t1, t2)      : t1 must unify with t2.
       NotinProviso (v, t)          : variable v must not occur free in t
       NotoneofProviso (vs, pat, C) : in any element of collection C that matches pat,
                                      variables vs don't occur in the places indicated
                                      by pat.
     *)
                          
    let rec catelim_provisostring p tail =
      match p with
        FreshProviso (h, g, r, v) ->
          (if r then "IMP" else "") ::
            (match h, g with
               true, true -> ""
             | true, false -> "HYP"
             | false, true -> "CONC"
             | false, false -> "????") ::
            "FRESH " :: catelim_termstring v tail
      | UnifiesProviso (t1, t2) ->
          let good =
            List.exists
              (function
                 Segvar _ -> true
               | _ -> false)
          in
          let easy =
            match t1, t2 with
              Collection (_, _, e1s), Collection (_, _, e2s) ->
                good e1s || good e2s
            | Collection (_, _, e1s), _ -> good e1s
            | _, Collection (_, _, e2s) -> good e2s
            | _ -> true
          in
          let tstring =
            if easy then catelim_termOrCollectionstring ","
            else catelim_termstring
          in
          tstring t1 (" UNIFIESWITH " :: tstring t2 tail)
      | NotinProviso (t1, t2) ->
          catelim_termstring t1
            (" NOTIN " :: catelim_termOrCollectionstring "," t2 tail)
      | NotoneofProviso (vs, pat, C) ->
          (* this madness IS used! *)
          catelim_liststring catelim_termstring "," vs
            (" IN " ::
               catelim_termstring pat
                 (" NOTONEOF " :: catelim_collectionstring "," C tail))
    let provisostring = catelim2stringfn catelim_provisostring
    let rec isFreshProviso =
      function
        FreshProviso _ -> true
      | _ -> false
    type visproviso =
      VisProviso of < visible : bool; parent : proviso; actual : proviso >
    let rec mkvisproviso (vis, pro) =
      VisProviso
        (let module M =
           struct
             class a =
               object
                 val visible = vis
                 val parent = pro
                 val actual = pro
                 method visible = visible
                 method parent = parent
                 method actual = actual
               end
           end
         in
         new M.a)
    let rec mkparentedvisproviso parent (vis, pro) =
      VisProviso
        (let module M =
           struct
             class a =
               object
                 val visible = vis
                 val parent = parent
                 val actual = pro
                 method visible = visible
                 method parent = parent
                 method actual = actual
               end
           end
         in
         new M.a)
    let rec provisovisible = fun (VisProviso {visible = visible}) -> visible
    let rec provisoparent = fun (VisProviso {parent = parent}) -> parent
    let rec provisoactual = fun (VisProviso {actual = actual}) -> actual
    let rec provisoresetactual =
      fun (VisProviso {visible = visible; parent = parent}) actual ->
        VisProviso
          (let module M =
             struct
               class a =
                 object
                   val visible = visible
                   val actual = actual
                   val parent = parent
                   method visible = visible
                   method actual = actual
                   method parent = parent
                 end
             end
           in
           new M.a)
    let rec provisoselfparent =
      fun (VisProviso {visible = visible; actual = actual}) ->
        VisProviso
          (let module M =
             struct
               class a =
                 object
                   val visible = visible
                   val parent = actual
                   val actual = actual
                   method visible = visible
                   method parent = parent
                   method actual = actual
                 end
             end
           in
           new M.a)
    let rec catelim_visprovisostring a1 a2 =
      match a1, a2 with
        VisProviso {visible = true; actual = p}, tail ->
          catelim_provisostring p tail
      | VisProviso {visible = false; actual = p}, tail ->
          "<<" :: catelim_provisostring p (">>" :: tail)
    let rec catelim_visprovisostringall =
      fun (VisProviso {visible = visible; parent = parent; actual = actual})
        tail ->
        "{" :: string_of_int visible :: ", actual=" ::
          catelim_provisostring actual
            (", parent=" :: catelim_provisostring parent ("}" :: tail))
    let visprovisostring = catelim2stringfn catelim_visprovisostring
    let visprovisostringall = catelim2stringfn catelim_visprovisostringall
    let rec stripElement =
      function
        Element (_, _, t) -> t
      | s ->
          raise
            (Catastrophe_
               ["stripElement (proviso) called on Segvar ";
                termstring (mkBag [s])])
    let rec checkNOTINvars pname vars =
      List.iter
        (fun t ->
           if (isId t || isVariable t) || isconstant t then ()
           else
             raise
               (ParseError_
                  [termstring t; " in "; pname; " proviso is neither an identifier nor an unknown variable or constant"]))
        vars
    let rec parseNOTINvars pname =
      scansymb ();
      let vs = parseList (fun _ -> true) parseTerm commasymbol in
      checkNOTINvars pname vs; vs
    let rec parseProvisos () =
      let rec parseNOTONEOF vars =
        let rec bad ss =
          raise (ParseError_ ("in NOTONEOF proviso, " :: ss))
        in
        let _ =
          if List.exists (fun ooo -> not (ismetav ooo)) vars then
            bad
              ["not all the names "; bracketedliststring termstring ", " vars;
               " are schematic"]
        in
        let _ = check (SHYID "IN") in
        let pat = parseBindingpattern () in
        let varsinpat = ( <| ) (ismetav, termvars pat) in
        let _ =
          if not (subset (vars, varsinpat)) then
            bad
              ["not all the names "; bracketedliststring termstring ", " vars;
               " appear in the pattern "; termstring pat]
        in
        let _ = check (SHYID "NOTONEOF") in
        let (classopt, els) =
          parseElementList canstartTerm parseTerm commasymbol None
        in
        let class__ =
          match classopt with
            Some c -> c
          | None -> listclass
        in
        NotoneofProviso (vars, pat, registerCollection (class__, els))
      in
      let rec parseNOTIN vars =
        let _ = checkNOTINvars "NOTIN" vars in
        let _ = check (SHYID "NOTIN") in
        let (class__, els) =
          parseElementList canstartTerm parseTerm commasymbol None
        in
        let terms =
          match class__ with
            None -> m_a_p (stripElement, els)
          | Some k -> [registerCollection (k, els)]
        in
        m_a_p (NotinProviso, ( >< ) (vars, terms))
      in
      let rec freshp p h g r v = p (h, g, r, v) in
      match currsymb () with
        SHYID "FRESH" ->
          m_a_p (freshp FreshProviso true true false, parseNOTINvars "FRESH")
      | SHYID "HYPFRESH" ->
          m_a_p
            (freshp FreshProviso true false false, parseNOTINvars "HYPFRESH")
      | SHYID "CONCFRESH" ->
          m_a_p
            (freshp FreshProviso false true false, parseNOTINvars "CONCFRESH")
      | SHYID "IMPFRESH" ->
          m_a_p (freshp FreshProviso true true true, parseNOTINvars "FRESH")
      | SHYID "IMPHYPFRESH" ->
          m_a_p (freshp FreshProviso true false true, parseNOTINvars "HYPFRESH")
      | SHYID "IMPCONCFRESH" ->
          m_a_p
            (freshp FreshProviso false true true, parseNOTINvars "CONCFRESH")
      | sy ->
          if canstartTerm sy then
            let (class__, els) =
              parseElementList canstartTerm parseTerm commasymbol None
            in
            let rec bk =
              function
                [el] -> termstring (mkBag [el])
              | els -> ("[" ^ termstring (mkBag els)) ^ "]"
            in
            let rec collbad s =
              raise
                (ParseError_
                   ["when parsing a proviso, symbol "; s;
                    " found after collection "; bk els])
            in
            match class__, currsymb () with
              None, SHYID "NOTIN" -> parseNOTIN (m_a_p (stripElement, els))
            | None, SHYID "IN" -> [parseNOTONEOF (m_a_p (stripElement, els))]
            | _, SHYID "NOTIN" -> collbad "NOTIN"
            | _, SHYID "IN" -> collbad "IN"
            | _, SHYID "UNIFIESWITH" ->
                let _ = scansymb () in
                let (class', els') =
                  parseElementList canstartTerm parseTerm commasymbol class__
                in
                let (t1, t2) =
                  match class', els, els' with
                    Some k, _, _ ->
                      registerCollection (k, els),
                      registerCollection (k, els')
                  | None, [el], [el'] -> stripElement el, stripElement el'
                  | _ ->
                      raise
                        (ParseError_
                           ["can't determine collection class of "; bk els;
                            " and "; bk els'])
                in
                [UnifiesProviso (t1, t2)]
            | _ ->
                raise
                  (ParseError_
                     ["comma, IN, NOTIN or UNIFIESWITH expected after ";
                      liststring elementstring ", " els; " in provisos"])
          else
            raise
              (ParseError_
                 ["Proviso -- FRESH.. or HYPFRESH.. or CONCFRESH.. ";
                  "or formula UNIFIESWITH formula or ids NOTIN terms or ";
                  "var IN pattern NOTONEOF collection -- expected, ";
                  "found "; smlsymbolstring sy])
    (* function for sorting proviso lists so that they are nice and readable *)
    let rec earlierproviso (p1, p2) =
      let rec lin1 =
        function
          FreshProviso (h, g, r, v) ->
            (if h then 10 else 0) + (if g then 20 else 0) +
              (if r then 40 else 0)
        | NotinProviso (v, t) -> 300
        | NotoneofProviso (vs, pat, C) -> 400
        | UnifiesProviso (t, t') -> 500
      in
      let rec lin2 =
        function
          FreshProviso (h, g, r, v) -> [termstring v]
        | NotinProviso (v, t) -> [termstring v; termstring t]
        | NotoneofProviso (vs, pat, C) ->
            nj_fold (fun (v, ss) -> termstring v :: ss) vs
              [termstring pat; termstring C]
        | UnifiesProviso (t, t') -> [termstring t; termstring t']
      in
      let n1 = lin1 p1 in
      let n2 = lin1 p2 in
      n1 < n2 ||
      n1 = n2 && earlierlist (fun (x, y) -> x < y) (lin2 p1, lin2 p2)
    let rec provisovars termvars tmerge p =
      match p with
        FreshProviso (_, _, _, t) -> termvars t
      | UnifiesProviso (t1, t2) -> tmerge (termvars t1, termvars t2)
      | NotinProviso (t1, t2) -> tmerge (termvars t1, termvars t2)
      | NotoneofProviso (vs, pat, C) ->
          nj_fold tmerge (m_a_p (termvars, vs)) (termvars C)
    let rec provisoVIDs p =
      orderVIDs (m_a_p (vartoVID, provisovars termvars tmerge p))
    let rec maxprovisoresnum p =
      let rec f t n =
        nj_fold max (m_a_p (resnum2int, elementnumbers t)) n
      in
      match p with
        FreshProviso (_, _, _, t) -> f t 0
      | UnifiesProviso (t1, t2) -> f t1 (f t2 0)
      | NotinProviso (v, t) -> f v (f t 0)
      | NotoneofProviso (vs, pat, C) -> nj_fold (fun (v, n) -> f v n) vs (f C 0)
  end
