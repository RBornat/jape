(* $Id$ *)

module type Provisotype =
  sig
    type term and vid
    type proviso =
        FreshProviso of (bool * bool * bool * term)
      | UnifiesProviso of (term * term)
      | NotinProviso of (term * term)
      | NotoneofProviso of (term list * term * term)

    val parseProvisos : unit -> proviso list
    (* yes, really a list - it has to translate x,y NOTIN A, B into
     * x NOTIN A AND x NOTIN B AND y NOTIN A AND y NOTIN B; similarly
     * FRESH and all its derivatives
     *)

    val provisostring : proviso -> string
    val catelim_provisostring : proviso -> string list -> string list
    val earlierproviso : proviso * proviso -> bool
    val provisovars : (term -> 'a) -> ('a -> 'a -> 'a) -> proviso -> 'a
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

module type T =
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

module M : Provisotype with type vid = Term.Type.vid
                        and type term = Term.Type.term
 =
  struct
    open Listfuns.M 
    open Miscellaneous.M
    open SML.M
    open Symbol.M
    open Symboltype.M 
    open Term.Funs 
    open Term.Store
    open Term.Termstring
    open Term.Type
    open Termparse.M
    
    type vid = Term.Type.vid
     and term = Term.Type.term
     
	let mkBag els = 
	  registerCollection(Idclass.M.BagClass Idclass.M.FormulaClass,els)
    
    let listclass = Idclass.M.ListClass Idclass.M.FormulaClass
    
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
       NotoneofProviso (vs, pat, _C) : in any element of collection _C that matches pat,
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
      | NotoneofProviso (vs, pat, _C) ->
          (* this madness IS used! *)
          catelim_liststring catelim_termstring "," vs
            (" IN " ::
               catelim_termstring pat
                 (" NOTONEOF " :: catelim_collectionstring "," _C tail))
    let provisostring = catelim2stringfn catelim_provisostring
    let rec isFreshProviso =
      function
        FreshProviso _ -> true
      | _ -> false
    
    type visrec = { visible : bool; parent : proviso; actual : proviso }
    type visproviso = VisProviso of visrec
    
    let rec mkvisproviso (vis, pro) = 
      VisProviso { visible=vis; parent=pro; actual=pro }
    let rec mkparentedvisproviso parent (vis, pro) =
      VisProviso { visible=vis; parent=parent; actual=pro }

    let rec provisovisible (VisProviso v) = v.visible
    let rec provisoparent (VisProviso v) = v.parent
    let rec provisoactual (VisProviso v) = v.actual
    
    let rec provisoresetactual (VisProviso v) actual = 
        VisProviso {v with actual=actual}
    let rec provisoselfparent (VisProviso v) =
        VisProviso {v with parent=v.actual}

    let rec catelim_visprovisostring a1 a2 =
      match a1, a2 with
        VisProviso {visible = true; actual = p}, tail ->
          catelim_provisostring p tail
      | VisProviso {visible = false; actual = p}, tail ->
          "<<" :: catelim_provisostring p (">>" :: tail)
    
    let rec catelim_visprovisostringall =
      fun (VisProviso {visible = visible; parent = parent; actual = actual})
        tail ->
        "{" :: string_of_bool visible :: ", actual=" ::
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
      let _ = scansymb () in
      let vs = parseList (fun _ -> true) parseTerm commasymbol in
      checkNOTINvars pname vs; vs
    let rec parseProvisos () =
      let rec parseNOTONEOF vars =
        let rec bad ss =
          raise (ParseError_ ("in NOTONEOF proviso, " :: ss))
        in
        let _ =
          if List.exists (not <*> ismetav) vars then
            bad
              ["not all the names "; bracketedliststring termstring ", " vars;
               " are schematic"]
        in
        let _ = check (SHYID "IN") in
        let pat = parseBindingpattern () in
        let varsinpat = ismetav <| termvars pat in
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
            None -> (stripElement <* els)
          | Some k -> [registerCollection (k, els)]
        in
        ((fun v->NotinProviso v) <* (vars >< terms))
      in
      let rec freshp p h g r v = p (h, g, r, v) in
      match currsymb () with
        SHYID "FRESH" ->
          freshp (fun v->FreshProviso v) true true false <* parseNOTINvars "FRESH"
      | SHYID "HYPFRESH" ->
          freshp (fun v->FreshProviso v) true false false <* parseNOTINvars "HYPFRESH"
      | SHYID "CONCFRESH" ->
          freshp (fun v->FreshProviso v) false true false <* parseNOTINvars "CONCFRESH"
      | SHYID "IMPFRESH" ->
          (freshp (fun v->FreshProviso v) true true true <* parseNOTINvars "FRESH")
      | SHYID "IMPHYPFRESH" ->
          (freshp (fun v->FreshProviso v) true false true <* parseNOTINvars "HYPFRESH")
      | SHYID "IMPCONCFRESH" ->
          freshp (fun v->FreshProviso v) false true true <* parseNOTINvars "CONCFRESH"
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
              None, SHYID "NOTIN" -> parseNOTIN ((stripElement <* els))
            | None, SHYID "IN" -> [parseNOTONEOF ((stripElement <* els))]
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
        | NotoneofProviso (vs, pat, _C) -> 400
        | UnifiesProviso (t, t') -> 500
      in
      let rec lin2 =
        function
          FreshProviso (h, g, r, v) -> [termstring v]
        | NotinProviso (v, t) -> [termstring v; termstring t]
        | NotoneofProviso (vs, pat, _C) ->
            nj_fold (fun (v, ss) -> termstring v :: ss) vs
              [termstring pat; termstring _C]
        | UnifiesProviso (t, t') -> [termstring t; termstring t']
      in
      let n1 = lin1 p1 in
      let n2 = lin1 p2 in
      n1 < n2 ||
      n1 = n2 && earlierlist (<) (lin2 p1) (lin2 p2)
    let rec provisovars termvars tmerge p =
      match p with
        FreshProviso (_, _, _, t) -> termvars t
      | UnifiesProviso (t1, t2) -> tmerge (termvars t1) (termvars t2)
      | NotinProviso (t1, t2) -> tmerge (termvars t1) (termvars t2)
      | NotoneofProviso (vs, pat, _C) ->
          nj_fold (uncurry2 tmerge) ((termvars <* vs)) (termvars _C)
    let rec provisoVIDs p =
      orderVIDs ((vartoVID <* provisovars termvars tmerge p))
    let rec maxprovisoresnum p =
      let rec f t n =
        nj_fold (uncurry2 max) ((resnum2int <* elementnumbers t)) n
      in
      match p with
        FreshProviso (_, _, _, t) -> f t 0
      | UnifiesProviso (t1, t2) -> f t1 (f t2 0)
      | NotinProviso (v, t) -> f v (f t 0)
      | NotoneofProviso (vs, pat, _C) -> nj_fold (fun (v, n) -> f v n) vs (f _C 0)
  end
