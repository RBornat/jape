(* $Id$ *)

module type Substmapfuns =
  sig
    (* Oh at this point I wish for a type which distinguishes
       variables from terms.  Even though it might be a pain everywhere
       else. RB
     *)
     
    type term and answer and facts
    val substdebug : bool ref
    val varmappedbyq : facts -> term -> (term * term) list -> answer
    val varboundbyq : facts -> term -> term list -> answer
    val varoccursinq : facts -> term -> term -> answer
    val varmappedto : facts -> term -> (term * term) list -> term option
    val simplifySubstAnyway : facts -> (term * term) list -> term -> term
    val simplifySubst : facts -> (term * term) list -> term -> term option
    val simplifysubstmap :
      facts -> term -> (term * term) list -> (term * term) list option
    val substmapdom : (term * term) list -> term list
    val substmapran : (term * term) list -> term list
    val restrictsubstmap :
      facts -> (term * term) list -> term list -> term list ->
        (term * term) list option
    val plussubstmap :
      facts -> (term * term) list -> (term * term) list ->
        (term * term) list option
    (* some useful survivors *)
    val vtmetacount : (term * term) list -> int
    val vtminus :
      facts -> (term * term) list -> (term * term) list -> (term * term) list
    val vtsplit :
      facts -> (term * term) list -> term list ->
        (term * term) list * (term * term) list * (term * term) list
  end
(* $Id$ *)

module
  Substmapfuns
  (AAA :
    sig
      module listfuns : Listfuns
      module mappingfuns : Mappingfuns
      module optionfuns : Optionfuns
      module answer : Answer
      module term : sig include Termtype include Termstore include Term end
      module proviso : Proviso
      module facts : Facts
      val consolereport : string list -> unit
      val interpolate : 'a -> 'a list -> 'a list
      val mkNotin : term.term * term.term -> proviso.proviso
      
    end)
  :
  Substmapfuns =
  struct
    open AAA
    open listfuns
    open mappingfuns
    open optionfuns
    open answer
    open term
    open proviso
    open facts
    
    
    
    
    
    
    
    
    
    
    let substdebug = ref false
    let rec substmapdom (vts : (term * term) list) = _MAP ((fun(hash1,_)->hash1), vts)
    let rec substmapran (vts : (term * term) list) = _MAP ((fun(_,hash2)->hash2), vts)
    (* varmappedbyq doesn't always take a VAR as its first parameter *)
    (* is that true any more? RB May 95 *)
    let rec varmappedbyq facts v vts = varboundbyq facts v (substmapdom vts)
    (* the vars in varboundbyq are all VARs, aren't they? But if the proof doesn't
       say so, we don't know so.
     *)
    (* Now we do, because of VariableClass and its friends *)
    and varboundbyq facts v vars = existsq (substeqvarsq facts v) vars
    (* varmappedto is None if we can't say, Some v if it doesn't map, Some term if it does *)
    let rec varmappedto facts v vts =
      let rec check a1 a2 =
        match a1, a2 with
          def, (v', t') :: vts ->
            begin match substeqvarsq facts v v' with
              Yes -> Some t'
            | Maybe -> check None vts
            | No -> check def vts
            end
        | def, [] -> def
      in
      check (Some v) vts
    let rec vtmetacount vts =
      nj_fold (fun (x, y) -> x + y)
        (_MAP ((fun (v, _) -> if isUnknown v then 1 else 0), vts)) 0
    let rec vtmaps facts vts (v, _) =
      List.exists (fun (v', _) -> qDEF (substeqvarsq facts v v')) vts
    let rec vtminus facts vts1 vts2 =
      ( <| ) ((fun ooo -> not (vtmaps facts vts2 ooo)), vts1)
    let rec vtsplit facts vts bs =
      let rec S ((v, t), (ys, ns, ms)) =
        let rs = _MAP (substeqvarsq facts v, bs) in
        if List.exists qDEF rs then (v, t) :: ys, ns, ms
        else if List.exists qUNSURE rs then ys, ns, (v, t) :: ms
        else ys, (v, t) :: ns, ms
      in
      nj_fold S vts ([], [], [])
    exception Whoops_
    (* moved outside for OCaml *)
           
       (* Moving a map through another map. If this works you get a new map *)
    let rec plussubstmap facts vtout vtin =
      let vsin = substmapdom vtin in
      match vtsplit facts vtout vsin with
        ys, ns, [] ->
          Some
            (_MAP
               ((fun (v, t) -> v, simplifySubstAnyway facts vtout t), vtin) @
               ns)
      | _ -> None
    (* Moving a map through a binder.  If this works you get a new map *)
    and restrictsubstmap facts vts bs ss =
      let rec res f r =
        if !substdebug then
          consolereport
            ["restrictsubstmap "; vtsstring vts; " "; termliststring bs; " ";
             termliststring ss; " => "; f ()];
        r
      in
      let (ys, ns, ms) = vtsplit facts vts bs in
      let rec newfacts v =
        expandfacts facts (_MAP ((fun b -> mkNotin (v, b)), bs))
      in
      let rec foundinside v =
        List.exists
          (fun ooo ->
             (fun ooo -> not (qDEFNOT ooo)) (varoccursinq (newfacts v) v ooo))
          ss
      in
      if not (List.exists foundinside (substmapdom ms)) then
        if not
             (List.exists
                (fun b ->
                   List.exists
                     (fun ooo ->
                        (fun ooo -> not (qDEFNOT ooo))
                          (varoccursinq facts b ooo))
                     (substmapran ns))
                bs)
        then
          res (fun _ -> vtsstring ns) (Some ns)
        else res (fun _ -> "failure cos of unsafeity (sic)") None
      else res (fun _ -> "failure cos of uncertainty") None
    and varoccursinq facts v =
      fun P ->
        let EF = exterioreqvarsq facts in
        let rec EFv v' =
          if canoccurfreein (idclass v, idclass v') then EF (v, v') else No
        in
        let OF = varoccursinq facts in
        let OFv = OF v in
        let BFv = varboundbyq facts v in
        let MFv = varmappedbyq facts v in
        let rec res r =
          fun P ->
            if !substdebug then
              begin
                consolereport
                  ["varoccursinq "; factsstring facts; " "; termstring v; " ";
                   argstring P; " => "; answerstring r];
                r
              end
            else r
        in
        let rec insidebinding bs ss =
          match BFv bs with
            Yes -> No
          | No -> existsq (varoccursinq facts v) ss
          | Maybe ->
              let newfacts =
                expandfacts facts (_MAP ((fun b -> mkNotin (v, b)), bs))
              in
              existsq (varoccursinq newfacts v) ss
        in
        (* search function to be folded across terms *)
        let rec search =
          fun P ->
            if knownNOTIN facts (v, P) then Some (res No P)
            else
              match P with
                Id _ -> Some (res (EFv P) P)
              | Unknown _ -> Some (res (EFv P) P)
              | Literal _ -> Some (res No P)
              | Subst (_, _, P, vts) ->
                  Some
                    (res
                       (orelseq
                          (existsq
                             (fun (v', t') ->
                                andalsoq (OFv t') (fun _ -> OF (v', P)))
                             vts)
                          (fun _ -> insidebinding (substmapdom vts) [P]))
                       P)
              | Binding (_, (bs, ss, us), _, _) ->
                  Some
                    (res
                       (orelseq (existsq OFv us)
                          (fun _ -> insidebinding bs ss))
                       P)
              | _ -> None
        in
        let rec foldsearch =
          fun (P, sofar) ->
            match sofar with
              Yes -> Some Yes
            | No -> search P
            | Maybe ->
                match search P with
                  None -> None
                | Some v -> Some (orq (sofar, v))
        in
        foldterm foldsearch No P
    and simplifySubstAnyway facts vts =
      fun P ->
        match simplifySubst facts vts P with
          Some t -> t
        | None -> registerSubst (true, P, vts)
    (*
       simplifySubst and simplifysubstmap are partial functions.
       simplifySubst won't deliver a reducible substitution.
    *)
    and simpres vts =
      fun P r ->
        consolereport
          ["simplifySubst "; " "; termstring (Subst (None, true, P, vts));
           " => "; optionstring termstring r]
    and simplifySubst a1 a2 a3 =
      match a1, a2, a3 with
        facts, [], (Subst (_, _, P', vts') as P) ->
          let r = Some (simplifySubstAnyway facts vts' P') in
          if !substdebug then simpres [] P r; r
      | facts, [], P ->
          let r = Some P in if !substdebug then simpres [] P r; r
      | facts, vts, P ->
          let rec res r = if !substdebug then simpres vts P r; r in
          let S = simplifySubstAnyway facts in
          let rec more vts' = fun P' -> Some (S (vts', P')) in
          let rec Svar v =
            match varmappedto facts v vts with
              Some (Subst (_, _, P', vts')) -> res (more vts' P')
            | Some t -> res (Some t)
            | None -> fail v
          and fail =
            fun P ->
              match simplifysubstmap facts P vts with
                Some vts' -> res (more vts' P)
              | None -> res None
          in
          match P with
            Id _ -> Svar P
          | Unknown _ -> Svar P
          | App (_, f, a) -> res (Some (registerApp (S (vts, f), S (vts, a))))
          | Tup (_, sep, ts) ->
              res (Some (registerTup (sep, _MAP (S vts, ts))))
          | Literal k -> res (Some P)
          | Fixapp (_, ss, ts) ->
              res (Some (registerFixapp (ss, _MAP (S vts, ts))))
          | Subst (_, _, P', vts') ->
              begin match plussubstmap facts vts vts' with
                Some vts'' -> res (more vts'' P')
              | None -> fail P
              end
          | Binding (_, (bs, ss, us), env, pat) ->
              begin match restrictsubstmap facts vts bs ss with
                Some vts' ->
                  res
                    (Some
                       (registerBinding
                          ((bs, _MAP (S vts', ss), _MAP (S vts, us)), env,
                           pat)))
              | None -> fail P
              end
          | Collection (_, k, es) ->
              let rec se =
                function
                  Element (_, _, t) -> registerElement (Nonum, S (vts, t))
                | _ -> raise Whoops_
              in
              res
                (try Some (registerCollection (k, _MAP (se, es))) with
                   Whoops_ -> None)
    and simplifysubstmap facts =
      fun P vts ->
        (* W detects substitutions in substitutions and takes them out if poss *)
        (* V detects v slosh v and y slosh E where y NOTIN P *)
        (* U takes out those elements in a very silly way *)
        let rec W =
          function
            v, Subst (_, r, P, m) ->
              begin match simplifySubst facts m P with
                Some t -> Some (v, t)
              | None -> None
              end
          | _ -> None
        in
        let rec V (v, t) =
          eqterms (v, t) || qDEFNOT (varoccursinq facts v P)
        in
        let rec U =
          function
            [] -> None
          | vt :: vts ->
              if V vt then Some (anyway U vts)
              else
                match W vt with
                  Some vt -> Some (anyway U (vt :: vts))
                | None ->
                    match U vts with
                      Some vts -> Some (vt :: vts)
                    | None -> None
        in
        let r = U vts in
        if !substdebug then
          consolereport
            ["simplifysubstmap "; " ";
             termstring (Subst (None, true, P, vts)); " => ";
             optionstring vtsstring r];
        r
  end
