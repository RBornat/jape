(* $Id$ *)

module type T =
  sig
    type term and element and cxt and vid
    val unifyterms : term * term -> cxt -> cxt option
    val unifytermsandcheckprovisos : term * term -> cxt -> cxt option
    val unifyvarious : term * term -> cxt -> cxt list
    val dropunify : element * element list -> cxt -> cxt option
    val simplifydeferred : cxt -> cxt option
    val matchedtarget : cxt -> cxt -> vid list -> bool
    val unifydebug : bool ref
  end
(* $Id$ *)

(* Includes Bag and List unification, with trepidation *)

(* Includes proper treatment of non-reducible Subst terms *)

(* includes alpha conversion : forall x._P unifies with forall y.Q if 
 * w NOTIN _P, w NOTIN Q (we invent w) and _P[x\w] unifies with Q[y\w].  This
 * means being clever with substitutions, q.v. below.
 *
 * For a long time _P[x\E] has unified with _P, generating the proviso x NOTIN _P.
 * Now, because of alpha conversion and non-reducible substitutions, we have to be
 * a lot cleverer even than that.
 *
 * The fundamental trick is what I call 'abstraction'.  _P[xs\Es] unifies with Q 
 * (Q not a substitution, _P[xs\Es] not reducible even if we ignore its 
 * reducible/non-reducible status) if there is a Q' such that Q'[xs\Es] = Q, 
 * and then _P unifies with Q'.  That's higher-order unification, that last step ...
 *
 * Then there is the case of substitutions where one's replacement domain is a 
 * subset of the other's.  _P[xs\Es] will unify with Q[xs,y\Fs,G] if there is a Q' 
 * such that Q'[xs,y\xs,G] = Q and then _P[xs\Es] unifies with Q'[xs\Fs].
 *
 * When the substitution lengths are the same, in order to handle non-reducible 
 * substitutions we first try higher-order unification: that is, _P[xs\Es] unifies 
 * with Q[ys\Fs] if _P unifies with Q, xs with ys, Es with Fs.
 * But that may fail ...
 *
 * In the final extreme we will try abstraction as if one _P[xs\Es], Q[ys\Fs] wasn't
 * a substitution.
 *
 * RB May 95
 *)
module M : T with type term = Term.Type.term
			  and type element = Term.Type.element
			  and type cxt = Context.Cxt.cxt
			  and type vid = Term.Type.vid
=
  struct
    open Answer
    open Context.Cxt
    open Context.Cxtstring
    open Facts.M
    open Idclass.M
    open Listfuns.M
    open Mappingfuns.M
    open Miscellaneous.M
    open Optionfuns.M
    open Proviso.M
    open Provisofuns.M
    open Rewrite.Funs
    open Sml.M
    open Stringfuns.M
    open Substmapfuns.M
    open Symbol
    open Term.Funs
    open Term.Store
    open Term.Type
    open Term.Termstring
    
    type term = Term.Type.term
     and element = Term.Type.element
     and cxt = Context.Cxt.cxt
     and vid = Term.Type.vid

    let unifydebug = ref false
    let rec freshUnknown cxt c v =
      let (cxt, v) = freshVID cxt c v in cxt, registerUnknown (v, c)
    let rec freshalphavar cxt c v =
      try freshproofvar cxt c v with
        Catastrophe_ ss ->
          try freshproofvar cxt c (vid_of_string (autoID c "alpha")) with
            Catastrophe_ _ -> raise (Catastrophe_ ss)
    let rec occurs cxt v t =
      match t with
        Id _ -> false
      | Unknown (_, v', _) ->
          v = v' ||
          (match at (varmap cxt, v') with
             Some t -> occurs cxt v t
           | None -> false)
      | App (_, f, a) -> occurs cxt v f || occurs cxt v a
      | Tup (_, _, ts) -> List.exists (occurs cxt v) ts
      | Literal _ -> false
      | Fixapp (_, _, ts) -> List.exists (occurs cxt v) ts
      | Binding (_, (bs, ss, us), _, _) ->
          (List.exists (occurs cxt v) bs || List.exists (occurs cxt v) ss) ||
          List.exists (occurs cxt v) us
      | Subst (_, _, _P, vts) ->
          occurs cxt v _P ||
          List.exists (fun (v', t') -> occurs cxt v v' || occurs cxt v t') vts
      | Collection (_, _, es) ->
          let rec occurse =
            function
              Segvar (_, _, t) -> occurs cxt v t
            | Element (_, _, t) -> occurs cxt v t
          in
          List.exists occurse es
    exception MatchinAbstract_
    (* spurious exception to shut compiler up *)
       (* for useful abstraction, m and _P must have been rewritten *)
    let rec abstract orig map term ps cxt =
      let newprovisos : proviso list ref = ref [] in
      let newunifications : (term * term) list ref = ref [] in
      let realmapdom = substmapdom ((fun (v, t) -> v <> t) <| map) in
      let rec addunification (t1, t2) =
        if !unifydebug then
          consolereport
            ["addunification ";
             pairstring termstring termstring "<:>" (t1, t2)];
        newunifications := (t1, t2) :: !newunifications
      in
      let rec addproviso p =
        if !unifydebug then consolereport ["addproviso "; provisostring p];
        newprovisos := p :: !newprovisos
      in
      let rec ourprovisos () =
        ps @ ((fun p -> mkvisproviso (true, p)) <* !newprovisos)
      in
      let rec okname cxt =
        fun _P m ->
          match varmappedbyq (facts (ourprovisos ()) cxt) _P m with
            No -> Some (cxt, _P)
          | Maybe ->
              List.iter addproviso
                 ((fun v -> NotinProviso (v, _P)) <* realmapdom);
              Some (cxt, _P)
          | Yes -> None
      in
      let rec showAres m =
        fun _P r ->
          consolereport
            ["_Abstract cxt "; vtsstring m; " "; termstring _P; " => ";
             optionstring (fun (cxt, t) -> "cxt'," ^ termstring t) r]
      in
      let rec _Abstract a1 a2 a3 =
        match a1, a2, a3 with
          cxt, [], _P ->
            let r = Some (cxt, _P) in if !unifydebug then showAres [] _P r; r
        | cxt, m, _P ->
            let rec _Ares r = if !unifydebug then showAres m _P r; r in
            let rec match__ a1 a2 =
              match a1, a2 with
                f, [] -> f ()
              | f, (v, t) :: vts ->
                  match unifyeqtermsq (facts (ourprovisos ()) cxt) _P t with
                    Yes -> _Ares (Some (cxt, reb _P t v))
                  | Maybe -> match__ defermatch vts
                  | No -> match__ f vts
            and reb =
              fun _P t v ->
                match bracketed _P, bracketed v with
                  true, true -> reb (debracket _P) (debracket t) v
                | true, _ -> enbracket v
                | _ -> v
            and defermatch () =
              let prefix = vid_of_string (autoID FormulaClass "abst") in
              let (cxt', vid') = freshVID cxt FormulaClass prefix in
              let var' = registerUnknown (vid', FormulaClass) in
              let subst = registerSubst (true, var', map) in
              begin match _P with
                Unknown _ -> addunification (_P, subst)
              | _ -> addproviso (UnifiesProviso (_P, subst))
              end;
              _Ares (Some (cxt', var'))
            and _Afold m (t, (cxt, ts)) =
              match _Abstract cxt m t with
                Some (cxt', t') -> Some (cxt', t' :: ts)
              | _ -> None
            and analyse =
              fun _P ->
                match _P with
                  Id _ -> _Ares (okname cxt _P m)
                | Unknown (_, vid, c) ->
                    begin match
                      varmappedbyq (facts (ourprovisos ()) cxt) _P m
                    with
                      No -> _Ares (Some (cxt, _P))
                    | Maybe ->
                        let (cxt', vid') = freshVID cxt c vid in
                        let var' = registerUnknown (vid', c) in
                        let subst = registerSubst (true, var', map) in
                        addunification (_P, subst); _Ares (Some (cxt', subst))
                    | Yes -> _Ares None
                    end
                | App (_, f, a) ->
                      (optionfold (_Afold m) [f; a] (cxt, []) &~~
                       (function
                          cxt, [f; a] -> _Ares (Some (cxt, registerApp (f, a)))
                        | _ -> raise MatchinAbstract_))
                | Tup (_, sep, ts) ->
                         (optionfold (_Afold m) ts (cxt, []) &~~
                          (fun (cxt, ts') ->
                             _Ares (Some (cxt, registerTup (sep, ts'))))) |~~
                       (fun _ -> _Ares None)
                | Literal _ -> _Ares (Some (cxt, _P))
                | Fixapp (_, bras, ts) ->
                         (optionfold (_Afold m) ts (cxt, []) &~~
                          (fun (cxt, ts) ->
                             Some (cxt, registerFixapp (bras, ts)))) |~~
                       (fun _ -> _Ares None)
                | Binding (_, (bs, ss, us), env, pat) ->
                    begin match
                      restrictsubstmap (facts (ourprovisos ()) cxt) m bs ss
                    with
                      Some m' ->
                          (
                             (optionfold (_Afold m) us (cxt, []) &~~
                              (fun (cxt, us') ->
                                   (
                                      (optionfold (_Afold m') ss (cxt, []) &~~
                                       (fun (cxt, ss') ->
                                          Some
                                            (cxt,
                                             registerBinding
                                               ((bs, ss', us'), env, pat)))) |~~
                                    (fun _ -> _Ares None)))) |~~
                           (fun _ -> _Ares None))
                    | None -> defermatch ()
                    end
                | Subst (_, r, _P', m') ->
                    let vs' = substmapdom m' in
                    let ts' = substmapran m' in
                    let fs = facts (ourprovisos ()) cxt in
                    begin match vtsplit fs m vs' with
                      ys, ns, [] ->
						(optionfold (_Afold m) ts' (cxt, []) 
						 &~~
						 (fun (cxt, ts'') ->
							  (_Abstract cxt
									 ((vs'@substmapdom ns)|||(vs'@substmapran ns)) _P' 
							   &~~
							   (fun (cxt, _P'') ->
								  Some (cxt, registerSubst (r, _P'', (substmapdom m'|||ts''))))
							  ) 
							  |~~ (fun _ -> _Ares None)
						 ) 
						) |~~ (fun _ -> _Ares None)
                    | _ -> defermatch ()
                    end
                | Collection _ -> _Ares None
            in
            match__ (fun () -> analyse _P) m
      in
      let rec doit () =
        match _Abstract cxt map term with
          Some (cxt, term) -> Some (cxt, term, !newprovisos, !newunifications)
        | _ -> None
      in
      match orig with
        Unknown (_, v, _) ->
          if occurs cxt v term then
            begin
              if !unifydebug then
                consolereport
                  ["making ";
                   provisostring
                     (UnifiesProviso
                        (registerSubst (true, orig, map), term))];
              Some
                (cxt, orig,
                 [UnifiesProviso (registerSubst (true, orig, map), term)], [])
            end
          else doit ()
      | _ -> doit ()
    let rec whatever f cxt t =
      match f cxt t with
        None -> Some (cxt, t)
      | r -> r
    (* class used to call subststep, but now it doesn't.  The reason is that subststep
     * can force an alpha-conversion; this looks awful if a formula is only being unified
     * with an unknown, as for example on the rhs of a left rule.  So now we defer subststep
     * until the unifier sees that it could be helpful
     *)
    let rec class__ cxt t =
      match t with
        Unknown (_, v, _) ->
          (at (varmap cxt, v) &~~ whatever class__ cxt)
      | _ -> if bracketed t then whatever class__ cxt (debracket t) else None
    (* was debracket (simplifySubstAnyway (facts (rewrittenprovisos cxt) cxt) m _P) *)
    (* now a single-step simplifier, in an attempt to speed-up failing unifications.
     * RB 16/8/93
     * and it is even more important that we keep this single-step function now,
     * since interpretation of text selection now depends on it - see selection.sml
     * RB April 95
     *)
    and subststep cxt =
      fun (r, _P, m as s) ->
        let rec _S = fun _P' -> registerSubst (r, _P', m) in
        let rec _Svar () =
          (* exponential behaviour alert! *)
          let t = registerSubst s in
          let cxt = rewritecxt cxt in
          let t' = rewrite cxt t in
          if t = t' then None else whatever class__ cxt t'
        in
        let r =
          match debracket _P with
            Id _ -> _Svar ()
          | Unknown _ ->
              begin match class__ cxt _P with
                None -> _Svar ()
              | Some (cxt, _P') -> whatever class__ cxt (_S _P')
              end
          | App (_, f, a) -> Some (cxt, registerApp (_S f, _S a))
          | Tup (_, sep, ts) -> Some (cxt, registerTup (sep, (_S <* ts)))
          | Literal k -> Some (cxt, debracket _P)
          | Fixapp (_, bras, ts) ->
              Some (cxt, registerFixapp (bras, (_S <* ts)))
          | Binding (_, (bs, ss, us), env, pat) ->
              let cxt = rewritecxt cxt in
              let m = rewritesubstmap cxt m in
              let bs = (rewrite cxt <* bs) in
              let fs = facts (provisos cxt) cxt in
              let (ys, ns, ms) = vtsplit fs m bs in
              let m' = ns @ ms in
              let rec newb (b, (bs, ns, cxt)) =
                if List.exists
                     (fun (v, t) ->
                        not (qDEFNOT (substeqvarsq fs v b)) ||
                        not (qDEFNOT (varoccursinq fs b t)))
                     m'
                then
                  let (cxt, b') =
                    freshalphavar cxt VariableClass (vid_of_var b)
                  in
                  b' :: bs, (b, b') :: ns,
                  plusvisibleprovisos cxt
                       ((fun s -> NotinProviso (b', s)) <*
                        (substmapdom m' @ substmapran m') @ ss)
                else b :: bs, ns, cxt
              in
              let (bs', ns, cxt) = nj_fold newb bs ([], [], cxt) in
              Some
                (plusvisibleprovisos cxt ((fun v->NotinProviso v) <* allpairs bs'),
                 registerBinding
                   ((bs', (fun s -> registerSubst (r, registerSubst (true, s, ns), m')) <* ss,
						  (fun u -> registerSubst (r, u, m)) <* us),
                    env, pat))
          | Subst (_, r', _P', m') ->
              let cxt = rewritecxt cxt in
              begin match plussubstmap (facts (provisos cxt) cxt) m m' with
                Some m'' -> whatever class__ cxt (registerSubst (r, _P', m''))
              | None ->
                  match subststep cxt (r', _P', m') with
                    Some (cxt, _P'') ->
                      whatever class__ cxt (registerSubst (r, _P'', m))
                  | None -> None
              end
          | Collection _ -> None
        in
        if !unifydebug then
          consolereport
            ["subststep rewriting "; termstring (registerSubst s); " => ";
             optionstring (pairstring cxtstring termstring ",") r];
        r
    let rec assign (v, c) t cxt =
      if not (specialisesto (c, idclass t)) || occurs cxt v t then None
      else Some (plusvarmap cxt (( |-> ) (v, t)))
    let rec reb origv origt t =
      match bracketed origt, bracketed origv with
        true, true -> reb (debracket origv) (debracket origt) t
      | false, true -> enbracket t
      | _ -> t
    let rec simp cxt t = _The (whatever class__ cxt t)
    type defers = DeferAssignment | DeferAlignment | DeferSimplification
    let rec pp tts =
      bracketedliststring (pairstring termstring termstring "<:>") "," tts
    let rec ppc dds =
      bracketedliststring
        (pairstring
           (function
              DeferAlignment -> "DeferAlignment"
            | DeferAssignment -> "DeferAssignment"
            | DeferSimplification -> "DeferSimplification")
           (pairstring termstring termstring ",") ",")
        ", " dds
    let rec unify a1 a2 a3 a4 =
      match a1, a2, a3, a4 with
        [], [], _, cxt -> Some cxt
      | [], dds, false, cxt -> unifycleverly dds [] cxt
      | [], dds, true, cxt -> unify ((snd <* dds)) [] false cxt
      | (t1orig, t2orig) :: tts, dds, progress, cxt ->
          let (cxt, t1) = simp cxt t1orig in
          let (cxt, t2) = simp cxt t2orig in
          let rec success cxt = unify tts dds true cxt in
          let rec pushtts (t1s, t2s) =
            try unify ((t1s ||| t2s) @ tts) dds progress cxt with
              Zip_ -> None
          in
          let rec doit t1 t2 cxt =
            let rec defer d = unify tts ((d, (t1, t2)) :: dds) progress in
            let delay = unify (tts @ [t1, t2]) dds progress in
            if !unifydebug then
              consolereport
                ["unify "; pp ((t1, t2) :: tts); " "; ppc dds; " ";
                 string_of_bool progress; " "; cxtstring cxt];
            match t1, t2 with
              Unknown (_, v1, c1), Unknown (_, v2, c2) ->
                if v1 = v2 && c1 = c2 then success cxt
                else
                    (
                       (assign (v1, c1) (reb t1orig t2orig t2) cxt |~~
                        (fun _ ->
                           assign (v2, c2) (reb t2orig t1orig t1) cxt)) &~~
                     success)
            | Unknown (_, v1, c1), _ ->
                begin match assign (v1, c1) (reb t1orig t2orig t2) cxt with
                  Some cxt -> success cxt
                | None ->
                    match t2 with
                      Subst _ -> defer DeferAssignment cxt
                    | _ -> None
                end
            | _, Unknown v2 -> doit t2 t1 cxt
            | Id (_, v1v, v1c), Id (_, v2v, v2c) ->
                if v1v = v2v && v1c = v2c then success cxt else None
            | App (_, f1, a1), App (_, f2, a2) -> pushtts ([f1; a1], [f2; a2])
            | Tup (_, s1, t1s), Tup (_, s2, t2s) ->
                if s1 = s2 then pushtts (t1s, t2s) else None
            | Literal (_, k1), Literal (_, k2) ->
                if k1 = k2 then success cxt else None
            | Fixapp (_, bra1s, t1s), Fixapp (_, bra2s, t2s) ->
                if bra1s = bra2s then pushtts (t1s, t2s) else None
            | Binding (_, b1, _, pat1), Binding (_, b2, _, pat2) ->
                if pat1 = pat2 then
                  if List.exists
                       (function
                          Binding _, Binding _ -> false
                        | _ -> true)
                          ((fun (t1, t2) ->
                              snd (simp cxt t1),
                              snd (simp cxt t2)) <*
                           tts)
                  then
                    delay cxt
                  else
                    let (cxt', tts') = alignbindings (b1, b2) cxt in
                    unify (tts' @ tts) dds true cxt'
                else None
            | Subst (_, r1, _P1, m1), Subst (_, r2, _P2, m2) ->
                if not (r1 && r2) then defer DeferAlignment cxt
                else
                  begin match subststep cxt (r1, _P1, m1) with
                    Some (cxt, t1) ->
                      begin match subststep cxt (r2, _P2, m2) with
                        Some (cxt, t2) -> doit t1 t2 cxt
                      | None -> doit t2 t1 cxt
                      end
                  | None ->
                      match subststep cxt (r2, _P2, m2) with
                        Some (cxt, t2) -> doit t1 t2 cxt
                      | None -> defer DeferAlignment cxt
                  end
            | Subst (_, false, _P1, m1), t2 ->
                begin match subststep cxt (false, _P1, m1) with
                  Some (cxt, t1) -> doit t1 t2 cxt
                | None -> defer DeferSimplification cxt
                end
            | Subst (_, true, _P1, m1), _ ->
                begin match subststep cxt (true, _P1, m1) with
                  Some (cxt, t1) -> doit t1 t2 cxt
                | None ->
                    let cxt = rewritecxt cxt in
                    let _P1 = rewrite cxt _P1 in
                    let m1 = rewritesubstmap cxt m1 in
                    match
                      abstract (debracket _P1) m1 (rewrite cxt t2)
                        (provisos cxt) cxt
                    with
                      Some (cxt, _P2, ps, ms) ->
                        unify (ms @ (_P1, _P2) :: tts) dds true
                          (plusvisibleprovisos cxt ps)
                    | _ -> None
                end
            | _, Subst _ -> doit t2 t1 cxt
            | Collection (_, k1, es1), Collection (_, k2, es2) ->
                if k1 = k2 then
                  match unifycollections k1 (es1, es2) cxt with
                    [] -> None
                  | [cxt] -> success cxt
                  | cs ->
                      match optionfilter checkprovisos cs with
                        [] -> None
                      | [cxt] -> success cxt
                      | cs ->
                          success
                            (plusvisibleprovisos cxt [UnifiesProviso (t1, t2)])
                else None
            | _ -> None
          and alignbindings ((b1s, s1s, u1s), (b2s, s2s, u2s)) cxt =
            let rec newb ((v1, v2), (cxt, alls, m1, m2, es)) =
              let v1 = rewrite cxt v1 in
              let v2 = rewrite cxt v2 in
              if v1 = v2 then cxt, v1 :: alls, m1, m2, es
              else
                match v1, v2 with
                  Id (_, vid1, c1), Id (_, vid2, c2) ->
                    let rec notins cxt v ss =
                      plusvisibleprovisos
                        cxt ((fun s -> NotinProviso (v, s)) <* ss)
                    in
                    let fs = facts (provisos cxt) cxt in
                    if knownproofvar fs v1 then
                      notins cxt v1 s2s, v1 :: alls, m1, (v2, v1) :: m2, es
                    else if knownproofvar fs v2 then
                      notins cxt v2 s1s, v2 :: alls, (v1, v2) :: m1, m2, es
                    else
                      let (cxt', v') = freshalphavar cxt c1 vid1 in
                      notins cxt' v' (s1s @ s2s), v' :: alls, (v1, v') :: m1,
                      (v2, v') :: m2, es
                | _ -> cxt, v1 :: alls, m1, m2, (v1, v2) :: es
            in
            let rec news m s = registerSubst (true, s, m) in
            let (cxt, alls, m1, m2, extras) =
              nj_fold newb (((debracket <* b1s) ||| (debracket <* b2s)))
                (cxt, [], [], [], [])
            in
            let news1s = if null m1 then s1s else (news m1 <* s1s) in
            let news2s = if null m2 then s2s else (news m2 <* s2s) in
            plusvisibleprovisos cxt ((fun v->NotinProviso v) <* allpairs alls),
            extras @ ((news1s @ u1s) ||| (news2s @ u2s))
          in
          doit t1 t2 cxt
    (* This function used to be desperation, but now ....  .
       We could be into multiple possible unifications here.
       I only look for the first one.
     *)
    and unifycleverly tts dds cxt =
      if !unifydebug then
        consolereport
          ["unifycleverly "; ppc tts; " "; ppc dds; " "; cxtstring cxt];
      match tts with
        (reason, (t1, t2) as tt) :: tts ->
          let rec yes tts' cxt' =
            unify (tts' @ (snd <* (tts @ dds))) [] true cxt'
          in
          let rec no cxt = unifycleverly tts (tt :: dds) cxt in
          let rec reversecleverly () =
            unifycleverly ((reason, (t2, t1)) :: tts) dds cxt
          in
          let (cxt, t1') = simp cxt t1 in
          let (cxt, t2') = simp cxt t2 in
          begin match reason, (t1', t2') with
            DeferAlignment,
            (Subst (_, r1, t1, vts1), Subst (_, r2, t2, vts2)) ->
              begin match alignsubsts (r1, t1, vts1) (r2, t2, vts2) cxt with
                Some (cxt', tts') -> yes tts' cxt'
              | None -> no cxt
              end
          | DeferAssignment, (Unknown _, Subst _) ->
              (* it is possible to determine the minimum amount of rewriting 
               * required to make t1 and t2 unify, and this function is 
               * designed to permit, but for the moment I just blitz t2 
               * and hope for the best.
               * RB 9/xii/96
               *)
              let cxt = rewritecxt cxt in
              let t2'' = rewrite cxt t2' in
              if t2'' <> t2' then yes [t1', t2''] cxt else no cxt
          | DeferAssignment, (Subst _, Unknown _) -> reversecleverly ()
          | DeferAssignment, _ -> yes [t1, t2] cxt
          | DeferSimplification, (Subst (_, false, _P1, m1), t2) ->
              (* something happened! *)  
              begin match subststep cxt (false, _P1, m1) with
                Some (cxt', t1') -> yes [t1', t2] cxt'
              | _ -> no cxt
              end
          | DeferSimplification, (_, Subst (_, false, _, _)) ->
              reversecleverly ()
          | _ ->
              raise
                (Catastrophe_
                   ["unifycleverly "; ppc (tt :: tts); " "; ppc dds])
          end
      | [] -> lastditch dds [] cxt
    and lastditch tts dds cxt =
      if !unifydebug then
        consolereport
          ["lastditch "; ppc tts; " "; ppc dds; " "; cxtstring cxt];
      match tts with
        (reason, (t1, t2) as tt) :: tts ->
          let rec no cxt = lastditch tts (tt :: dds) cxt in
          let (cxt, t1') = simp cxt t1 in
          let (cxt, t2') = simp cxt t2 in
          begin match reason, (t1', t2') with
            DeferAlignment, (Subst (_, _, _P1, m1), Subst (_, _, _P2, m2)) ->
              (* we've tried everything - now try it component-wise. *)
              let cxt = rewritecxt cxt in
              begin match
                zipsubstmaps (facts (provisos cxt) cxt)
                  (canonicalsubstmap m1, canonicalsubstmap m2)
              with
                Some mms ->
                  unify ((_P1, _P2) :: mms @ (snd <* (tts @ dds))) []
                    true cxt
              | None ->(* used to allow failure ... *)
                 no cxt
              end
          | _ -> no cxt
          end
      | [] -> None
    and zipsubstmaps pdb (vts1, vts2) = zipvts pdb (vts1, vts2)
    (* we can unify term->term mappings if their domains are the same,
       or if their differences are
       one element, or if the differences are two elements only
       one of which has a domain which is a metavariable.
     *)
    and zipvts pdb (vts1, vts2) =
      let diff1 = vtminus pdb vts1 vts2 in
      let diff2 = vtminus pdb vts2 vts1 in
      let rec zipup (tt1s, tt2s) =
        nj_fold (fun (((v1, t1), (v2, t2)), mms) -> (v1, v2) :: (t1, t2) :: mms)
          ((tt1s ||| tt2s)) []
      in
      let rec ok (d1, d2) =
        Some
          (zipup (d1, d2) @
             zipup (vtminus pdb vts1 diff1, vtminus pdb vts2 diff2))
      in
      match
        List.length vts1 = List.length vts2,
        vtmetacount diff1 = 1 && vtmetacount diff2 = 1, diff1, diff2
      with
        true, _, [], [] -> ok (diff1, diff2)
      | true, _, [_], [_] -> ok (diff1, diff2)
      | true, true, [Unknown _, _; _], [Unknown _, _; _] ->
          ok (diff1, List.rev diff2)
      | true, true, [_; Unknown _, _], [_; Unknown _, _] ->
          ok (diff1, List.rev diff2)
      | true, true, _, _ -> ok (diff1, diff2)
      | _ -> None
    and ziptermlists pdb (t1s, t2s) =
      try Some ((t1s ||| t2s)) with
        Zip_ -> None
    and alignsubsts =
      fun (r1, _P1, m1) ->
        fun (r2, _P2, m2) cxt ->
          (* align the maps in substitutions if you can *)
          let cxt = rewritecxt cxt in
          let m1 = rewritesubstmap cxt m1 in
          let m2 = rewritesubstmap cxt m2 in
          let fs = facts (provisos cxt) cxt in
          let rec try__ =
            fun (r1, _P1, m1) ->
              fun (r2, _P2, m2) ->
                match vtsplit fs m1 (substmapdom m2) with
                  ys, (_ :: _ as ns), [] ->
                    let _P1 = rewrite cxt _P1 in
                    let _P2 = rewrite cxt _P2 in
                    begin match
                      abstract (debracket _P1)
                        (
                           (substmapdom m2 @ substmapdom ns |||
                            substmapdom m2 @ substmapran ns))
                        _P2 (provisos cxt) cxt
                    with
                      Some (cxt, _P2', ps, ms) ->
                        Some
                          (plusvisibleprovisos cxt ps,
                           ms @
                             [registerSubst (r1, _P1, ys),
                              registerSubst (r2, _P2', m2)])
                    | _ -> None
                    end
                | _ -> None
          in
            (try__ (r1, _P1, m2) (r2, _P2, m2) |~~
             (fun _ -> try__ (r2, _P2, m2) (r1, _P1, m1)))
    and unifycollections kind (e1s, e2s) cxt =
      let rec bk f = bracketedliststring f "," in
      let bkels = bk (smlelementstring termstring) in
      let rec ures ((r1, t1), (r2, t2)) cxt =
        let rec rval =
          function
            (ResUnknown i as r), t ->
              begin match at (resmap cxt, i) with
                Some (r, t) -> rval (r, t)
              | None -> r, t
              end
          | p -> p
        in
        let (r1, t1) = rval (r1, t1) in
        let (r2, t2) = rval (r2, t2) in
        match r1, r2 with
          Nonum, _ -> Some cxt
        | _, Nonum ->(* can't stop it *)
           Some cxt
        | ResUnknown i1, ResUnknown i2 ->
            (* can't stop it *)
            if i1 = i2 then Some cxt
            else Some (plusresmap cxt (( |-> ) (i1, (r2, t2))))
        | ResUnknown i1, _ -> Some (plusresmap cxt (( |-> ) (i1, (r2, t2))))
        | _, ResUnknown _ -> ures ((r2, t2), (r1, t1)) cxt
        | Resnum _, Resnum _ -> if r1 = r2 then Some cxt else None
      in
      (* even the blasted list unifier needs to know the true shape of its collection 
       * now ... so we find the shape of the whole list, every time.  Oh dear.
       *)
      let rec collclass cxt es =
        let rec expand (e, (cxt, es)) =
          match e with
            Segvar (_, [], Unknown (_, v, _)) ->
              begin match at (varmap cxt, v) with
                Some (Collection (_, _, es')) -> nj_fold expand es' (cxt, es)
              | _ -> cxt, e :: es
              end
          | Segvar (_, ps, Unknown (_, v, _)) ->
              begin match at (varmap cxt, v) with
                Some (Collection (_, _, es')) ->
                  (* nj_fold expand (modeifyelement ps <* es') (cxt,es) *)
                  raise (Catastrophe_ ["unify collclass expand"])
              | _ -> cxt, e :: es
              end
          | Element (_, r, t) ->
              let (cxt, t') = simp cxt t in cxt, registerElement (r, t') :: es
          | _ -> cxt, e :: es
        in
        let r = nj_fold expand es (cxt, []) in
        if !unifydebug then
          consolereport
            ["collclass "; cxtstring cxt; " "; bkels es; " => ";
             pairstring cxtstring bkels "," r];
        r
      in
      (* this function used to be in term.sml, then I realised it needed to use 
       * unification ...
       * RB 4/ix/96
       *)
      let rec demodeify a1 a2 a3 =
        match a1, a2, a3 with
          [], cxt, el -> Some (cxt, el)
        | ps, cxt, Segvar (_, ps', v) ->
            let rec dm a1 a2 =
              match a1, a2 with
                [], ps' -> Some ([], ps')
              | ps, [] -> Some (ps, [])
              | p :: ps, p' :: ps' -> if p = p' then dm ps ps' else None
            in
            (dm ps ps' &~~
               (function
                  [], ps' -> Some (cxt, registerSegvar (ps', v))
                | ps, _ ->
                    match v with
                      Unknown (_, vid, c) ->
                        let (cxt, u) = freshUnknown cxt c vid in
                        Some
                          (plusvarmap cxt
                              (( |-> )
                                (vid,
                                 registerCollection
                                   (c, [registerSegvar (ps, u)]))),
                           registerSegvar ([], u))
                    | _ -> None))
        | ps, cxt, Element (_, r, t) ->
            let rec debsimp cxt t =
              let (cxt, t) = simp cxt t in cxt, debracket t
            in
            let rec dm a1 a2 =
              match a1, a2 with
                [], (cxt, t) -> Some (cxt, t)
              | p :: ps, (cxt, App (_, p', t)) ->
                  if p = p' then dm ps (debsimp cxt t) else None
              | ps, (cxt, t) ->
                  let prefix = vid_of_string (autoID FormulaClass "demodeify") in
                  let (cxt, u) = freshUnknown cxt FormulaClass prefix in
                  let rec mkApp =
                    function
                      [] -> u
                    | p :: ps -> registerApp (p, mkApp ps)
                  in
                    (unify [t, mkApp ps] [] false cxt &~~
                     (fun cxt -> Some (simp cxt u)))
            in
            (dm ps (debsimp cxt t) &~~
               (fun (cxt, t) ->
                  let (cxt', n) = freshresnum cxt in
                  let r' =
                    match r with
                      Nonum -> r
                    | Resnum _ -> Resnum n
                    | ResUnknown _ -> ResUnknown n
                  in
                  Some (cxt', registerElement (r', t))))
      in
      let rec demodeifyall ps (el, (cxt, els)) =
        (demodeify ps cxt el &~~ (fun (cxt, el') -> Some (cxt, el' :: els)))
      in
      let rec isuseg =
        function
          Segvar (_, _, Unknown _) -> true (* oh dear this was an sml bug -- what next *)
        | _ -> false
      in
      let rec isel =
        function
          Element _ -> true
        | _ -> false
      in
      (* Pragmatism, pragmatism, wherefore must we be so damned pragmatic?
       * Because Jape is about putting a convenient gloss on a sound mechanism, that's
       * why.  This function is very pragmatic.  At present it defers all and only 
       * 'difficult' unifications where one side consists of two or more unknown 
       * segment variables, and it attempts to do so without introducing unnecessary
       * instances of new unknown segment variables.  If it's done the other way
       * -- giving a choice of all the possible different unifications -- then there
       * would be far too much Offering in applyrule.
       * 
       * At present I can only think of two ways in which this mechanism becames 
       * unhelpful.  If we know that x NOTIN _Gamma, we might still defer the unification
       * of x with _Gamma, _Delta -- that's unhelpful, but not actually wrong.  What is 
       * unsound (and therefore more than usually unhelpful) is that we don't detect that
       * C, D unified with _Gamma, _Gamma is impossible.  It hasn't arisen yet, and 
       * we can fix it then or before.
       *
       * The mechanism used here is reasonably obvious.  The latest bit of pragmatism
       * is to try to identify 'difficult' unifications very early and get rid of them
       * without creating new unknown segment variables.  That will tide us over until
       * we make a serious attempt at modal logics, which will tax the level of 
       * pragmatism employed here still further.
       *
       * RB 17/ix/96
       *)
      let rec unifybags (e1s, e2s) cxt =
        let (cxt, e1s) = collclass cxt e1s in
        let (cxt, e2s) = collclass cxt e2s in
        let rec extract p xs =
          match xs with
            [] -> None
          | x :: xs ->
              if p x then Some (x, xs)
              else (extract p xs &~~ (fun (y, ys) -> Some (y, x :: ys)))
        in
        (* It is in some sense optimal to extract equal components from 
         * each side, and unify them (think about it ...).  But in doing so
         * we must not forget that we must still unify resource numbers.
         * I leave this part of the function here with some trepidation.
         * I also did it uncurried to tease Bernard.
         * RB 29/vii/96
         *)
        (* There is a bug in this procedure: what if two different resources
         * unify with the same unknown?  Then the mechanism of principal-
         * formula selection breaks down, because if we select one of the two
         * resources, and unifybags selects the other (apparently identical) 
         * resource, we don't get a match.  This happens, and therefore 
         * stripeqs must be removed from the mechanism at present.
         * Of course, if we imported selection information into this function
         * (as Bernard always suggested, but I can't see how to implement)
         * then this faulty optimisation could be reintroduced.
         * But for now (four years after I introduced the bug :-)
         * it has to go.
         * RB 31/x/2000
        fun stripeqs (arg as (cxt,e1s,e2s)) =
          case (e1s,e2s) of
            ([],_) => arg
          | (_,[]) => arg
          | (el1::e1s,e2s) =>
              let fun def () = 
                    let val (cxt,e1s,e2s) = stripeqs (cxt,e1s,e2s) in 
                            (cxt,el1::e1s,e2s) 
                    end
              in
                  case extract (fn el2 => eqelements eqterms (el1,el2)) e2s of
                    Some (el2,e2s) =>
                       (case (el1,el2) of
                          (Element (_,r1,t1),Element (_,r2,t2)) => 
                             (case ures ((r1,t1),(r2,t2)) cxt of
                                Some cxt => stripeqs (cxt,e1s,e2s)
                              | None     => def()
                             )
                        | _ => stripeqs (cxt,e1s,e2s)
                       )
                  | _ => def()
              end
        val (cxt,e1s,e2s) = stripeqs (cxt,e1s,e2s)
         *)
        
        let (e1s, e2s) =
          match List.exists isuseg e1s, List.exists isuseg e2s with
            false, true -> e2s, e1s
          | _ -> e1s, e2s
        in
        let rec urev () = unifybags (e2s, e1s) cxt in
        let rec res cs =
          if !unifydebug then
            consolereport
              ["unifybags ("; bkels e1s; ","; bkels e2s; ") "; cxtstring cxt;
               " => "; bk cxtstring cs];
          cs
        in
        let rec uel a1 a2 a3 a4 =
          match a1, a2, a3, a4 with
            defers, cxt, (r, t), el :: es ->
              let rec push (cxt, es) = cxt, el :: es in
              let recv = (push <* uel defers cxt (r, t) es) in
              begin match el with
                Element (_, r', t') ->
                  begin match
                      (ures ((r, t), (r', t')) cxt &~~ unify [t, t'] [] false)
                  with
                    Some cxt' ->
                      (cxt', ((fun(el,_,_,_)->el) <* defers) @ es) :: recv
                  | None -> recv
                  end
              | Segvar (_, ps, Unknown (_, v, c)) ->
                  begin match demodeify ps cxt (registerElement (r, t)) with
                    Some (cxt, el') ->
                      uel ((el, el', ps, v) :: defers) cxt (r, t) es
                  | None -> recv
                  end
              | _ -> recv
              end
          | defers, cxt, (r, t), [] ->
              match defers with
                [] -> []
              | [_, el', ps, v] ->
                  let (cxt, u) = freshUnknown cxt kind v in
                  let cxt =
                    plusvarmap cxt
                       (( |-> )
                         (v,
                          registerCollection
                            (kind, [registerSegvar ([], u); el'])))
                  in
                  [cxt, [registerSegvar (ps, u)]]
              | _ ->
                  let segvs = ((fun(el,_,_,_)->el) <* defers) in
                  let (cxt, newsegvs) =
                    nj_fold
                      (fun ((_, _, ps, v), (cxt, nsvs)) ->
                         let (cxt, u) = freshUnknown cxt kind v in
                         cxt, registerSegvar (ps, u) :: nsvs)
                      defers (cxt, [])
                  in
                  let cxt =
                    plusvisibleprovisos
                       cxt
                       [UnifiesProviso
                          (registerCollection
                             (kind, newsegvs @ [registerElement (r, t)]),
                           registerCollection (kind, segvs))]
                  in
                  [cxt, newsegvs]
        in
        let rec ub e1s (cxt, e2s) = unifybags (e1s, e2s) cxt in
        let rec uf cxt es =
          match es with
            [] -> [cxt]
          | Segvar (_, _, Unknown (_, v, c)) :: es ->
              uf (plusvarmap cxt (( |-> ) (v, registerCollection (c, []))))
                 es
          | _ -> []
        in
        let rec def () =
          [plusvisibleprovisos
              cxt
              [UnifiesProviso
                 (registerCollection (kind, e1s),
                  registerCollection (kind, e2s))]]
        in
        (* once we know that we have to do some unification, here is where it happens *)
        let rec dive () =
          (* see if we can extract an element from e1s, which is the side
           * that has a useg, if either side does
           *)
          match extract isel e1s with
            Some (Element (_, r, t), e1s) ->
              res (List.concat (ub e1s <* uel [] cxt (r, t) e2s))
          | _ ->
              (* run out of elements on left-hand side - check what's left *)
              match e1s, e2s with
                _, [] -> res (uf cxt e1s)
              | [], _ -> res (uf cxt e2s)
              | [Segvar (_, ps, Unknown (_, v, c))], _ ->
                  begin match optionfold (demodeifyall ps) e2s (cxt, []) with
                    Some (cxt, e2s) ->
                      res
                        [plusvarmap cxt (( |-> ) (v, registerCollection (c, e2s)))]
                  | _ -> res []
                  end
              | _, [Segvar (_, ps, Unknown (_, v, c))] -> urev ()
              | _ ->
                  match extract isel e2s with
                    Some (Element (_, r, t), e2s) ->
                      res (List.concat (ub e2s <* uel [] cxt (r, t) e1s))
                  | _ ->
                      (* the two sides are disjoint;
                       * neither side is empty;
                       * neither has any elements left; 
                       * neither is a single unknown segment variable. 
                       *)
                      match
                        List.length (isuseg <| e1s),
                        List.length (isuseg <| e2s)
                      with
                        0, 0 -> res []
                      | 1, _ ->
                          begin match extract isuseg e1s with
                            Some (Segvar (_, ps, Unknown (_, v, c)), e1s') ->
                              let (cxt, u') = freshUnknown cxt c v in
                              let (e2us, e2ks) = split isuseg e2s in
                              begin match
                                optionfold (demodeifyall ps) e2ks (cxt, [])
                              with
                                Some (cxt, e2ks') ->
                                  res
                                    (unifybags
                                       (registerSegvar (ps, u') :: e1s', e2us)
                                       (plusvarmap cxt
                                           (( |-> )
                                             (v,
                                              registerCollection
                                                (kind, e2ks')))))
                              | None -> res []
                              end
                          | _ -> res []
                          end
                      | _, 1 ->(* can't happen, but shut up compiler *)
                         urev ()
                      | _, 0 ->
                          res
                            (if List.exists (not <*> isuseg) e1s then
                               []
                             else def ())
                      | 0, _ ->
                          res
                            (if List.exists (not <*> isuseg) e2s then
                               []
                             else def ())
                      | _ -> res (def ())
        in
        (* case (e1s, e2s) *)
                       (* check that diving would be useless *)
        let rec couldunify cxt usegs el =
          _All
             (function
                Segvar (_, ps, Unknown _) -> opt2bool (demodeify ps cxt el)
              | _ -> false)
             usegs
        in
        let rec maybedef (allusegs, others) =
          if _All (couldunify cxt allusegs) others then def () else dive ()
        in
        (* one possibility that is of great interest is that one side is a
         * more-than-one-member list of unknown segvars, and the other
         * is more-than-one-member list.  In such a case we can cut to the
         * UnifiesProviso straight away.
         *)
        match
          extract (not <*> isuseg) e1s, List.length e1s > 1,
          extract (not <*> isuseg) e2s, List.length e2s > 1
        with
          None, true, _, true -> maybedef (e1s, e2s)
        | _, true, None, true -> maybedef (e1s, e2s)
        | _ -> dive ()
      in
      let rec unifylists (e1s, e2s) cxt =
        let rec res rs =
          if !unifydebug then
            consolereport
              ["unifylists ("; bkels e1s; ","; bkels e2s; ") "; cxtstring cxt;
               " => "; bk cxtstring rs];
          rs
        in
        let (cxt, e1s) = collclass cxt e1s in
        let (cxt, e2s) = collclass cxt e2s in
        let rec ul svs es (cxt, hds, tls) =
          let chds = registerCollection (kind, hds) in
          let cxt' =
            match svs with
              [Segvar (_, _, Unknown (_, v, _))] ->
                plusvarmap  cxt (( |-> ) (v, chds))
            | _ ->
                plusvisibleprovisos
                   cxt
                   [UnifiesProviso (registerCollection (kind, svs), chds)]
          in
          unifylists (es, tls) cxt'
        in
        let rec allsplits ps cxt es =
          let rec asp el cxt es =
            let ess = allsplits ps cxt es in
            let rec push (cxt, hds, tls) = cxt, el :: hds, tls in
            (push <* ess)
          in
          match es with
            el :: es ->
              begin match demodeify ps cxt el with
                Some (cxt, (Segvar (_, ps', Unknown (_, v, c)) as el)) ->
                  (let (cxt, u') = freshUnknown cxt c v in
                   let (cxt, u'') = freshUnknown cxt c v in
                   let cxt =
                     plusvarmap cxt
                        (( |-> )
                          (v,
                           registerCollection
                             (kind,
                              [registerSegvar ([], u');
                               registerSegvar ([], u'')])))
                   in
                   cxt, [registerSegvar (ps', u')],
                   registerSegvar (ps', u'') :: es) ::
                    asp el cxt es
              | Some (cxt, el) -> (cxt, [], el :: es) :: asp el cxt es
              | None -> [cxt, [], el :: es]
              end
          | [] -> [cxt, [], []]
        in
        (* it turns out that our main customer (applyrule) doesn't like 
         * to be told about unifications that differ only in the way that
         * segments are assigned to segment variables.  So we use a UnifiesProviso
         * in that case
         *)
        let rec twuseg =
          function
            (Segvar (_, ps, Unknown _) as e) :: es ->
              let rec simsv =
                function
                  Segvar (_, ps', Unknown _) -> ps = ps'
                | _ -> false
              in
              ps, e :: takewhile simsv es, dropwhile simsv es
          | es -> [], [], es
        in
        let (p1s, svh1s, tl1s) = twuseg e1s in
        let (p2s, svh2s, tl2s) = twuseg e2s in
        let rec def ps svs es es' =
          res (List.concat ((ul svs es <* allsplits ps cxt es')))
        in
        match List.length svh1s, List.length svh2s with
          0, 0 ->
            (* the easy case *)
            begin match e1s, e2s with
              [], [] -> res [cxt]
            | Element (_, r1, t1) :: e1s, Element (_, r2, t2) :: e2s ->
                res
                  (match
                       (ures ((r1, t1), (r2, t2)) cxt &~~
                        unify [t1, t2] [] false)
                   with
                     Some cxt -> unifylists (e1s, e2s) cxt
                   | None -> [])
            | el1 :: e1s, el2 :: e2s ->
                res (if el1 = el2 then unifylists (e1s, e2s) cxt else [])
            | _ -> res []
            end
        | l1, l2 ->
            if l1 <> 1 && l2 = 1 || l1 = 0 then def p2s svh2s tl2s e1s
            else(* prefer e2s *)
             def p1s svh1s tl1s e2s
      in
      let rec res cs =
        if !unifydebug then
          consolereport
            ["unifycollections ("; idclassstring kind; ") ("; bkels e1s;
             ",  "; bkels e2s; ") "; cxtstring cxt; " => "; bk cxtstring cs];
        cs
      in
      match kind with
        BagClass _ -> res (unifybags (e1s, e2s) cxt)
      | ListClass _ -> res (unifylists (e1s, e2s) cxt)
      | _ ->
          raise
            (Catastrophe_
               ["unifycollections ("; idclassstring kind; ") (";
                termstring (registerCollection (kind, e1s)); ",";
                termstring (registerCollection (kind, e2s)); ")"])
    and simplifydeferred pros cxt =
      (* cxt, provisos already rewritten *)
      let _ =
        if !unifydebug then consolereport ["** start simplifydeferred"]
      in
      let rec res r =
        if !unifydebug then
          consolereport
            ["simplifydeferred "; cxtstring cxt; " => ";
             bracketedliststring cxtstring ", " r];
        r
      in
      let rec dodefer (t1, t2 as pair) cxt =
        let rec say s =
          consolereport
            ["dodefer "; argstring (rewrite cxt t1); " ";
             argstring (rewrite cxt t2); " "; s]
        in
        let rec res s r =
          if !unifydebug then
            say
              ((" " ^ s) ^
                 (match r with
                    _ :: _ -> (" succeeds (" ^ string_of_int (List.length r)) ^ ")"
                  | [] -> " fails "));
          r
        in
        let rec nores s r =
          if !unifydebug then say ((" " ^ s) ^ " deferred again"); r
        in
        let rec rematch a1 a2 a3 =
          match a1, a2, a3 with
            (t1, t2), (_, t) :: vts, go ->
              begin match unifyeqtermsq (facts pros cxt) t1 t with
                Maybe -> rematch (t1, t2) vts false
              | Yes ->
                  res "trying unification in rematch 1" (unifyv (t1, t2) cxt)
              | No -> rematch (t1, t2) vts go
              end
          | (t1, t2), [], go ->
              if go then
                res "trying unification in rematch 2" (unifyv (t1, t2) cxt)
              else
                nores "avoiding unification in rematch"
                  [plusvisibleprovisos cxt [UnifiesProviso pair]]
        in
        let (cxt, t1') = simp cxt t1 in
        let (cxt, t2') = simp cxt t2 in
        if not (deferrable cxt (t1', t2')) then res "not deferrable" []
        else if t1' <> t1 || t2' <> t2 then
          res "trying unification" (unifyv (t1', t2') cxt)
        else
          match t1, t2 with
            Subst _, Subst _ ->
              res "trying unification" (unifyv (t1', t2') cxt)
          | _, Subst (_, _, _, vts) -> rematch (t1', t2') vts true
          | Subst (_, _, _, vts), _ -> rematch (t2', t1') vts true
          | _ -> res "trying unification" (unifyv (t1', t2') cxt)
      in
      let rec do1 (pair, cxts) = List.concat ((dodefer pair <* cxts)) in
      let rec split (vp, (defers, others)) =
        match provisoactual vp with
          UnifiesProviso pair -> pair :: defers, others
        | _ -> defers, vp :: others
      in
      let (defers, others) = nj_fold split pros ([], []) in
      let r =
        if null defers then [cxt]
        else(* don't disturb sleeping provisos *)
         nj_fold do1 defers [withprovisos cxt others]
      in
      res r
    and checkdeferred (t1, t2) cxt =
      if null (provisos cxt) then [cxt]
      else
        let cxt = rewritecxt cxt in
        let pros = provisos cxt in
        let (t1, t2) = rewrite cxt t1, rewrite cxt t2 in
        (* if the current problem is in the list, don't check anything *)
        if List.exists
             (
                (function
                   UnifiesProviso (t1', t2') ->
                     eqterms (t1, t1') && eqterms (t2, t2') ||
                     eqterms (t2, t1') && eqterms (t1, t2')
                 | _ -> false) <*> provisoactual)
             pros
        then
          [cxt]
        else simplifydeferred pros cxt
    and doUnify (t1, t2) cxt =
      if !unifydebug then
        consolereport
          ["** start doUnify"; pairstring termstring termstring "," (t1, t2);
           " "; cxtstring cxt];
      let r = unify [t1, t2] [] false cxt in
      if !unifydebug then
        consolereport
          (["doUnify("; termstring t1; ","; termstring t2; ") "] @
             (match r with
                Some cxt -> ["succeeds "; cxtstring cxt]
              | _ ->
                  ["fails -- \n("; smltermstring t1; ","; smltermstring t2;
                   ");\n"; cxtstring cxt]));
      r
    (* function to help sequent (and Collection) unification *)
    and unifyv (t1, t2) cxt =
      let (cxt, t1) = simp cxt t1 in
      let (cxt, t2) = simp cxt t2 in
      match t1, t2 with
        Collection (_, c1, e1s), Collection (_, c2, e2s) ->
          if c1 = c2 then unifycollections c1 (e1s, e2s) cxt else []
      | _ ->
          match doUnify (t1, t2) cxt with
            Some cxt -> [cxt]
          | None -> []
    let rec unifyterms (t1, t2) cxt =
      let r = doUnify (t1, t2) cxt in
      let r' =
        (r &~~
           (fun cxt ->
              match checkdeferred (t1, t2) cxt with
                [] -> None
              | [cxt] -> Some cxt
              | _ :: _ -> Some cxt))
      in
      if !unifydebug then
        begin match r, r' with
          _, Some _ -> consolereport ["and checkdeferred succeeds"]
        | Some _, None -> consolereport ["but checkdeferred fails"]
        | _ -> ()
        end;
      r'
    let unifytermsandcheckprovisos pair =
      (doUnify pair &~ checkprovisos)
    let rec unifyvarious pair cxt =
      List.concat ((checkdeferred pair <* unifyv pair cxt))
    let rec dropunify (target, sources) cxt =
      let rec bad mess =
        raise
          (Catastrophe_
             ["bad call of dropunify ("; mess; ") -- arguments (";
              smlelementstring termstring target; ") (";
              bracketedliststring (smlelementstring termstring) "," sources;
              ")"])
      in
      let rec checkout tc sc =
        if List.exists
             (function
                Segvar (_, _, v) -> idclass v <> sc
              | Element (_, _, t) -> idclass t <> tc)
             sources
        then
          bad "some sources don't fit target"
      in
      let rec res tc espair cxt =
        match unifycollections tc espair cxt with
          [cxt] -> Some cxt
        | [] -> None
        | cxts ->
            bad
              ("multiple answers -- " ^
                 bracketedliststring cxtstring "," cxts)
      in
      match target with
        Segvar (_, ops, (Unknown (_, u, tc) as v)) ->
          begin match tc, sources with
            _, [] -> Some cxt
          | ListClass fc, [source] ->
              (* nothing to do *)
              let (cxt, u') = freshUnknown cxt tc u in
              let (cxt, u'') = freshUnknown cxt tc u in
              checkout fc tc;
              res tc
                ([target],
                 [registerSegvar (ops, u'); source;
                  registerSegvar (ops, u'')])
                cxt
          | ListClass _, _ -> bad "target a List and multiple sources"
          | BagClass fc, _ ->
              let (cxt, u') = freshUnknown cxt tc u in
              checkout fc tc;
              res tc ([target], registerSegvar (ops, u') :: sources) cxt
          | _ -> bad "can't happen -- target is neither list nor bag"
          end
      | _ -> bad "target not a Segvar"
    let sd = simplifydeferred
    (* for export *)
    
    let rec simplifydeferred cxt =
      let cxt = rewritecxt cxt in
      match sd (provisos cxt) cxt with
        [] -> None
      | [cxt'] ->(* can't happen, I think *)
         Some cxt'
      | _ -> Some cxt
    let rec matchedtarget origcxt newcxt uvids =
      let rec ok u =
        match at (varmap origcxt, u), at (varmap newcxt, u) with
          Some old, Some new__ ->
            eqterms (rewrite origcxt old, rewrite newcxt new__)
        | None, None -> true
        | _ -> false
      in
      not (List.exists (not <*> ok) uvids)
  end
