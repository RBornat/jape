(* $Id$ *)

module type Rew =
  sig
    type cxt
    type term
    and seq
    and proviso
    and vid
    and rawinf
    and rewinf
    and resnum
    and element
    (* parameters for rew_... are dosubst, cxt, whatsit *)
    val rew_Term : bool -> cxt -> term -> term option
    val rew_Seq : bool -> cxt -> seq -> seq option
    (* rew_cxt always does substs, you can't choose *)
    val rew_cxt : cxt -> cxt option
    val rew_resnum : cxt -> resnum -> resnum option
    val rew_elements : bool -> cxt -> element list -> element list option
    (* and now some useful ones - really special optionfuns *)
    val rew_ : ('a -> 'a option) -> 'a -> ('a -> 'b) -> 'b option
    val rew_2 :
      ('a -> 'a option) -> 'a -> ('b -> 'b option) -> 'b -> ('a * 'b -> 'c) ->
        'c option
    val rew_Pair : ('a -> 'a option) -> 'a * 'a -> ('a * 'a) option
    val mkrawinf : term list * vid list * int list * int option -> rawinf
    val nullrawinf : rawinf
    (* functions for folding *)
    val rawinfTerm : cxt -> term * rawinf -> rawinf
    val rawinfSeq : cxt -> seq * rawinf -> rawinf
    val rawinfElements : cxt -> element list * rawinf -> rawinf
    val raw2rew_ : rawinf -> rewinf
    val rewinfCxt : cxt -> rewinf
    val rew_worthwhile : bool -> cxt -> rewinf -> bool
    val rewinfstring : rewinf -> string
  end

module type Rewrite =
  sig
    type cxt and term and seq
    val rewrite : cxt -> term -> term
    val rewriteseq : cxt -> seq -> seq
    val rewritesubstmap : cxt -> (term * term) list -> (term * term) list
    val rewritecxt : cxt -> cxt
    (* for updating provisos and BaseSeq *)

    val rewritedebug : bool ref
  end
(* $Id$ *)

module M :
  sig include Rew include Rewrite end =
  struct
    open Listfuns.M
    open Mappingfuns.M
    open Optionfuns.M
    open Answer.M
    open Term.Type
    open Sequent.Type
    open Proviso.M
    open Context.Cxt
    open Rewinf.M
    open Substmapfuns.M
    
    type cxt = Context.Cxt.cxt and term = Term.Funs.term and seq = Sequent.Type.seq
    
    let rewritedebug = ref false
    (* the rewrite functions rewrite a term/sequent/proviso, 
       returning Some .. if it rewrites, None if it doesn't.  
       This allows 'conservative' rewrite, which minimises the size of proof states.
       
       To speed things up, we have removed the property of rewrites that they 
       return information on variables, etc., present within a rewritten formula.
     *)
    
    (* rewinf is a quadruple of vars, uVIDs, badres, psig; 
     * vars is all names; uVIDs all VIDs from unknowns; 
     * badres all ResUnknown/Nonum resource numbers from enclosed collections;
     * psig is Some int -- the context contains an int which is incremented 
     * when the provisos change significantly (i.e., when they are rewritten or 
     * augmented - and perhaps when they are simplified, but we shall come to that).
     * The vars, uVIDs and badres lists are sorted and don't contain duplicates.
     *)
        
    (* rawinf is the same as rewinf, but it doesn't have the same invariants -- 
     * essentially the lists are unsorted and may contain duplicates
     *)
    
    type rawinf = term list * vid list * int list * int option
    let nullrawinf = [], [], [], None
    let mkrawinf raw = raw
    let rec raw2rew_ (vars, uVIDs, badres, psig) =
      mkrewinf
        (sortunique earliervar vars, sortunique (fun (x, y) -> x < y) uVIDs,
         sortunique (fun (x, y) -> x < y) badres, psig)
    (* these functions designed to be folded *)
    let rec rawinfTerm n (t, stuff) =
      let rec f (t, (vars, uVIDs, badres, psig as z)) =
        match t with
          Id _ -> Some (t :: vars, uVIDs, badres, psig)
        | Unknown (_, u, _) -> Some (t :: vars, u :: uVIDs, badres, psig)
        | Subst (_, r, P, vts) ->
            let ff = foldterm f in
            let rec fp ((v, t), z) = ff (ff z v) t in
            Some (ff (nj_fold fp vts (vars, uVIDs, badres, Some n)) P)
        | Collection (_, _, es) ->
            let rec g =
              function
                Segvar (_, ms, t), z -> nj_fold (nj_foldterm f) (t :: ms) z
              | Element (_, ResUnknown i, t), z ->
                  foldterm f (vars, uVIDs, i :: badres, psig) t
              | Element (_, _, t), z -> foldterm f z t
            in
            Some (nj_fold g es z)
        | _ -> None
      in
      foldterm f stuff t
    let rec rawinfresnum (r, (vars, uVIDs, badres, psig as raw)) =
      match r with
        ResUnknown i -> vars, uVIDs, i :: badres, psig
      | _ -> raw
    let rec rawinfSeq n =
      fun (Seq (st, Hs, Gs), stuff) ->
        let RIT = rawinfTerm n in RIT (Hs, RIT (Gs, stuff))
    let rec rawinfProviso n (p, stuff) =
      let RIT = rawinfTerm n in
      match p with
        FreshProviso (_, _, _, v) -> RIT (v, stuff)
      | NotinProviso (v, p) -> RIT (v, RIT (p, stuff))
      | NotoneofProviso (vs, pat, C) -> nj_fold RIT vs (RIT (C, stuff))
      | UnifiesProviso (p1, p2) -> RIT (p1, RIT (p2, stuff))
    let rec rawinfElements n (es, stuff) =
      let rec RE (e, stuff) =
        match e with
          Segvar (_, ops, v) ->
            rawinfTerm n (v, nj_fold (rawinfTerm n) ops stuff)
        | Element (_, r, t) -> rawinfresnum (r, rawinfTerm n (t, stuff))
      in
      nj_fold RE es stuff
    let rec rew_worthwhile subst cxt ri =
      (subst &&
       (match rewinf_psig ri, getprovisosig cxt with
          Some si, ci -> si <> ci
        | _ -> false) ||
       List.exists (opt2bool <*> (fun uVID -> at (varmap cxt, uVID)))
         (rewinf_uVIDs ri)) ||
      List.exists (opt2bool <*> (fun i -> at (resmap cxt, i)))
        (rewinf_badres ri)
    (* this is almost f &~ (Some o yes), but types get in the way ... *)
    let rec rew_ f x yes =
      match f x with
        Some x' -> Some (yes x')
      | _ -> None
    (* this is Some o anyway f *)
    let rec rew_yes f t =
      match f t with
        None -> Some t
      | some -> some
    let rec rew_2 f x g y yes =
      (option_rewrite2 f g &~ (fSome <*> yes)) (x, y)
    let rec rew_Pair = fun R -> option_rewrite2 R R
    (* there must be a better way ... *)
    let rec rew_3 f x g y h z yes =
      (option_rewrite3 f g h &~ (fSome <*> yes)) (x, y, z)
    let rec rew_binding outer inner (h, (bs, ss, us), env, pat) =
      rew_3 (option_rewritelist outer) bs (option_rewritelist inner) ss
        (option_rewritelist outer) us
        (fun stuff -> registerBinding (stuff, env, pat))
    let rec rew_Term subst cxt t =
      let rec S t =
        match t with
          Id (_, v, _) -> None
        | Unknown (_, v, _) ->
            begin match at (varmap cxt, v) with
              Some t' -> rew_yes S t'
            | None -> None
            end
        | App (h, f, a) -> rew_ (rew_Pair S) (f, a) registerApp
        | Tup (h, sep, ts) ->
            rew_ (option_rewritelist S) ts (fun ts' -> registerTup (sep, ts'))
        | Literal _ -> None
        | Fixapp (h, ss, ts) ->
            rew_ (option_rewritelist S) ts
              (fun ts' -> registerFixapp (ss, ts'))
        | Binding binding -> rew_binding S S binding
        | Subst Pm -> rew_Subst subst cxt Pm
        | Collection (h, k, es) ->
            rew_ (rew_elements subst cxt) es
              (fun es' -> registerCollection (k, es'))
      in
      let res = S t in
      if !rewritedebug then
        consolereport
          ["rew_Term "; string_of_int subst; " cxt "; argstring t; " => ";
           optionstring termstring res];
      res
    and rew_substmap subst cxt vts =
      option_rewritelist (rew_Pair (rew_Term subst cxt)) vts
    and rew_Subst subst cxt =
      fun (h, r, P, m) ->
        let simp = simplifySubst (facts (provisos cxt) cxt) in
        match
          rew_2 (rew_Term subst cxt) P (rew_substmap subst cxt) m
            (fun (P, m) ->
               if subst then
                 match simp m P with
                   None -> registerSubst (r, P, m)
                 | Some t -> t
               else registerSubst (r, P, m))
        with
          None -> if subst then simp m P else None
        | some -> some
    and rew_elements subst cxt es =
      let RT = rew_Term subst cxt in
      let rec def (r, t) =
        rew_ (rew_Term subst cxt) t (fun t -> [registerElement (r, t)])
      in
      let rec RE =
        function
          Segvar (_, [], (Unknown _ as v)) ->
            rew_ RT v
              (function
                 Collection (_, _, es) -> es
               | _ -> raise (Catastrophe_ ["rew_elements 1"]))
        | Segvar (_, ps, (Unknown _ as v)) as sv ->
            rew_ RT v
              (function
                 Collection (_, _, []) -> []
               | Collection (_, _, es) as c ->
                   (* modeifyelement ps <* es *)
                   raise
                     (Catastrophe_
                        ["rew_elements 2 "; elementstring sv; " ";
                         bracketedliststring elementstring "," es])
               | _ -> raise (Catastrophe_ ["rew_elements 3"]))
        | Element (_, (ResUnknown i as r), t) ->
            (* This code, and the type of resmap, are designed to fix a space leak.
             * Provided that it is invariant that resource numbers map uniquely
             * to formulae in the tree (modulo rewriting), then it is safe to 
             * take a canonical version of any element.  
             * Without this, some replayed proofs generate enormous amounts of
             * space
             *)
            begin match at (resmap cxt, i) with
              Some (r, t) ->
                rew_yes (rew_elements subst cxt) [registerElement (r, t)]
            | None -> def (r, t)
            end
        | Element (_, r, t) -> def (r, t)
        | _ -> None
      in
      match es with
        [] -> None
      | e :: es ->
          match RE e, rew_elements subst cxt es with
            Some es1, Some es2 -> Some (es1 @ es2)
          | None, Some es -> Some (e :: es)
          | Some es1, None -> Some (es1 @ es)
          | None, None -> None
    let rec rew_resnum cxt r =
      match r with
        ResUnknown i ->
          begin match at (resmap cxt, i) with
            Some (r, _) -> rew_yes (rew_resnum cxt) r
          | None -> None
          end
      | _ -> None
    let rec rew_Seq subst cxt =
      fun (Seq (st, H, G)) ->
        rew_2 (rew_Term subst cxt) H (rew_Term subst cxt) G
          (fun (H', G') -> Seq (st, H', G'))
    let rec rew_Proviso subst cxt p =
      let RT = rew_Term subst cxt in
      match p with
        FreshProviso (h, g, r, v) ->
          rew_ RT v (fun v -> FreshProviso (h, g, r, v))
      | NotinProviso vp -> rew_ (rew_Pair RT) vp NotinProviso
      | NotoneofProviso (vs, pat, C) ->
          rew_3 (option_rewritelist RT) vs RT pat RT C NotoneofProviso
      | UnifiesProviso pp -> rew_ (rew_Pair RT) pp UnifiesProviso
    let rec rew_update a1 a2 a3 a4 =
      match a1, a2, a3, a4 with
        f, g, Some ri, cxt ->
          begin match rewinf_psig ri with
            Some n ->
              let n' = getprovisosig cxt in
              if n = n' then None else Some (f (rewinf_setpsig ri (Some n')))
          | None -> None
          end
      | f, g, None, cxt -> g ()
    let rec rew_cxt cxt =
      (* there is no point rewriting the substitutions inside the provisos
       * until we have replaced the unknowns.  
       * That fact is the whole reason for the 'subst' argument in the rew_
       * functions.
       *)
      let rec needsit a1 a2 a3 =
        match a1, a2, a3 with
          subst, cxt, Some inf -> rew_worthwhile subst cxt inf
        | subst, cxt, None -> true
      in
      let rec doprovisos subst cxt =
        let (ps, pinf) = getprovisos cxt in
        if needsit subst cxt pinf then
          let rec newrewinf n ps =
            Some
              (raw2rew_
                 (nj_fold (rawinfProviso n) ((provisoactual <* ps))
                    nullrawinf))
          in
          let rec yes nextcxt ps =
            Some (setprovisos nextcxt (ps, newrewinf (getprovisosig cxt) ps))
          in
          let rec updatesig () =
            let rec f pinf' = setprovisos cxt (ps, Some pinf') in
            let rec g () = yes cxt ps in(* doesn't change ps, so doesn't cause incprovisosig *)
             rew_update f g pinf cxt
          in
          let rec rew_visproviso subst cxt vp =
            rew_ (rew_Proviso subst cxt <*> provisoactual) vp
              (provisoresetactual vp)
          in
          match option_rewritelist (rew_visproviso subst cxt) ps with
            Some ps -> yes (incprovisosig cxt) ps
          | None -> updatesig ()
        else None
      in
      (* doprovisos *)
               
      let rec doexterior subst cxt =
        match getexterior cxt with
          Exterior ((ss, s), inf, fvinf) ->
            if needsit subst cxt inf then
              let rec yes (ss, s) =
                let raw =
                  nj_fold (rawinfSeq (getprovisosig cxt)) (s :: ss) nullrawinf
                in
                let inf = raw2rew_ raw in
                let fvinf =
                  match rewinf_vars inf, rewinf_uVIDs inf with
                    avs, [] ->
                      let (Seq (_, hs, cs)) = s in
                      let hbindings = varbindings hs in
                      let cbindings = varbindings cs in
                      let rec sv1 (s, stuff) =
                        bmerge (seqvars varbindings bmerge s, stuff)
                      in
                      let stuff =
                        nj_fold sv1 ss (bmerge (hbindings, cbindings))
                      in
                      let ps =
                        optionfilter
                          (
                             (function
                                NotinProviso p -> Some p
                              | _ -> None) <*> provisoactual)
                          (provisos cxt)
                      in
                      let (fvs, m) = freevarsfrombindings stuff ps in
                      let (bhfvs, _) = freevarsfrombindings hbindings ps in
                      let (bcfvs, _) = freevarsfrombindings cbindings ps in
                      Some (avs, fvs, mkmap m, bhfvs, bcfvs)
                  | _ -> None
                in
                Some
                  (setexterior (incprovisosig cxt)
                     (Exterior ((ss, s), Some inf, fvinf)))
              in
              let rec updatesig () =
                let rec f inf' =
                  setexterior cxt (Exterior ((ss, s), Some inf', fvinf))
                in
                let rec g _ = yes (ss, s) in rew_update f g inf cxt
              in
              match
                rew_2 (option_rewritelist (rew_Seq subst cxt)) ss
                  (rew_Seq subst cxt) s (fun p -> p)
              with
                Some r -> yes r
              | None -> updatesig ()
            else None
        | _ ->(* end case Exterior ... *)
           None
      in
      let rec doit subst cxt =
        let rec doit2 cxt =
          match doexterior subst cxt with
            Some cxt -> rew_yes (doit true) cxt
          | None -> if subst then None else doit true cxt
        in
        match doprovisos subst cxt with
          Some cxt -> rew_yes (if subst then doit true else doit2) cxt
        | None -> doit2 cxt
      in
      let r = doit false cxt in
      if !rewritedebug then
        consolereport
          ["rew_cxt "; cxtstring cxt; " => "; optionstring cxtstring r];
      r
    (*rew_cxt*)let rawinfTerm cxt = rawinfTerm (getprovisosig cxt)
    let rawinfSeq cxt = rawinfSeq (getprovisosig cxt)
    let rawinfProviso cxt = rawinfProviso (getprovisosig cxt)
    let rawinfElements cxt = rawinfElements (getprovisosig cxt)
    let rec rewinfCxt cxt =
      match getprovisos cxt, getexterior cxt with
        (_, Some pinf), Exterior (_, Some binf, _) ->
          rewinf_merge (pinf, binf)
      | (_, Some pinf), _ -> pinf
      | _, Exterior (_, Some binf, _) -> binf
      | _ -> nullrewinf
    let rec rewrite cxt = anyway (rew_Term true cxt)
    let rec rewriteseq cxt = anyway (rew_Seq true cxt)
    let rec rewritesubstmap cxt = anyway (rew_substmap true cxt)
    let rewritecxt = anyway rew_cxt
  end
