(* $Id$ *)

module type T =
  sig
    type term and cxt
    exception Selection_ of string list
    val selection2Subst : bool -> string list -> cxt -> cxt * term
    val subterm2subst :
      (term * term -> cxt -> cxt option) -> cxt -> term -> term ->
        (cxt * term) option
  end
(* $Id$ *)

module M : T =
  struct
    open Term
    
    type cxt = cxt
    
    (* convert a text selection into a non-reducible substitution *)
    exception Selection_ of string list
    let rec selection2Subst object__ sels cxt =
      let original = implode sels in
      let origterm =
        try string2term original with
          ParseError_ s ->
            raise
              (Catastrophe_
                 (["selection2Subst can't parse original: "; original;
                   " -- "] @
                    s))
      in
      let (cxt', v) = freshvar object__ cxt in
      let rec splitup =
        function
          [s] -> [s], []
        | t1 :: s1 :: ts ->
            let (ts', ss') = splitup ts in t1 :: ts', s1 :: ss'
        | _ ->
            raise
              (Catastrophe_
                 ["selection2Subst given even number of strings: ";
                  bracketedliststring enQuote "," sels])
      in
      let (ts, ss) = splitup sels in
      let rec bad s =
        raise
          (Selection_
             (s :: " - you split the formula up thus: " ::
                interpolate "; " ((enQuote <* sels))))
      in
      let rec badsub () =
        bad
          (if List.length ss = 1 then "the selection you made wasn't a subformula"
           else "the selections you made weren't all subformulae")
      in
      let ss' =
        try (string2term <* ss) with
          ParseError_ _ -> bad "your selection(s) didn't parse"
      in
      let _ =
        if List.exists (fun t -> not (eqterms (t, List.hd ss'))) (List.tl ss') then
          bad "your selections weren't all the same"
      in
      let P =
        try
          string2term (implode (interpolate ((" " ^ termstring v) ^ " ") ts))
        with
          ParseError_ _ -> badsub ()
      in
      let m = [v, List.hd ss'] in
      let res = registerSubst (false, P, m) in
      let pvs = ((fun v' -> v, v') <* termvars origterm) in
      let extraps = (NotinProviso <* (indistinct cxt <| pvs)) in
      let cxt'' = plusvisibleprovisos (cxt', extraps) in
      let rec check t =
        let E = List.exists (fun b -> v = b) in
        let rec bb () =
          bad "one of your selections was a binding instance \
                      \of a variable"
        in
        if t = v then Some (List.hd ss')
        else
          match t with
            Binding (_, (bs, ss, us), env, pat) ->
              if E bs then bb () else None
          | Subst (_, r, P, vts) ->
              if E ((fst <* vts)) then bb () else None
          | _ -> None
      in
      if eqterms (rewrite cxt'' res, origterm) then cxt'', res
      else if eqterms (mapterm check P, origterm) then
        bad
          (("you can't make the substitution you want because " ^
              (if List.length ss = 1 then "your selection"
               else "one of your selections")) ^
             " was inside a binding, and involved a possible bound variable capture")
      else badsub ()
    let rec subterm2subst unify cxt pat t =
      let (cxt', v) = freshvar true cxt in
      let rec f hole subt =
        match unify (pat, subt) cxt' with
          Some cxt'' ->
            (* we have found a match, but there may be binding problems *)
            let t' = registerSubst (false, hole v, [v, rewrite cxt'' subt]) in
            if eqterms (rewrite cxt'' t', t) then Some (cxt'', t') else None
        | None -> None
      in
      findhole f t
  end
