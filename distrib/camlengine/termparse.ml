(* $Id$ *)

module type T =
  sig
    type symbol and term and idclass and element
    val check : symbol -> unit
    val ignore : symbol -> unit
    val canstartTerm : symbol -> bool
    val canstartidentifier : symbol -> bool
    val parseVariable : unit -> term
    val parseidentifier : unit -> term
    val parseTerm : symbol -> term
    val parseBindingpattern : unit -> term
    val parseCollection : idclass -> term
    val parseElementList :
      (symbol -> bool) -> (symbol -> term) -> symbol -> idclass option ->
        idclass option * element list
    val parseList : (symbol -> bool) -> (symbol -> 'a) -> symbol -> 'a list
    val parseUnsepList : (symbol -> bool) -> (symbol -> 'a) -> 'a list
    val parsecurriedarglist : unit -> term list
    val tryparse : (symbol -> 'a) -> string -> 'a
    val tryparse_dbug : (symbol -> 'a) -> ('a -> string) -> string -> 'a
    val asTactic : ('a -> term) -> 'a -> term
    (* parse with no notice of name classification, non-vars allowed in Substs *)
    val checkTacticTerm : term -> unit
    (* raises Tacastrophe_ (really) if argument isn't a pukka term *)
  
    val string2term : string -> term
    val string2tactic : string -> term
    val declareOutRightfix : symbol list -> symbol -> unit
    val declareLeftMidfix : symbol list -> unit
    val resettermparse : unit -> unit
  end
(* $Id$ *)

module M : T with type element = Term.M.element
              and type idclass = Idclass.M.idclass
              and type term = Term.M.term
              and type symbol = Symboltype.M.symbol
=
  struct
    open Listfuns.M
    open Searchtree.M
    open Symbol.M
    open Symboltype.M
    open Idclass.M
    open Idclassfuns.M
    open Term.M
    open Binding.M
    open Optionfuns.M
    open Miscellaneous.M
    
    type element = Term.M.element
    type idclass = Idclass.M.idclass
    type term = Term.M.term
    type symbol = Symboltype.M.symbol
    
    (* stuff to handle variable leftfix/outfix syntax *)
    let symbeq : symbol * symbol -> bool = fun (x, y) -> x = y
    let rec mkalt sts def s =
      match
        findfirst (fun (s', t) -> if s = s' then Some t else None) sts
      with
        Some t -> t
      | None -> def
    let outrightfixtree : (symbol, symbol) searchtree ref =
      ref (emptysearchtree mkalt)
    let leftmidfixtree : (symbol, unit) searchtree ref =
      ref (emptysearchtree mkalt)
    let rec declareOutRightfix braseps ket =
      outrightfixtree :=
        addtotree (fun (x, y) -> x = y) !outrightfixtree (braseps, ket, false)
    let rec declareLeftMidfix syms =
      leftmidfixtree :=
        addtotree (fun (x, y) -> x = y) !leftmidfixtree (syms, (), false)
    let rec resettermparse () =
      outrightfixtree := emptysearchtree mkalt;
      leftmidfixtree := emptysearchtree mkalt;
      declareOutRightfix [BRA "("] (KET ")");
      ()
    let rec check s =
      if currsymb () = s then let _ = scansymb () in ()
      else
        raise
          (ParseError_
             ["Expected "; smlsymbolstring s; ", found ";
              smlsymbolstring (currsymb ())])
    let rec ignore s = if currsymb () = s then let _ = scansymb () in ()
    let rec parseUnsepList start f =
      if start (currsymb ()) then f EOF :: parseUnsepList start f else []
    let rec parseList start f sep =
      if start (currsymb ()) then
        let rec more () =
          if currsymb () = sep then let _ = scansymb () in one () else []
        and one () = let v = f sep in v :: more () in
        one ()
      else []
    let rec canstartTerm sy =
      match sy with
        ID _ -> true
      | UNKNOWN _ -> true
      | NUM _ -> true
      | STRING _ -> true
      | BRA _ -> true
      | PREFIX _ -> true
      | LEFTFIX _ -> true
      | _ ->(* temporary hack to allow Collection formulae *)
         canstartCollectionidclass sy
    let rec canstartAtom s =
      match s with
        ID _ -> true
      | UNKNOWN _ -> true
      | NUM _ -> true
      | STRING _ -> true
      | BRA _ -> true
      | LEFTFIX _ -> true
      | PREFIX _ -> true
      | _ -> false
    let rec canstartidentifier s =
      match s with
        ID _ -> true
      | UNKNOWN _ -> true
      | BRA "(" -> true
      | _ ->(* because it can be (op) ?? *)
         false
    let undecl : (string * term -> term) ref =
      ref
        (fun (s, t) ->
           raise (ParseError_ ["unclassified "; s; " "; termstring t]))
    let unvar : (term -> term) ref =
      ref (fun t -> raise (ParseError_ [termstring t; " is not a variable"]))
    let unclass : (string * term -> term) ref =
      ref
        (fun (id, t) ->
           raise
             (ParseError_
                [unparseidclass (idclass t); " "; id; " "; termstring t;
                 " in formula"]))
    let rec checkclass (id, t) =
      match idclass t with
        BagClass _ -> !unclass (id, t)
      | ListClass _ -> !unclass (id, t)
      | _ -> t
    
    (* moved from let block for OCaml *)
      
    let rec parseAtom () =
      let sy = currsymb () in
      let _ = scansymb () in
      match sy with
        ID (s, Some c) -> checkclass ("identifier", registerId (s, c))
      | ID (s, None) -> !undecl ("identifier", registerId (s, NoClass))
      | UNKNOWN (s, Some c) -> checkclass ("unknown", registerUnknown (s, c))
      | UNKNOWN (s, None) -> !undecl ("unknown", registerUnknown (s, NoClass))
      | NUM s ->
          begin try registerLiteral (Number (atoi s)) with
            _ -> raise (Catastrophe_ ["parseAtom can't atoi "; s])
          end
      | STRING s -> registerLiteral (String s)
      | BRA "(" -> parseBRA ()
      | BRA _ ->(* special cos of (op) *)
         parseOutfix sy
      | LEFTFIX (m, _) -> parseLeftfix m sy
      | PREFIX (m, s) ->
          checkAfterBra sy m;
          registerApp (registerId (s, OperatorClass), parseExpr m true)
      | s ->
          raise
            (ParseError_
               ["beginning of formula expected, found "; smlsymbolstring s])
    and checkAfterBra pre n =
      let sy = currsymb () in
      let rec checkpre m =
        if m < n then
          raise
            (ParseError_
               [smlsymbolstring sy; " (priority "; string_of_int m;
                ") found after "; smlsymbolstring pre; " (priority ";
                string_of_int n; ")"])
      in
      match sy with
        PREFIX (m, _) -> checkpre m
      | LEFTFIX (m, _) -> checkpre m
      | _ -> ()
    and checkAfterKet pre n =
      let sy = currsymb () in
      let rec checkpre m =
        if n < m then
          raise
            (ParseError_
               [smlsymbolstring sy; " (priority "; string_of_int m;
                ") found after "; smlsymbolstring pre; " (priority ";
                string_of_int n; ")"])
      in
      match sy with
        INFIX (m, _, _) -> checkpre m
      | INFIXC (m, _, _) -> checkpre m
      | _ -> if canstartAtom sy then checkpre !appfix
    and parseOutfix bra =
      (* check for empty bracketed form first *)
      match
        (fsmpos (rootfsm outrightfixtree) [bra] &~~
           (fun t ->
              match currsymb () with
                KET _ as ket ->
                  begin match scanfsm (fun _ -> ket) t [] ket with
                    Found (ket', []) ->
                      check ket';
                      Some
                        (registerFixapp ((symbolstring <* [bra; ket]), []))
                  | _ -> None
                  end
              | _ -> None))
      with
        Some t -> t
      | None -> putbacksymb bra; snd (parseOutRightfixTail 0 [])
    and parseRightfix m t =
      let (ket, t) = parseOutRightfixTail m [t] in checkAfterKet ket m; t
    and parseOutRightfixTail m ts =
      match
        scanstatefsm treecurr (treenext m false) (rootfsm outrightfixtree)
          ([], ts)
      with
        Found (ket, (ss, ts)) ->
          check ket;
          ket, registerFixapp ((symbolstring <* List.rev (ket :: ss)), List.rev ts)
      | NotFound (ss, _) ->
          raise
            (ParseError_
               ["expected "; liststring symbolstring " or " ss; "; found ";
                symbolstring (currsymb ())])
    and parseLeftfix m bra = putbacksymb bra; parseLeftMidfixTail m []
    and parseMidfix m t = parseLeftMidfixTail m [t]
    and parseLeftMidfixTail m ts =
      match
        scanstatefsm treecurr (treenext m true) (rootfsm leftmidfixtree)
          ([], ts)
      with
        Found (_, (ss, ts)) ->
          registerFixapp ((symbolstring <* List.rev ss), List.rev ts)
      | NotFound (ss, _) ->
          raise
            (ParseError_
               ["expected "; liststring symbolstring " or " ss; "; found ";
                symbolstring (currsymb ())])
    and treecurr _ = currsymb ()
    and treenext m a (sys, ts) =
      let s = currsymb () in
      let t = (let _ = scansymb () in parseExpr m a) in 
      s :: sys, t :: ts
    and parseVariable () =
      let rec okv =
        function
          Id (_, _, VariableClass) as t -> t
        | Unknown (_, _, VariableClass) as t -> t
        | Id _ as t -> !unvar t
        | Unknown _ as t -> !unvar t
        | t -> raise (ParseError_ [termstring t; " is not a variable"])
      in
      okv (parseAtom ())
    and parseBRA () =
      let rec defop s =
        let r = registerId (s, OperatorClass) in
        let _ =  scansymb () in
        check (KET ")"); r
      in
      let rec defexpr () =
        let r =
          (let t = parseterm (KET ")") in
		   match t with
			 Collection _ as t' -> t'
		   | Id (_, _, OperatorClass) as t' -> t'
		   | _ -> enbracket t)
        in check (KET ")"); r
      in
      match currsymb () with
        INFIX (_, _, s) -> defop s
      | INFIXC (_, _, s) -> defop s
      | POSTFIX (_, s) -> defop s
      | PREFIX (_, s) -> if peeksymb () = KET ")" then defop s else defexpr ()
      | sy ->
          if sy = KET ")" then
            (let _ = scansymb () in enbracket (registerTup (",", [])))
          else defexpr ()
    (* here a is not an associativity, it is 'left operator gets it' *)
    and parseExpr n a =
      let rec ( >> ) (m, n) = m > n || m = n && not a in
      let rec pe t =
        let sy = currsymb () in
        let rec pq (n', a', s) down =
          let rec nextt a =
            (let _ = scansymb () in checkAfterBra sy n'; parseExpr n' a)
          in
          let rec pts () =
            if currsymb () = sy then nextt true :: pts () else []
          in
          if ( >> ) (n', n) then
            pe (if a' = TupleAssoc then registerTup (s, t :: pts ())
                else down (s, nextt (a' = LeftAssoc)))
          else t
        in
        let rec pr (n', _) down =
          if ( >> ) (n', n) then pe (down n' t) else t
        in
        match sy with
          INFIX i ->
            pq i
               (fun (s, t') ->
                  registerApp
                    (registerId (s, OperatorClass),
                     registerTup (",", [t; t'])))
        | INFIXC i ->
            pq i
               (fun (s, t') ->
                  registerApp
                    (registerApp (registerId (s, OperatorClass), t), t'))
        | POSTFIX (n', s) ->
            if ( >> ) (n', n) then
              (let _ = scansymb () in
                checkAfterBra sy n';
                pe (registerApp (registerId (s, OperatorClass), t))
              )
            else t
        | RIGHTFIX i -> pr i parseRightfix
        | MIDFIX i -> pr i parseMidfix
        | SUBSTBRA ->
            if ( >> ) (!substfix, n) then
              let t = registerSubst (true, t, parseSubststuff ()) in
              checkAfterKet SUBSTKET !substfix; pe t
            else t
        | _ ->
            if canstartAtom sy && ( >> ) (!appfix, n) then
              pe (registerApp (t, parseExpr !appfix true))
            else t
      in
      pe (parseAtom ())
    and parseSubststuff () =
      let rec parseside b =
        if b then
          parseList canstartidentifier (fun _ -> parseVariable ()) commasymbol
        else parseList canstartTerm parseterm commasymbol
      in
      let _ = check SUBSTBRA in
      let xs = parseside !substsense in
      let _ = check SUBSTSEP in
      let ys = parseside (not !substsense) in
      let _ = check SUBSTKET in
      if List.length xs = List.length ys then
        if !substsense then (xs ||| ys) else (ys ||| xs)
      else
        (* Zip_ can't happen *)
        raise
          (ParseError_
             ["Substitution "; symbolstring SUBSTBRA;
              liststring termstring "," xs; symbolstring SUBSTSEP;
              liststring termstring "," ys; symbolstring SUBSTKET;
              " is unbalanced"])
    and parseterm fsy =
      if canstartCollectionidclass (currsymb ()) then
        let c = parseidclass "" in
        let (_, els) =
          parseElementList canstartTerm parseterm commasymbol (Some c)
        in
        registerCollection (c, els)
      else
        match fsy with
          INFIX (n, _, _) -> parseExpr n true
        | INFIXC (n, _, _) -> parseExpr n true
        | _ -> parseExpr 0 false
    and parseElementList starter parser__ sep k =
      let kind : idclass option ref = ref k in
      let rec iscoll c =
        match c with
          BagClass _ -> true
        | ListClass _ -> true
        | _ -> false
      in
      let rec parseElement sep =
        let rec getSegvar () =
          let rec id con s k' =
            let rec def k = (let _ = scansymb () in Some ([], con (s, k))) in
            match !kind with
              Some k ->
                if k = k' then def k
                else if iscoll k' then
                  raise
                    (ParseError_
                       [unparseidclass k'; " identifier found with ";
                        unparseidclass k; " identifier"])
                else None
            | None ->
                if iscoll k' then begin kind := Some k'; def k' end else None
          in
          match currsymb () with
            PREFIX (_, sp) as sy ->
              (let _ = scansymb () in
               match getSegvar () with
				 Some (ps, v) -> Some (registerId (sp, OperatorClass) :: ps, v)
			   | None -> putbacksymb sy; None
              )
          | ID (s, Some k') -> id registerId s k'
          | UNKNOWN (s, Some k') -> id registerUnknown s k'
          | _ -> None
        in
        match getSegvar () with
          Some r -> registerSegvar r
        | None -> registerElement (Nonum, parser__ sep)
      in
      let elements = parseList starter parseElement sep in !kind, elements
    and parseTerm sy =
      let t = parseterm sy in
      let rec doit t = mapterm findbinding t
      and findbinding t =
        match bindingstructure t with
          Some ((bs, us, ss), env, pat) ->
            Some
              (registerBinding
                 ((bs, (doit <* us), (doit <* ss)), env, pat))
        | None -> None
      in
      doit t
    (* -------------------------------------------------------------------------------- *)
    (*                               the interface functions                            *)
     
    let rec parseBindingpattern () = parseterm EOF
    (* no binding processing, please ... *)
       
    let rec parseidentifier _ =
      let rec okv =
        function
          Id _ as t -> t
        | Unknown _ as t -> t
        | t -> raise (ParseError_ [termstring t; " is not an identifier"])
      in
      okv (parseAtom ())
    (* for parsing sequents *)
    let rec parseCollection k =
      let (_, els) =
        parseElementList canstartTerm parseTerm commasymbol (Some k)
      in
      registerCollection (k, els)
    (* a desperate attempt to get around the fact that we can't actually give 
     * argument lists to commands.  Respect their bracketing, but treat unbracketed
     * applications as successive arguments
     *)
    let rec parsecurriedarglist _ =
      if canstartTerm (currsymb ()) then
        let rec flatten a1 a2 =
          match a1, a2 with
            App (_, f, a), rs -> flatten f (a :: rs)
          | t, rs -> t :: rs
        in
        flatten (parseTerm (KET ")")) []
      else []
    let rec tryparse =
      fun _R s ->
        let s = pushlex "" (Stream.of_string s) in
        let r = 
          (try (let r = _R EOF in check EOF; r) 
          with x -> poplex s; raise x)
        in poplex s; r
    let rec tryparse_dbug =
      fun _R p s ->
        let lex_s = pushlex "" (Stream.of_string s) in
        let r =
		  (try
			 let _ = consolereport ["tryparse_dbug \""; s; "\""] in
			 let r = _R EOF in
			 consolereport ["tryparse_dbug found "; p r];
			 check EOF;
			 consolereport ["tryparse_dbug EOF ok"];
			 r
		   with
			 x -> poplex lex_s; raise x)
         in poplex lex_s; r
    let rec asTactic f a =
      let u = !undecl in
      let v = !unvar in
      let c = !unclass in
      let rec cleanup () = undecl := u; unvar := v; unclass := c in
      undecl := (fun (_, t) -> t);
      unvar := (fun t -> t);
      unclass := (fun (_, t) -> t);
      let r = 
		(try f a with
		   exn -> cleanup (); raise exn)
      in cleanup (); r
    let rec checkTacticTerm t =
      let rec bang reason t =
        raise (Tacastrophe_ [reason; " "; termstring t])
      in
      let rec badvar =
        function
          Id (_, _, class__) as t ->
            if class__ <> VariableClass then
              bang "non-variable in substitution" t
            else None
        | Unknown (_, _, class__) as t ->
            if class__ <> VariableClass then
              bang "non-variable in substitution" t
            else None
        | _ -> None
      in
      let rec badterm =
        function
          Id (_, _, class__) as t ->
            if class__ = NoClass then bang "undeclared variable" t else None
        | Unknown (_, _, class__) as t ->
            if class__ = NoClass then bang "undeclared variable" t else None
        | Subst (_, _, _, vts) -> findfirst badvar ((fst <* vts))
        | _ -> None
      in
      let _ = findterm badterm t in
      ()
    let rec string2term s = tryparse parseTerm s
    let rec string2tactic s = tryparse (asTactic parseTerm) s
  end
