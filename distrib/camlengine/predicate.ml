(* $Id$ *)

module type Predicate =
  sig
    type term and vid and idclass
    val compilepredicate :
      (term -> bool) -> (term -> term list option) -> term -> term option
    val discardzeroarities :
      (term * (term list * term list list) list) list ->
        (term * (term list * term list list) list) list
    val findpredicates :
      (term -> bool) -> term list ->
        term * (term * (term list * term list list) list) list ->
        (term * (term list * term list list) list) list option
    val findpredicatevars :
      (term list * term list list) list -> term list option
    val interpretpredicates : bool ref
    val matchpredicate :
      bool -> (term -> bool) -> term -> (term * term list) option
    val predicatedebug : bool ref
    val predicatebindingstring :
      (term * (term list * term list list) list) list -> string
    exception Predicate_ of string list
  end
(* $Id$ *)

module
  Predicate
  (AAA :
    sig
      module idclass : Idclass
      module term : sig include Termtype include Termstore include Term end
      val ( <| ) : ('a -> bool) * 'a list -> 'a list
      val ( ||| ) : 'a list * 'b list -> ('a * 'b) list
      val bracketedliststring : ('a -> string) -> string -> 'a list -> string
      val consolereport : string list -> unit
      val earlierlist : ('a * 'a -> bool) -> 'a list * 'a list -> bool
      val eqbags : ('a * 'a -> bool) -> 'a list * 'a list -> bool
      val findfirst : ('a -> 'b option) -> 'a list -> 'b option
      val m_a_p : ('a -> 'b) * 'a list -> 'b list
      val member : 'a * 'a list -> bool
      val optionstring : ('a -> string) -> 'a option -> string
      val pairstring :
        ('a -> string) -> ('b -> string) -> string -> 'a * 'b -> string
      val sort : ('a * 'a -> bool) -> 'a list -> 'a list
      (* given op<, sorts in < order *)
      val sortedmerge : ('a * 'a -> bool) -> 'a list * 'a list -> 'a list
      val sortunique : ('a * 'a -> bool) -> 'a list -> 'a list
      exception Catastrophe_ of string list exception Zip
      
    end)
  :
  Predicate =
  struct
    open AAA
    open idclass open term
    
    
    
    
    let interpretpredicates = ref false
    let predicatedebug = ref false
    exception Predicate_ of string list
    (* aid for debuggers *)
    let predicatebindingstring =
      bracketedliststring
        (pairstring termstring
           (bracketedliststring
              (pairstring termliststring
                 (bracketedliststring termliststring ",") ",")
              ",")
           ", ")
        "; "
    (* check that a formula is an application which can be viewed as a predicate *)
    (* FormulaClass stops operators becoming predicates when interpretpredicates is on ... *)
    let rec matchpredicate all isabstraction t =
      match t with
        App (_, (Id (_, _, c) as P), arg) ->
          if !interpretpredicates && c = FormulaClass || isabstraction P then
            Some
              (P,
               (match debracket arg with
                  Tup (_, ",", ts) -> ts
                | arg -> [arg]))
          else None
      | Id (_, v, c) ->
          if all &&
             (!interpretpredicates && c = FormulaClass || isabstraction t)
          then
            Some (t, [])
          else None
      | _ -> None
    (* given a mapping from names to lists of variables, translate predicates into substitutions *)
    let rec compilepredicate isabstraction env t =
      match matchpredicate false isabstraction t with
        Some (P, ts) ->
          let _ =
            if !predicatedebug then
              consolereport
                ["compilepredicate spotted "; termstring P; "(";
                 termliststring ts; ")"]
          in
          let ts = m_a_p (mapterm (compilepredicate isabstraction env), ts) in
          begin match env P with
            Some vs ->
              let res =
                Some
                  (if vs = ts then P
                   else
                     try registerSubst (true, P, ( ||| ) (vs, ts)) with
                       Zip ->
                         raise (Catastrophe_ ["Zip in compilepredicates"]))
              in
              if !predicatedebug then
                consolereport
                  ["compilepredicate "; termstring P; "("; termliststring ts;
                   ") => "; optionstring termstring res];
              res
          | None -> raise (Catastrophe_ ["bad env in compilepredicates"])
          end
      | None -> None
    (* find predicates, record their arguments and the bindings which enclose them.
       Produces a list of predicates, each paired with a list of arguments, each
       paired with a list of binding contexts in which that predicate/argument pair
       occurs. Does an arity check. Designed to be foldtermed.
     *)
    let rec findpredicates isabstraction bs (t, pbs) =
      let rec addbinding newbs bs =
        sortedmerge earliervar (sort earliervar newbs, bs)
      in
      let rec insertinlist eq f k kvs =
        let rec g =
          function
            [] -> [k, f []]
          | (k', v') :: kvs ->
              if eq (k, k') then (k, f v') :: kvs else (k', v') :: g kvs
        in
        g kvs
      in
      let rec inserttbs =
        fun P ((ts : term list), bs) ->
          insertinlist
            (fun (ts, ts') ->
               if length ts <> length ts' then
                 raise
                   (Predicate_
                      ["predicate "; termstring P;
                       " is used inconsistently: "; "sometimes with ";
                       string_of_int (length ts); ", sometimes with ";
                       string_of_int (length ts'); " arguments."])
               else ts = ts')
            (fun bs' -> sortedmerge (earlierlist earliervar) (bs, bs')) ts
      in
      let rec insertP =
        fun ((P : term), tbs) ->
          insertinlist (fun (x, y) -> x = y)
            (fun tbss -> inserttbs P tbs tbss) P
      in
      let fp = findpredicates isabstraction in
      match t with
        Binding (_, (newbs, ss, us), _, _) ->
          Some
            (nj_fold (nj_foldterm (fp (addbinding newbs bs))) ss
               (nj_fold (nj_foldterm (fp bs)) us pbs))
      | Subst (_, r, P, vts) ->
          Some
            (foldterm (fp (addbinding (m_a_p ((fun(hash1,_)->hash1), vts)) bs))
               (nj_fold (nj_foldterm (fp bs)) (m_a_p ((fun(_,hash2)->hash2), vts)) pbs) P)
      | _ ->
          match matchpredicate true isabstraction t with
            Some (P, ts) ->
              Some
                (insertP (P, (ts, [bs])) (nj_fold (nj_foldterm (fp bs)) ts pbs))
          | _ -> None
    (* discard zero-arity 'predicates' -- only necessary for arity check *)
    let rec discardzeroarities pbs =
      ( <| )
        ((fun (_, (abss : (term list * 'a) list)) ->
            List.exists (fun ooo -> (fun ooo -> not (null ooo)) ((fun(hash1,_)->hash1) ooo))
              abss),
         pbs)
    (* To make a mapping from predicates to default args we prefer binding variables, if
     * suitable examples can be found.
     *)
    let rec findpredicatevars abss =
      findfirst
        (fun (ts, bss) ->
           if List.exists (fun bs -> eqbags eqterms (ts, bs)) bss then Some ts
           else None)
        abss
  end
