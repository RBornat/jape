(* $Id$ *)

module type Type =
  sig
    type term
    type seq = Seq of (string * term * term) (* stile, left, right *)
  end

module Type : Type with type term = Term.Type.term =
struct
    type term = Term.Type.term
    type seq = Seq of (string * term * term) (* stile, left, right *)
end

module type Funs =
  sig
    type seq and symbol and term and element and vid and ('a, 'b) mapping and idclass
    
    val describeSeqs : (idclass * string * idclass) list -> unit
    val getsemanticturnstile : string -> string option
    val setsemanticturnstile : string -> string -> unit
    val parseSeq : unit -> seq
    val canstartSeq : symbol -> bool
    val resetsyntaxandturnstiles : unit -> unit

    val seqstring : seq -> string
    val smlseqstring : seq -> string
    val elementseqstring : seq -> string
    val catelim_seqstring : seq -> string list -> string list
    val catelim_smlseqstring : seq -> string list -> string list
    val catelim_elementseqstring : seq -> string list -> string list
    val alwaysshowturnstile : bool ref

    val string2sequent : string -> seq
    val seqexplode : seq -> string * term * term
    val seqvars :
      (term -> 'a list) -> ('a list -> 'a list -> 'a list) -> seq -> 'a list
    val seqVIDs : seq -> vid list
    val eqseqs : seq * seq -> bool
    val seqmatch :
      bool -> seq -> seq -> (term, term) mapping ->
        (term, term) mapping option
    val seqmatchvars :
      bool -> (term -> bool) -> seq -> seq -> (term, term) mapping ->
        (term, term) mapping option
    val remapseq : (term, term) mapping -> seq -> seq
    val mkSeq : string * element list * element list -> seq
    val maxseqresnum : seq -> int
    val syntacticturnstiles : unit -> string list
end

module Funs:  Funs with type seq = Type.seq
                    and type symbol = Symbol.Type.symbol
					and type ('a,'b) mapping = ('a,'b) Mappingfuns.M.mapping
					and type vid = Term.Type.vid
					and type term = Term.Type.term
					and type element = Term.Type.element
					and type idclass = Idclass.M.idclass
=
  struct
    open Type 

    open Idclass.M
    open Idclassfuns.M
    open Listfuns.M
    open Match.M
    open Mappingfuns.M
    open Miscellaneous.M
    open Optionfuns.M
    open Sml.M
    open Symbol.Funs
    open Symbol.Type
    open Term.Funs
    open Term.Store
    open Term.Termstring
    open Term.Type
    open Termparse.M    
    
    type seq = Type.seq
     and symbol = Symbol.Type.symbol
     and ('a,'b) mapping = ('a,'b) Mappingfuns.M.mapping
	 and vid = Term.Type.vid
	 and term = Term.Type.term
	 and element = Term.Type.element
	 and idclass = Idclass.M.idclass

    let bagkind = BagClass FormulaClass
    and listkind = ListClass FormulaClass
    and formulakind = FormulaClass

    type kind = Syntactic | Semantic

    (* Because everything in a tree has to have a resource number, single-formula
     * sides of sequents are encoded as lists (not bags because the unification of
     * lists is simpler :-)).  This causes difficulties, because
     * I keep forgetting ....
     * RB 17/x/1996
     *)
    let rec trueclass =
      function
        FormulaClass -> listkind
      | c -> c
    let validforms = [bagkind; listkind; formulakind]

    let rec syntaxclassstring =
      function
        FormulaClass -> "FORMULA"
      | BagClass FormulaClass -> "BAG"
      | ListClass FormulaClass -> "LIST"
      | c -> raise (Catastrophe_ ["syntaxclassstring "; unparseidclass c])
    let rec syntaxstring (kind, hyps, stile, concs) =
      (((((match kind with
             Syntactic -> "SEQUENT "
           | Semantic -> "(SEMANTIC)SEQUENT ") ^
            syntaxclassstring hyps) ^
           " ") ^
          stile) ^
         " ") ^
        syntaxclassstring concs
    let syntaxes : (kind * idclass * string * idclass) list ref = ref []

    let semanticturnstiles : (string, string) mapping ref = ref empty

    let rec syntacticsequents () =
         (function
            Syntactic, _, _, _ -> true
          | _ -> false) <| !syntaxes

    let rec syntacticturnstiles () = ((fun(_,_,stile,_)->stile) <* syntacticsequents ())

    let rec lookupsyntax stilestring =
      findfirst
        (fun (_, _, stile, _ as syn) ->
           if stile = stilestring then Some syn else None)
        !syntaxes

    let rec lookupSTILE st =
      match lookupsyntax st with
        Some syn -> syn
      | None -> raise (Catastrophe_ ["couldn't find syntax for "; st])

    let rec setsemanticturnstile syn sem =
      let rec bad ss =
        error
          (implode ["Error in SEMANTICTURNSTILE "; syn; " IS "; sem; ": "] ::
             ss)
      in
      let newsyntaxes =
        match lookupsyntax syn with
          None -> bad [syn; " is not a turnstile"]
        | Some (Semantic, hyps, _, concs) ->
            bad [syn; " is a semantic turnstile"]
        | Some (Syntactic, hyps, _, concs) ->
            match lookupsyntax sem with
              None -> (Semantic, hyps, sem, concs) :: !syntaxes
            | Some (Syntactic, _, _, _) ->
                bad [sem; " is a syntactic turnstile"]
            | Some (Semantic, hyps', _, concs' as sem') ->
                if hyps = hyps' && concs = concs' then !syntaxes
                else
                  bad
                    ["you cannot redeclare "; syntaxstring sem'; " as ";
                     syntaxstring (Semantic, hyps, sem, concs)]
      in
      let newturnstiles =
        match at (!semanticturnstiles, syn) with
          None ->
            enter (sem, STILE sem);
            ( ++ ) (!semanticturnstiles, ( |-> ) (syn, sem))
        | Some sem' ->
            if sem = sem' then !semanticturnstiles
            else bad ["semantic turnstile for "; syn; " is "; sem']
      in
      syntaxes := newsyntaxes; semanticturnstiles := newturnstiles
    let rec getsemanticturnstile syn = at (!semanticturnstiles, syn)
    let rec resetsyntaxandturnstiles () =
      syntaxes := []; semanticturnstiles := empty

    let rec describeSeqs ds =
      (* val show = triplestring idclassstring enQuote idclassstring "," *)
      let rec f (hyps, stile, concs) =
        match lookupsyntax stile with
          Some (kind, hyps', _, concs' as syn') ->
            if (kind = Syntactic && hyps = hyps') && concs = concs' then ()
            else
              error
                ["you cannot redeclare "; syntaxstring syn'; " as ";
                 syntaxstring (Syntactic, hyps, stile, concs)]
        | None ->
            if member (hyps, validforms) && member (concs, validforms) then
              begin
                (* consolereport ["accepting ", show syn]; *)
                syntaxes := (Syntactic, hyps, stile, concs) :: !syntaxes;
                enter (stile, STILE stile)
              end
            else
              error
                ["bad syntactic sequent syntax description ";
                 syntaxstring (Syntactic, hyps, stile, concs)]
      in
      (* consolereport ["describing ", bracketedliststring show "," ds]; *)
      List.iter f ds

    let rec parseSeq () =
      let rec formside el =
        registerCollection (trueclass FormulaClass, [el])
      in
      let rec conformside (found, c) =
        match found, c with
          (None, [el]), FormulaClass -> formside el
        | (_, []), FormulaClass ->
            raise
              (ParseError_
                 ["single formula expected on left-hand side of sequent; saw ";
                  symbolstring (currsymb ())])
        | (_, els), FormulaClass ->
            raise
              (ParseError_
                 ["single formula expected on left-hand side of sequent; found ";
                  termstring (Collection (None, listkind, els))])
        | (None, els), c -> registerCollection (c, els)
        | (Some c', els), c ->
            if c = c' then registerCollection (c, els)
            else
              raise
                (ParseError_
                   [syntaxclassstring c;
                    " expected on left-hand side of sequent; found ";
                    termstring (Collection (None, c', els))])
      in
      let rec parseside =
        function
          BagClass FormulaClass as c -> parseCollection c
        | ListClass FormulaClass as c -> parseCollection c
        | FormulaClass -> formside (registerElement (Nonum, parseTerm EOF))
        | x -> error ["internal error parseSeq parseside "; unparseidclass x]
      in
      match currsymb () with
        STILE st ->
          (* we can sometimes start with a stile ... *)
          let (_, hypform, _, concform) = lookupSTILE st in
          if hypform = formulakind then
            raise
              (ParseError_
                 ["badly formed sequent - lhs formula missing before "; st])
          else
            let _ = scansymb () in
            Seq (st, conformside ((None, []), hypform), parseside concform)
      | _ ->
          let first =
            parseElementList canstartTerm parseTerm commasymbol None
          in
          match currsymb (), first, syntacticsequents () with
            STILE st, _, _ ->
              let (_, hypform, _, concform) = lookupSTILE st in
              let _ = scansymb () in
              Seq (st, conformside (first, hypform), parseside concform)
          | _, (None, [el]), [_, hypform, st, concform] ->
              begin match hypform, concform with
                FormulaClass, _ ->
                  raise
                    (ParseError_
                       ["badly formed sequent - "; st; " expected after ";
                        termstring (formside el)])
              | _, FormulaClass ->
                  Seq (st, registerCollection (hypform, []), formside el)
              | _ ->
                  raise
                    (ParseError_
                       ["badly formed sequent - "; st; " expected after ";
                        termstring (Collection (None, hypform, [el]))])
              end
          | _, (_, els), _ ->
              raise
                (ParseError_
                   ["badly formed sequent - some kind of turnstile expected after ";
                    termstring (Collection (None, listkind, els))])

    let canstartSeq = canstartTerm

    let rec mkSeq (st, hs, gs) =
      let (_, hypform, _, concform) = lookupSTILE st in
      let sh = trueclass hypform in
      let sg = trueclass concform in
      Seq (st, registerCollection (sh, hs), registerCollection (sg, gs))

    let alwaysshowturnstile = ref false

    let rec sqs tf =
      fun (Seq (st, hs, gs)) ss ->
        let rec default () =
          let tail =
            st :: " " :: (if isemptycollection gs then ss else tf gs ss)
          in
          if isemptycollection hs then tail else tf hs (" " :: tail)
        in
        let (stkind, hypform, _, concform) = lookupSTILE st in
        match
          (!alwaysshowturnstile || stkind <> Syntactic) ||
          List.length (syntacticsequents ()) <> 1,
          hypform, concform
        with
          false, BagClass FormulaClass, FormulaClass ->
            if isemptycollection hs then tf gs ss else default ()
        | false, ListClass FormulaClass, FormulaClass ->
            if isemptycollection hs then tf gs ss else default ()
        | _ -> default ()
    let catelim_seqstring = sqs (catelim_collectionstring ", ")
    let catelim_smlseqstring = sqs catelim_smltermstring
    let catelim_elementseqstring =
      sqs
        (function
           Collection (_, _, es) ->
             catelim_liststring (catelim_smlelementstring catelim_termstring)
               ", " es
         | t -> catelim_termstring t)
    let seqstring = catelim2stringfn catelim_seqstring
    let smlseqstring = catelim2stringfn catelim_smlseqstring
    let elementseqstring = catelim2stringfn catelim_elementseqstring

    let rec seqexplode = fun (Seq s) -> s

    (* 
    fun parseComponent c () =
      case currsymb() of
        s0 as (PREFIX p) =>
         let fun nope() = (putbacksymb s0; parseTerm()) in
             case scansymb() of 
               ID(s,Some c0) => 
                 if c=c0 then (App(Id(p,OperatorClass),Id(s,c)) before scansymb()) 
                 else nope()
             | _ => nope()
         end
      | ID(s,Some c0) => if c=c0 then (Id(s,c) before scansymb()) else parseTerm()
      | _             => parseTerm()
    *)
      
    let rec string2sequent s = tryparse (fun _ -> parseSeq ()) s
    let rec seqvars termvars tmerge =
      fun (Seq (st, hs, gs)) -> tmerge (termvars hs) (termvars gs)
    let rec seqVIDs s = orderVIDs ((vid_of_var <* seqvars termvars tmerge s))
    let rec eqseqs =
      fun (Seq (st1, h1s, g1s), Seq (st2, h2s, g2s)) ->
        (st1 = st2 && eqterms (h1s, h2s)) && eqterms (g1s, g2s)
    let rec seqmatchvars matchbra ispatvar =
      fun (Seq (stpat, pathyps, patgoals)) ->
        fun (Seq (st, hyps, goals)) env ->
          if stpat <> st then None
          else
            (matchvars matchbra ispatvar pathyps hyps env &~~
               matchvars matchbra ispatvar patgoals goals)
    let rec seqmatch matchbra = seqmatchvars matchbra ismetav
    let rec remapseq env =
      fun (Seq (st, hs, gs)) -> Seq (st, remapterm env hs, remapterm env gs)
    let rec maxseqresnum =
      fun (Seq (st, hs, gs)) ->
        nj_fold (uncurry2 max) ((resnum2int <* elementnumbers hs))
          (nj_fold (uncurry2 max) ((resnum2int <* elementnumbers gs)) 0)
    
  end
