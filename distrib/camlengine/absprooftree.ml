(* $Id$ *)

module type T =
  sig
    type tree and structurerule and font
    type sequent and reason and text and term and element
    val subtrees : tree -> tree list
    val sequent : tree -> sequent
    val matched : tree -> element list * element list
    val reason : tree -> reason option
    val ismultistep : tree -> bool
    val ishiddencut : tree -> bool
    val explode : sequent -> string * element list * element list
    val element2text : (element -> string) -> element -> text
    val term2text : (term -> string) -> term -> text
    val reason2text : reason -> text
    val reason2fontNstring : reason -> font * string
    val validhyp : tree -> element -> int list -> bool
    val validconc : tree -> element -> int list -> bool
    val stillopen : tree -> int list -> bool
    val allTipConcs : tree -> int list -> (int list * element list) list
    val tip : tree -> int list -> sequent option
    val isStructureRulenode : tree -> structurerule -> bool
    val turnstile : string -> text
    val comma : unit -> text
  end

(* $Id$ *)

module M  : T =
  struct
    open Text
    open Displayfont
    open Treeformat
    open Prooftree
    open Prooftree.Visprooftree
    
    type tree = Visformat prooftree
    and structurerule = structurerule
    and sequent = seq
    and reason = string
    and element = element
    and text = text
    let explode =
      (fun (st, hs, gs) -> st, explodeCollection hs, explodeCollection gs) <*>
      seqexplode
    let rec isStructureRulenode node rule =
      match visprooftree.rule node with
        Some r -> isstructurerule rule r
      | None -> false
    let matched = thinned
    let rec allTipConcs tree ns =
      try
        let concs = visprooftree.allTipConcs (followPath tree (VisPath ns)) in
        List.map (fun (VisPath cpath, conc) -> ns @ cpath, conc) concs
      with
        FollowPath_ _ -> []
    let rec tip tree ns =
      try Some (findTip tree (VisPath ns)) with
        _ -> None
    let rec comma () =
      Text [Syllable (TermFont, symbolstring commasymbol ^ " ")]
    let rec turnstile st = Text [Syllable (TermFont, (" " ^ st) ^ " ")]
    let rec reason2text why = Text [Syllable (ReasonFont, why)]
    let rec reason2fontNstring why = ReasonFont, why
    let rec element2text elementstring e =
      Text [Syllable (TermFont, elementstring e)]
    let rec term2text termstring t = Text [Syllable (TermFont, termstring t)]
    let rec validhyp t el ns = visprooftree.validhyp t el (VisPath ns)
    let rec validconc t el ns = visprooftree.validconc t el (VisPath ns)
    let rec stillopen T = visprooftree.stillopen T <*> VisPath
    let rec ismultistep t =
      match format t with
        VisFormat (b, _) -> b
    let rec ishiddencut t =
      match format t with
        VisFormat (_, b) -> b
    let reason = reason proved
  end
