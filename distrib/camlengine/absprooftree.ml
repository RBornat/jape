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

module M  : T with type structurerule = Thing.M.structurerule
			   and type tree          = Prooftree.Tree.Vistree.prooftree
			   and type sequent       = Sequent.Funs.seq
			   and type reason        = string
			   and type element       = Term.Funs.element
			   and type text          = Text.M.text
			   and type term          = Term.Funs.term
			   and type font          = Displayfont.M.displayfont
=
  struct
    open Text.M
    open Displayfont.M
    open Treeformat.VisFmt
    open Prooftree.Tree
    open Prooftree.Tree.Vistree
    open Sml.M
    
	let commasymbol       = Symbol.Funs.commasymbol
	let debracket         = Term.Funs.debracket
	let explodeCollection = Term.Funs.explodeCollection
	let isstructurerule   = Thing.M.isstructurerule
	let proved            = Proofstore.M.proved
	let seqexplode        = Sequent.Funs.seqexplode
	let symbolstring      = Symbol.Funs.symbolstring
	
	let termstring_invisbracketed    = Term.Termstring.termstring_invisbracketed
	let elementstring_invisbracketed = Term.Termstring.elementstring_invisbracketed
    
	type structurerule = Thing.M.structurerule
     and tree          = Prooftree.Tree.Vistree.prooftree
	 and sequent       = Sequent.Funs.seq
	 and reason        = string
	 and element       = Term.Funs.element
	 and text          = Text.M.text
	 and term          = Term.Funs.term
     and font          = Displayfont.M.displayfont
    
    let sequent = Vistree.sequent
    let subtrees = Vistree.subtrees
    
    let explode =
      (fun (st, hs, gs) -> st, explodeCollection hs, explodeCollection gs) <*>
      seqexplode
    let isStructureRulenode node rule =
      match Vistree.rule node with
        Some r -> isstructurerule rule r
      | None -> false
    let matched = thinned
    let allTipConcs tree ns =
      try
        let concs = Vistree.allTipConcs (followPath tree (VisPath ns)) in
        List.map (fun (VisPath cpath, conc) -> ns @ cpath, conc) concs
      with
        FollowPath_ _ -> []
    let tip tree ns =
      try Some (findTip tree (VisPath ns)) with
        _ -> None
    let comma () =
      Text [Syllable (TermFont, symbolstring commasymbol ^ " ")]
    let turnstile st = Text [Syllable (TermFont, (" " ^ st) ^ " ")]
    let reason2text why = Text [Syllable (ReasonFont, why)]
    let reason2fontNstring why = ReasonFont, why
    let element2text elementstring e =
      Text [Syllable (TermFont, elementstring e)]
    let term2text termstring t = Text [Syllable (TermFont, termstring t)]
    let validhyp t el ns = Vistree.validhyp t el (VisPath ns)
    let validconc t el ns = Vistree.validconc t el (VisPath ns)
    let stillopen t = Vistree.stillopen t <*> (fun v->VisPath v)
    let ismultistep t =
      match format t with VisFormat (b, _) -> b
    let ishiddencut t =
      match format t with VisFormat (_, b) -> b
    let reason = reason proved
  end
