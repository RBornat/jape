(* $Id$ *)
   
module type T =
  sig
    type element
    
    (* Hits are what drawing packages (treedraw, boxdraw, and one day etc.) translate
     * clicks into.  They are also used to locate text selections.  Only conclusions are
     * treated with the transitivity transformation, so only conclusion clicks can be
     * sided.  
     *
     * Sels are what we translate collections of Hits into.
     * 
     * The interface ('server') is never supposed to report an ambiguous click, but 
     * a text selection in an ambiguous formula must be reported somehow, so we can't
     * get rid of AmbigHit as I would like.
     *
     * ''a is a path -- there are various kinds.
     *)
    type 'a hit = FormulaHit of 'a fhit | ReasonHit of 'a
    and 'a fhit =
        ConcHit of ('a * (element * side option))
      | HypHit of ('a * element)
      | AmbigHit of (('a * (element * side option)) * ('a * element))
    and side = Left | Right 
    (* In box display at least, a single displayed element may play more than one 
     * role. hitkind (should perhaps be hitpathkind) allows us to choose which we want.
     *)
    type hitkind = HitPath | PrunePath | LayoutPath
    (* Selections uniquely identify a single sequent.  
     * They are either conclusion+hypotheses, reason or nothing at all.
     * At present I countenance either a single conclusion selection, or none.
     *
     * Interfaces are supposed to sort out the user's clicks so that it's reasonable
     * to say that either a reason is selected, or some formulae are selected, or neither.
     *
     * Text selections are rolled up into formula selections, because why not?
     *
     * We distinguish 'text selection only' selections.
     *) 
    type 'a sel =
        FormulaSel of
          ('a * (element * side option) option * element list *
             ((element * side option) * string list) list *
             (element * string list) list * string list)
      | TextSel of (('a fhit * string list) list * string list)
      | ReasonSel of 'a
    val hitpath : 'a hit -> 'a option
    val fhitpath : 'a fhit -> 'a option
    val selpath : 'a sel -> 'a option
    val tranhitpath : ('a -> 'b) -> 'a hit -> 'b hit
    val tranfhitpath : ('a -> 'b) -> 'a fhit -> 'b fhit
    val transelpath : ('a -> 'b) -> 'a sel -> 'b sel
    val sidestring : side -> string
    val hitkindstring : hitkind -> string
    val hitstring : ('a -> string) -> 'a hit -> string
    val fhitstring : ('a -> string) -> 'a fhit -> string
    val selstring : ('a -> string) -> 'a sel -> string
  end
(* $Id$ *)

module M : T with type element = Term.Funs.element =
  struct
    open Sml.M
	
	type element = Term.Funs.element
	
	let bracketedliststring = Listfuns.M.bracketedliststring
	let elementstring       = Term.Termstring.smlelementstring Term.Termstring.termstring
	let optionmap           = Optionfuns.M.optionmap
	let optionstring        = Optionfuns.M.optionstring
	let pairstring          = Stringfuns.M.pairstring
	let triplestring        = Stringfuns.M.triplestring
	let sextuplestring      = Stringfuns.M.sextuplestring

    type side = Left | Right
    type 'a hit = FormulaHit of 'a fhit | ReasonHit of 'a
    and 'a fhit =
        ConcHit of ('a * (element * side option))
      | HypHit of ('a * element)
      | AmbigHit of (('a * (element * side option)) * ('a * element))
    type hitkind = HitPath | PrunePath | LayoutPath
    type 'a sel =
        FormulaSel of
          ('a * (element * side option) option * element list *
             ((element * side option) * string list) list *
             (element * string list) list * string list)
      | TextSel of (('a fhit * string list) list * string list)
      | ReasonSel of 'a
    let rec fhitpath =
      function
        ConcHit (p, _) -> Some p
      | HypHit (p, _) -> Some p
      | AmbigHit _ -> None
    let rec hitpath =
      function
        FormulaHit h -> fhitpath h
      | ReasonHit p -> Some p
    let rec selpath =
      function
        FormulaSel f -> Some (fst_of_6 f)
      | TextSel (ths, _) ->
          begin match
            optionmap (fhitpath <*> fst) ths
          with
            Some (p :: ps) ->
              if not (List.exists (fun p' -> p <> p') ps) then Some p else None
          | _ -> None
          end
      | ReasonSel p -> Some p
    let rec tranfhitpath a1 a2 =
      match a1, a2 with
        f, ConcHit (p, c) -> ConcHit (f p, c)
      | f, HypHit (p, h) -> HypHit (f p, h)
      | f, AmbigHit ((p1, s1), (p2, s2)) -> AmbigHit ((f p1, s1), (f p2, s2))
    let rec tranhitpath a1 a2 =
      match a1, a2 with
        f, FormulaHit h -> FormulaHit (tranfhitpath f h)
      | f, ReasonHit p -> ReasonHit (f p)
    let rec transelpath a1 a2 =
      match a1, a2 with
        f, FormulaSel (p, c, hs, tc, ths, tg) ->
          FormulaSel (f p, c, hs, tc, ths, tg)
      | f, TextSel (ths, tg) ->
          TextSel (List.map (fun (th, ss) -> tranfhitpath f th, ss) ths, tg)
      | f, ReasonSel p -> ReasonSel (f p)
    let rec sidestring =
      function
        Left -> "Left"
      | Right -> "Right"
    let rec hitkindstring =
      function
        HitPath -> "HitPath"
      | PrunePath -> "PrunePath"
      | LayoutPath -> "LayoutPath"
    let elsistring = pairstring elementstring (optionstring sidestring) ","
    let rec fhitstring pathstring s =
      let pelsistring = pairstring pathstring elsistring "," in
      let pelstring = pairstring pathstring elementstring "," in
      match s with
        ConcHit pc -> "ConcHit" ^ pelsistring pc
      | HypHit ph -> "HypHit" ^ pelstring ph
      | AmbigHit pch -> "AmbigHit" ^ pairstring pelsistring pelstring "," pch
    let rec hitstring a1 a2 =
      match a1, a2 with
        pathstring, FormulaHit h ->
          ("FormulaHit(" ^ fhitstring pathstring h) ^ ")"
      | pathstring, ReasonHit p -> ("ReasonHit(" ^ pathstring p) ^ ")"
    let sstring = bracketedliststring (fun s -> s) ","
    let rec selstring a1 a2 =
      match a1, a2 with
        pathstring, FormulaSel f ->
          "FormulaSel" ^
            sextuplestring pathstring (optionstring elsistring)
              (bracketedliststring elementstring ",")
              (bracketedliststring (pairstring elsistring sstring ",") ",")
              (bracketedliststring (pairstring elementstring sstring ",") ",")
              sstring "," f
      | pathstring, TextSel t ->
          "TextSel" ^
            pairstring
              (bracketedliststring
                 (pairstring (fhitstring pathstring) sstring ",") ",")
              sstring "," t
      | pathstring, ReasonSel p -> ("ReasonSel(" ^ pathstring p) ^ ")"
  end

