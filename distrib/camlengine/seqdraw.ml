(* $Id$ *)

module type T =
  sig
    type element and displayclass and 'a plan and pos and seq and textbox
    type planclass =
      ElementClass of (element * displayclass) | PunctClass | ReasonClass
    val makeelementplan :
      (element -> string) -> displayclass -> element -> pos -> planclass plan
    val makeseqplan :
      (element -> string) -> bool -> pos -> seq ->
        planclass plan list * textbox
    val planclass2displayclass : planclass -> displayclass
    val planclassstring : planclass -> string
    val seqdraw : pos -> textbox -> planclass plan list -> unit
    val seqelementpos : pos -> textbox -> planclass plan -> pos
  end
(* $Id$ *)

(* draw a sequent - used in treedraw and disproof *)
module M : T with type element = Draw.M.element
			  and type displayclass = Displayclass.displayclass
			  and type 'a plan = 'a Draw.M.plan
			  and type pos = Draw.M.pos
			  and type seq = Sequent.Funs.seq
			  and type textbox = Draw.M.textbox
=
  struct
    open Box 
    open Draw.M 
    open Displayclass
    
    type element = Draw.M.element
	 and displayclass = Displayclass.displayclass
	 and 'a plan = 'a Draw.M.plan
	 and pos = Draw.M.pos
	 and seq = Sequent.Funs.seq
	 and textbox = Draw.M.textbox
    
    let comma = Absprooftree.comma
    let elementstring = Term.Termstring.elementstring
    let explode = Absprooftree.explode
    let turnstile = Absprooftree.turnstile

    type planclass =
      ElementClass of (element * displayclass) | PunctClass | ReasonClass
    let rec planclassstring =
      function
        ElementClass (el, c) ->
          (("ElementClass(" ^ elementstring el) ^ displayclassstring c) ^ ")"
      | PunctClass -> "PunctClass"
      | ReasonClass -> "ReasonClass"
    let rec planclass2displayclass =
      function
        ElementClass (_, i) -> i
      | PunctClass -> DisplayPunct
      | ReasonClass -> DisplayReason
    let rec makeelementplan elementstring c el =
      element2plan elementstring el (ElementClass (el, c))
    (* sequents are drawn
                                    C - if lhs empty and rhs a single element
       [ H {, H } ] |- [ C {, C } ] - otherwise
 
       that knowledge is entirely encapsulated in the makeseqplan function.
     *)
 
    let rec makeseqplan elementstring showturnstiles p seq =
      let comma = comma () in
      let (commasize, _ as comminf) = text2textinfo comma in
      let mkcommaplan = textinfo2plan comminf PunctClass in
      let rec makeelementplan c el =
        element2plan elementstring el (ElementClass (el, c))
      in
      let rec rhs cs =
        things2plans (makeelementplan DisplayConc) mkcommaplan
          (fun _ -> [], emptytextbox) cs
      in
      match explode seq, showturnstiles with
        (st, [], ([c] as cs)), false -> rhs cs p
      | (st, hs, cs), _ ->
          let turnstile = turnstile st in
          let (turnstilesize, _ as stileinf) = text2textinfo turnstile in
          let mkstileplan = textinfo2plan stileinf PunctClass in
          things2plans (makeelementplan DisplayHyp) mkcommaplan
            (fun p -> plancons (mkstileplan p) (rhs cs)) hs p
    let rec seqdraw p seqbox seqplan =
      List.iter (drawplan planclass2displayclass (( +->+ ) (p, tbPos seqbox)))
        seqplan
    let rec seqelementpos p seqbox plan =
      ( +->+ ) (( +->+ ) (p, tbPos seqbox), tbPos (plantextbox plan))
  end
