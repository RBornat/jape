(* $Id$ *)

module type Paragraph =
  sig
    type term
    and seq
    and proviso
    and paraparam
    and tactic
    and menudata
    and paneldata
    and panelkind
    and dclick
    and forcedef
    type name
    type ruleheading = RuleHeading of (name * paraparam list * proviso list)
    type tacticheading = TacticHeading of (name * paraparam list)
    type proofstage and model
    type paragraph =
        AutoRule of (bool * tactic list)
      | Conjecture of (ruleheading * seq)
      | File of (string * paragraph list)
      | FontSpec of string
      | ForceDef of (term * forcedef)
      | HitDef of (dclick * tactic * seq)
      | InitVar of (name * term)
      | MacroDef of (tacticheading * term)
      | Menu of (name * menupara list)
      | Panel of (name * panelpara list * panelkind)
      | Proof of
          (name * proofstage * seq *
             (seq list * paraparam list * proviso list * tactic) *
             (seq * model) option)
      | RuleDef of (ruleheading * seq list * seq * bool)
      | RuleGroup of (ruleheading * paragraph list)
      | StructureRule of (string * name)
      | TacticDef of (tacticheading * tactic)
      | Theory of (ruleheading * paragraph list)
    and panelpara = Panelstuff of paneldata | Panelpara of paragraph
    and menupara = Menustuff of menudata | Menupara of paragraph
    exception Use_
    val file2paragraphs :
      (string list -> unit) ->
        (string list * string * string * int -> bool) -> string ->
        paragraph list
    val string2paragraph :
      (string list -> unit) ->
        (string list * string * string * int -> bool) -> string -> paragraph
  end

(* $Id$ *)

module
  Paragraph
  (AAA :
    sig
      module Listfuns : Listfuns
      module Usefile : Usefile
      module Idclass : Idclass
      module Idclassfuns : Idclassfuns
      module Symboltype : Symboltype
      module Symbol : Symbol
      module Term : Term
      module Termparse : Termparse
      module Sequent : sig include Sequentparse include Sequent end
      module Proviso : Proviso
      module Paraparam : Paraparam
      module Menu : Menu
      module Proofstage : Proofstage
      module Panelkind : Panelkind
      module Alert : Alert
      module Name : sig include Nametype(* sanctioned. RB *) include Name end
      module Doubleclick : DoubleClick
      module Forcedef : Forcedef
      val addbindingdirective :
        term.term list * term.term list * term.term list * term.term -> unit
      val atoi : string -> int
      val consolereport : string list -> unit
      val enQuote : string -> string
      val liststring : ('a -> string) -> string -> 'a list -> string
      val opt2bool : 'a option -> bool
      val readintasarg : term.term array option ref
      val tacticstring : doubleclick.tactic -> string
      val unSOME : 'a option -> 'a
      val transTactic : term.term -> doubleclick.tactic
      exception ParseError_ of string list
      exception Catastrophe_ of string list
      exception UnSOME_
      
    end)
  :
  Paragraph =
  struct
    open AAA
    open Forcedef
    open Idclass
    open Idclassfuns
    open Listfuns
    open Proofstage
    open Proviso
    open Paraparam
    open Menu
    open Panelkind
    open Name
    open Doubleclick
    open Sequent
    open Symboltype
    open Symbol
    open Term
    open Termparse
    open Usefile
    
    
    
    
    type ruleheading = RuleHeading of (name * paraparam list * proviso list)
    type tacticheading = TacticHeading of (name * paraparam list)
    type model = model
    type paragraph =
        AutoRule of (bool * tactic list)
      | Conjecture of (ruleheading * seq)
      | File of (string * paragraph list)
      | FontSpec of string
      | HitDef of (dclick * tactic * seq)
      | ForceDef of (term * forcedef)
      | InitVar of (name * term)
      | MacroDef of (tacticheading * term)
      | Menu of (name * menupara list)
      | Panel of (name * panelpara list * panelkind)
      | Proof of
          (name * proofstage * seq *
             (seq list * paraparam list * proviso list * tactic) *
             (seq * model) option)
      | RuleDef of (ruleheading * seq list * seq * bool)
      | RuleGroup of (ruleheading * paragraph list)
      | StructureRule of (string * name)
      | TacticDef of (tacticheading * tactic)
      | Theory of (ruleheading * paragraph list)
    and panelpara = Panelstuff of paneldata | Panelpara of paragraph
    and menupara = Menustuff of menudata | Menupara of paragraph
    exception Use_
    (* so-called because it is normally raised by a bad USE "file" *)
           
       (************************************************************************************)
       
    let rec namefromsymbol sy =
      let rec ok ooo = SOME (Name ooo) in
      match sy with
        ID (s, _) -> ok s
      | UNKNOWN (s, _) -> ok (metachar ^ s)
      | PREFIX (_, s) -> ok s
      | POSTFIX (_, s) -> ok s
      | LEFTFIX (_, s) -> ok s
      | BRA s -> ok s
      | SEP s -> ok s
      | KET s -> ok s
      | INFIX (_, _, s) -> ok s
      | INFIXC (_, _, s) -> ok s
      | NUM s -> ok s
      | STRING s -> ok s
      | _ -> NONE
    let rec stringfromsymbol sy =
      match namefromsymbol sy with
        SOME (Name s) -> SOME s
      | NONE -> NONE
    let rec currsymb_as_name () =
      let sy = currsymb () before scansymb () in
      match namefromsymbol sy with
        SOME s -> s
      | NONE ->
          raise
            (ParseError_
               ["Identifier or string expected; found "; smlsymbolstring sy])
    let currsymb_as_string ooo = namestring (currsymb_as_name ooo)
    (************************************************************************************)
    
    let ISWORD = SHYID "IS"
    let AREWORD = SHYID "ARE"
    let rec parseProvisos () =
      match currsymb () with
        SHYID "WHERE" ->
          scansymb ();
          flatten
            (parseList (fun _ -> true) (fun _ -> proviso.parseProvisos ())
               (SHYID "AND"))
      | _ -> []
    exception Matchinparseformal of string
    (* no longer spurious *)
       
    let rec parseRuleHeading mustdecl ignoreable =
      let name = currsymb_as_name () in
      let formals = parseformals mustdecl in
      let provisos = parseProvisos () in
      ignore ignoreable; RuleHeading (name, formals, provisos)
    and parseTacticHeading ignoreable =
      (* shouldn't use parseformals, but one step at a time ... *)
      let name = currsymb_as_name () in
      let formals = parseformals false in
      ignore ignoreable; TacticHeading (name, formals)
    (* we no longer permit WHERE inside a parameter list *)
    and parseformals mustdecl =
      match currsymb () with
        BRA "(" ->
          scansymb ();
          parseList (fun sy -> sy <> KET ")") (fun _ -> parseformal mustdecl)
            commasymbol before
            (match currsymb () with
               KET ")" -> scansymb ()
             | sy ->
                 raise
                   (ParseError_
                      ["\")\" expected in heading; found \""; symbolstring sy;
                       "\""]))
      | _ -> []
    and parseformal mustdecl =
      try
        match currsymb () with
          SHYID "ABSTRACTION" ->
            begin match scansymb () with
              ID (s, SOME c) -> scansymb (); Abstractionparam (s, c)
            | _ -> raise (Matchinparseformal "an identifer after ABSTRACTION")
            end
        | SHYID "OBJECT" ->
            begin match scansymb () with
              ID (s, SOME c) -> scansymb (); Objectparam (s, c)
            | _ -> raise (Matchinparseformal "an identifer after OBJECT")
            end
        | ID (s, SOME c) -> scansymb (); Ordinaryparam (s, c)
        | ID (s, NONE) ->
            if mustdecl then
              raise
                (ParseError_
                   ["unclassified identifier "; s;
                    " in formal parameter list"])
            else begin scansymb (); Ordinaryparam (s, NoClass) end
        | UNKNOWN (s, SOME c) -> scansymb (); Unknownparam (s, c)
        | UNKNOWN (s, NONE) ->
            if mustdecl then
              raise
                (ParseError_
                   ["unclassified unknown "; symbol.metachar; s;
                    " in formal parameter list"])
            else begin scansymb (); Unknownparam (s, NoClass) end
        | sy ->
            raise
              (Matchinparseformal "an identifier in formal parameter list")
      with
        Matchinparseformal s ->
          raise
            (ParseError_
               ["Expecting "; s; ", found "; smlsymbolstring (currsymb ())])
    let rec itisaparamlist () =
      match currsymb () with
        BRA "(" ->
          let rec res ps (v : bool) = app (fun s -> putbacksymb s) ps; v in
          let rec param ps =
            match currsymb () with
              ID _ -> sep (currsymb () :: ps before scansymb ())
            | UNKNOWN _ -> sep (currsymb () :: ps before scansymb ())
            | SHYID "ABSTRACTION" -> res ps true
            | SHYID "OBJECT" -> res ps true
            | _ -> res ps false
          and sep ps =
            if currsymb () = commasymbol then
              param (commasymbol :: ps before scansymb ())
            else if currsymb () = KET ")" then ket ps
            else res ps false
          and ket ps =
            res (KET ")" :: ps)
              begin
                scansymb ();
                member
                  (currsymb (),
                   [SHYID "IS"; SHYID "WHERE"; SHYID "FROM"; SHYID "INFER"])
              end
          in
          let ps = scansymb (); [BRA "("] in
          if currsymb () = KET ")" then ket ps else param ps
      | _ -> false
    let rec parsenovelsymb message =
      let sy = currnovelsymb () in
      let rec bang () =
        raise
          (ParseError_ [smlsymbolstring sy; " shouldn't appear "; message])
      in
      match sy with
        ID (s, _) -> scansymb (); s
      | BRA "(" -> bang ()
      | BRA bra -> scansymb (); bra
      | SEP s -> scansymb (); s
      | KET ")" -> bang ()
      | KET s -> scansymb (); s
      | INFIX (_, _, s) ->(* |    INFIX(",",_,s)  => bang() *)
         scansymb (); s
      | INFIXC (_, _, s) -> scansymb (); s
      | LEFTFIX (_, bra) -> scansymb (); bra
      | POSTFIX (_, s) -> scansymb (); s
      | MIDFIX (_, s) -> scansymb (); s
      | PREFIX (_, s) -> scansymb (); s
      | STRING s -> scansymb (); s
      | sy -> bang ()
    let rec parseFixitySYMB () = parsenovelsymb "in a Fixity directive"
    let rec acceptpreviousnovelsymb ok message =
      let newsy = currnovelsymb () in
      if ok newsy then begin scansymb (); symbolstring newsy end
      else parsenovelsymb message
    let rec parseNUM () =
      match currsymb () with
        NUM n -> scansymb (); atoi n
      | s ->
          raise
            (ParseError_ ["Expecting a number, found "; smlsymbolstring s])
    let rec parseClassID () =
      match currnovelsymb () with
        ID (s, _) -> scansymb (); s
      | s ->
          raise
            (ParseError_
               ["Expecting an identifier, found "; smlsymbolstring s])
    let rec processKEYBOARD report query =
      let _ = ignore ISWORD in
      let symbols =
        parseUnsepList canstartnovelsymb
          (fun _ -> currnovelsymb () before scansymb ())
      in
      set_oplist (( <* ) (symbolstring, symbols))
    let rec processClassDecl report query class__ =
      let rec doit s =
        match declareIdClass class__ s with
          SOME (s', class') ->(* showInputError report ["warning: ", unparseidclass class, " ", s,
                                    " may be confused with earlier CLASS ", 
                                    unparseidclass class', " ", s', " directive"]
           *)
           ()
        | NONE -> ()
      in
      app doit
        (parseUnsepList
           (function
              ID _ -> true
            | _ -> false)
           (fun _ -> parseClassID ()))
    let rec processCLASS report query =
      let class__ = parseidclass "after CLASS" in
      let rec doit s = app (warn s) (declareIdPrefix class__ s)
      and warn s (s', class') =(* showInputError report ["warning: CLASS ", unparseidclass class, " ", s, 
                                " may be confused with earlier ",
                                unparseidclass class', " ", s', " directive"]
       *)
       () in
      app doit
        (parseUnsepList
           (function
              ID _ -> true
            | _ -> false)
           (fun _ -> parseClassID ()))
    let rec processInfix =
      fun SYMBCON ->
        let l = parseNUM () in
        let a =
          match currsymb_as_string () with
            "L" -> LeftAssoc
          | "R" -> RightAssoc
          | "T" -> TupleAssoc
          | s ->
              (* to be done ...
              | "A" => AssocAssoc 
              | "C" => CommAssoc
              ... end of to be done
              *)
              raise
                (ParseError_
                   [s; " found where associativity (L, R or T) expected"])
        in
        let symbs =
          parseUnsepList canstartnovelsymb (fun _ -> parseFixitySYMB ())
        in
        app (fun s -> enter (s, SYMBCON (l, a, s))) symbs
    let rec processUnifix con =
      let n = parseNUM () in
      let symbs =
        parseUnsepList canstartnovelsymb (fun _ -> parseFixitySYMB ())
      in
      app (fun s -> enter (s, con (n, s))) symbs
    let rec processSubstfix report query =
      substfix := parseNUM ();
      if canstartnovelsymb (currsymb ()) then
        let bra =
          acceptpreviousnovelsymb (fun s -> s = SUBSTBRA)
            "as opening substitution bracket"
        in
        let t1 = parseidentifier () in
        let sep =
          acceptpreviousnovelsymb (fun s -> s = SUBSTSEP)
            "as substitution separator"
        in
        let t2 = parseidentifier () in
        let ket =
          acceptpreviousnovelsymb (fun s -> s = SUBSTKET)
            "as closing substitution bracket"
        in
        enter (bra, SUBSTBRA);
        enter (sep, SUBSTSEP);
        enter (ket, SUBSTKET);
        match idclass t1, idclass t2 with
          VariableClass, FormulaClass -> substsense := true
        | FormulaClass, VariableClass -> substsense := false
        | _ ->
            raise
              (ParseError_
                 ["in SUBSTFIX definition, one of "; termstring t1; " and ";
                  termstring t2;
                  " must be CLASS VARIABLE, and the other CLASS FORMULA"])
    let rec parseUntilSHYID parser__ =
      if match currsymb () with
           EOF -> true
         | SHYID _ -> true
         | _ -> false
      then
        []
      else parser__ () :: parseUntilSHYID parser__
    let rec processLeftMidfix con =
      let n = parseNUM () in
      let bra = parseFixitySYMB () in
      let symbs = parseUntilSHYID parseFixitySYMB in
      app (fun sep -> enter (sep, SEP sep)) symbs;
      enter (bra, con (n, bra));
      declareLeftMidfix (( <* ) (lookup, bra :: symbs))
    let rec processOutRightfix name con =
      let bra = parseFixitySYMB () in
      let ms = parseUntilSHYID parseFixitySYMB in
      match rev ms with
        [] -> raise (ParseError_ [name; " "; bra; " has no closing bracket"])
      | ket :: ms' ->
          app (fun m -> enter (m, SEP m)) ms';
          enter (bra, con bra);
          enter (ket, KET ket);
          declareOutRightfix (( <* ) (lookup, bra :: rev ms')) (lookup ket)
    let rec processRightfix () =
      let n = parseNUM () in
      processOutRightfix "RIGHTFIX" (fun bra -> RIGHTFIX (n, bra))
    let rec processOutfix () = processOutRightfix "OUTFIX" BRA
    let rec processBind () =
      let vars =
        parseUnsepList canstartidentifier (fun _ -> parseVariable ())
      in
      let scope =
        match currsymb () with
          SHYID "SCOPE" ->
            scansymb ();
            parseUnsepList canstartidentifier (fun _ -> parseidentifier ())
        | _ -> []
      in
      let body = ignore (SHYID "IN"); parseBindingpattern () in
      let varsinbody = ( <| ) (ismetav, termvars body) in
      if not
           ((subset (vars, varsinbody) && subset (scope, varsinbody)) &&
            null (scope INTER vars))
      then
        raise
          (ParseError_
             ["Component names badly chosen in BIND directive. ";
              "Bound variables are "; liststring termstring "," vars;
              "; scopes are "; liststring termstring "," scope;
              "; names in formula are "; liststring termstring "," varsinbody;
              "; formula is "; termstring body])
      else
        addbindingdirective
          (vars, scope, slosh (slosh (varsinbody, vars), scope), body)
    let rec processPatchAlert () =
      let rec parsebuttonspec _ =
        scansymb ();
        (* we know there's a BRA there *)
        match currsymb () with
          STRING b ->
            scansymb ();
            begin match currsymb () with
              KET ")" -> scansymb (); b, NONE
            | s ->
                if s = commasymbol then
                  begin
                    scansymb ();
                    (b, SOME (parsealertspec ())) before
                      (match currsymb () with
                         KET ")" -> scansymb ()
                       | s ->
                           raise
                             (ParseError_
                                ["bracket expected after alert spec in PATCHALERT, found ";
                                 symbolstring (currsymb ())]))
                  end
                else
                  raise
                    (ParseError_
                       ["comma or bracket expected after button label in PATCHALERT, found ";
                        symbolstring (currsymb ())])
            end
        | _ ->
            raise
              (ParseError_
                 ["button label expected in PATCHALERT, found ";
                  symbolstring (currsymb ())])
      and parsealertspec _ =
        match currsymb () with
          STRING m ->
            scansymb ();
            begin match parsebuttonspecs () with
              [] ->
                raise
                  (ParseError_ ["alert in PATCHALERT has no button specs"])
            | bs -> alert.Alert (m, bs, parsedefaultindex ())
            end
        | s ->
            let rec bang () =
              raise
                (ParseError_
                   ["string, HowToSelect, HowToFormulaSelect or HowToDrag \
                                         \expected in alert spec, found ";
                    symbolstring s])
            in
            match stringfromsymbol s with
              SOME "HowToTextSelect" -> scansymb (); alert.HowToTextSelect
            | SOME "HowToFormulaSelect" ->
                scansymb (); alert.HowToFormulaSelect
            | SOME "HowToDrag" -> scansymb (); alert.HowToDrag
            | _ -> bang ()
      and parsebuttonspecs () =
        parseUnsepList
          (function
             BRA "(" -> true
           | _ -> false)
          parsebuttonspec
      and parsedefaultindex () =
        match currsymb () with
          NUM s -> atoi s before scansymb ()
        | _ -> 0
      in
      match currsymb () with
        STRING h ->
          scansymb ();
          begin match currsymb () with
            BRA "(" ->
              (* no replacement message, but there are buttons *)
              alert.patchalert
                (h,
                 alert.Alert ("", parsebuttonspecs (), parsedefaultindex ()))
          | _ -> alert.patchalert (h, parsealertspec ())
          end
      | _ ->
          raise
            (ParseError_
               ["string expected after PATCHALERT, found ";
                symbolstring (currsymb ())])
    let rec parseAntecedents starter =
      parseList starter (fun _ -> parseSeq ()) (SHYID "AND") before check
        (SHYID "INFER")
    let rec parseButtonDefault f =
      match currsymb () with
        SHYID "INITIALLY" -> scansymb (); SOME (f ())
      | _ -> NONE
    let rec parseCheckBox report query con =
      let var = currsymb_as_name () in
      let label = currsymb_as_name () in
      let defval = parseButtonDefault currsymb_as_string in
      con (var, label, ("true", "false"), defval)
    let rec parseRadioButton report query con =
      let var = currsymb_as_name () in
      let rec parseentry sep =
        let label = currsymb_as_name () in
        let value = ignore ISWORD; currsymb_as_string () in label, value
      in
      ignore ISWORD;
      match parseList (fun _ -> true) parseentry (SHYID "AND") with
        [] ->
          showInputError report
            ["null value/label list in RADIOBUTTON directive"];
          raise Use_
      | entries ->
          let defval = parseButtonDefault currsymb_as_string in
          match currsymb () with
            SHYID "END" -> scansymb (); con (var, entries, defval)
          | s ->
              showInputError report
                ["found symbol "; smlsymbolstring s;
                 " in RADIOBUTTON directive - expecting END"];
              raise Use_
    let structurerulestrings =
      ["CUT"; "IDENTITY"; "LEFTWEAKEN"; "REFLEXIVE"; "RIGHTWEAKEN";
       "TRANSITIVE"; "WEAKEN"]
    (* at the moment all we do is to check that names in params and provisos are from the rule *)
    let rec checkvalidruleheading report objkind wherekind =
      fun (RuleHeading (name, params, provisos)) antes conseq fbvs ->
        let bodyvars =
          fold tmerge (( <* ) (seqvars termvars tmerge, conseq :: antes)) []
        in
        let paramvars = sort earliervar (( <* ) (paramvar, params)) in
        let provars =
          fold tmerge (( <* ) (provisovars termvars tmerge, provisos)) []
        in
        (* don't evaluate fbvs unless there is an apparent error, and don't do it twice *)
        let obvs : term list option ref = ref NONE in
        let rec check vs errorf =
          match !obvs with
            NONE ->
              begin match sorteddiff earliervar vs bodyvars with
                [] -> ()
              | _ ->
                  (* nothing wrong - the usual case *)
                  obvs := SOME (tmerge (bodyvars, fbvs ())); check vs errorf
              end
          | SOME bvs ->
              match sorteddiff earliervar vs bvs with
                [] -> ()
              | extras ->(* definitely nothing wrong *)
                 errorf extras
        in
        let rec add_an_s s b = if b then s ^ "s" else s in
        let rec error place has vs =
          begin match split (fun v -> member (v, bodyvars)) vs with
            [], _ ->
              showInputError report
                [place; " "; objkind; " "; namestring name; " "; has; " ";
                 add_an_s "variable" (length vs > 1); " ";
                 liststring2 termstring ", " " and " vs;
                 " which aren't in the "; wherekind; "."]
          | _, [] ->
              showInputError report
                [place; " "; objkind; " "; namestring name; " "; has;
                 " duplicate "; add_an_s "variable" (length vs > 1); " ";
                 liststring2 termstring ", " " and " vs; "."]
          | dups, rogues ->
              showInputError report
                [place; " "; objkind; " "; namestring name; " "; has;
                 " duplicate "; add_an_s "variable" (length dups > 1); " ";
                 liststring2 termstring ", " " and " dups; ", and also ";
                 add_an_s "variable" (length rogues > 1); " ";
                 liststring2 termstring ", " " and " rogues;
                 " which aren't in the "; wherekind; "."]
          end;
          raise Use_
        in
        check paramvars (error "parameter list" "includes");
        check provars
          (error (add_an_s "proviso" (length provisos > 1))
             (add_an_s "contain" (length provisos = 1)))
    let rec parseParagraph report query =
      let sy = currsymb () in
      let rec more () = parseParagraph report query in
      match sy with
        SHYID "AUTOMATCH" -> scansymb (); SOME (parseAutoRule true)
      | SHYID "AUTOUNIFY" -> scansymb (); SOME (parseAutoRule false)
      | SHYID "BIND" -> scansymb (); processBind (); more ()
      | SHYID "CONCHIT" -> scansymb (); SOME (parseHitDef DClickConc)
      | SHYID "CONSTANT" ->
          scansymb (); processClassDecl report query ConstantClass; more ()
      | SHYID "CLASS" -> scansymb (); processCLASS report query; more ()
      | SHYID "CONJECTUREPANEL" ->
          scansymb (); SOME (parseConjecturePanel report query)
      | SHYID "CURRENTPROOF" ->
          scansymb (); SOME (parseProof report InProgress)
      | SHYID "CUT" -> scansymb (); SOME (parseStructure "CUT")
      | SHYID "DERIVED" -> scansymb (); SOME (parseDerived report)
      | SHYID "DISPROOF" -> scansymb (); SOME (parseProof report Disproved)
      | SHYID "FONTS" -> scansymb (); SOME (parseFontSpec ())
      | SHYID "FORCE" -> scansymb (); SOME (parseForceDefSpec ())
      | SHYID "FORMULA" ->
          raise (ParseError_ ["FORMULA without CLASS is meaningless"])
      | SHYID "GIVENPANEL" -> scansymb (); SOME (parseGivenPanel report query)
      | SHYID "HYPHIT" -> scansymb (); SOME (parseHitDef DClickHyp)
      | SHYID "IDENTITY" -> scansymb (); SOME (parseStructure "IDENTITY")
      | SHYID "INFIX" -> scansymb (); processInfix INFIX; more ()
      | SHYID "INFIXC" -> scansymb (); processInfix INFIXC; more ()
      | SHYID "JUXTFIX" -> scansymb (); appfix := parseNUM (); more ()
      | SHYID "KEYBOARD" -> scansymb (); processKEYBOARD report query; more ()
      | SHYID "INITIALISE" -> scansymb (); SOME (parseInitialise ())
      | SHYID "LEFTFIX" -> scansymb (); processLeftMidfix LEFTFIX; more ()
      | SHYID "LEFTWEAKEN" -> scansymb (); SOME (parseStructure "LEFTWEAKEN")
      | SHYID "MACRO" ->
          scansymb ();
          SOME (MacroDef (parseTacticHeading ISWORD, asTactic parseTerm EOF))
      | SHYID "MENU" -> scansymb (); SOME (parseMenu report query)
      | SHYID "MIDFIX" -> scansymb (); processLeftMidfix MIDFIX; more ()
      | SHYID "NUMBER" ->
          scansymb (); processClassDecl report query NumberClass; more ()
      | SHYID "OUTFIX" -> scansymb (); processOutfix (); more ()
      | SHYID "PATCHALERT" -> scansymb (); processPatchAlert (); more ()
      | SHYID "POSTFIX" -> scansymb (); processUnifix POSTFIX; more ()
      | SHYID "PREFIX" -> scansymb (); processUnifix PREFIX; more ()
      | SHYID "PROOF" -> scansymb (); SOME (parseProof report Proved)
      | SHYID "REFLEXIVE" -> scansymb (); SOME (parseStructure "REFLEXIVE")
      | SHYID "RIGHTFIX" -> scansymb (); processRightfix (); more ()
      | SHYID "RIGHTWEAKEN" ->
          scansymb (); SOME (parseStructure "RIGHTWEAKEN")
      | SHYID "RULE" -> scansymb (); SOME (RuleDef (parseRule report true))
      | SHYID "RULES" -> scansymb (); SOME (parseRules report true)
      | SHYID "STRING" ->
          scansymb (); processClassDecl report query StringClass; more ()
      | SHYID "SEMANTICTURNSTILE" ->
          scansymb (); processSemanticTurnstileSpec (); more ()
      | SHYID "SEQUENT" -> scansymb (); processSequentSpec (); more ()
      | SHYID "STRUCTURERULE" -> scansymb (); SOME (parseStructureRule ())
      | SHYID "SUBSTFIX" -> scansymb (); processSubstfix report query; more ()
      | SHYID "TACTIC" ->
          scansymb ();
          SOME
            (TacticDef
               (parseTacticHeading ISWORD,
                transTactic (asTactic parseTerm EOF)))
      | SHYID "TACTICPANEL" ->
          scansymb (); SOME (parseTacticPanel report query)
      | SHYID "THEOREM" -> scansymb (); SOME (parseTheorem report)
      | SHYID "THEOREMS" -> scansymb (); SOME (parseTheorems report)
      | SHYID "THEORY" -> scansymb (); SOME (parseTheory report query)
      | SHYID "TRANSITIVE" -> scansymb (); SOME (parseStructure "TRANSITIVE")
      | SHYID "USE" ->
          scansymb (); SOME (parseUse report query before scansymb ())
      | SHYID "VARIABLE" ->
          scansymb (); processClassDecl report query VariableClass; more ()
      | SHYID "WEAKEN" -> scansymb (); SOME (parseStructure "WEAKEN")
      | _ -> NONE
    and processSequentSpec () =
      let rec f _ =
        let s1 = parseidclass "on left-hand side of SEQUENT specification" in
        let s2 =
          acceptpreviousnovelsymb
            (function
               STILE s -> true
             | _ -> false)
            "as entailment in SEQUENT specification"
        in
        let s3 = parseidclass "on right-hand side of SEQUENT specification" in
        s1, s2, s3
      in
      ignore ISWORD; describeSeqs (parseList (fun _ -> true) f (SHYID "AND"))
    and processSemanticTurnstileSpec () =
      let syn =
        match currsymb () with
          STILE s -> s before scansymb ()
        | sy ->
            raise
              (ParseError_
                 ["turnstile expected after SEMANTICTURNSTILE: found ";
                  symbolstring sy])
      in
      let _ = ignore ISWORD in
      let sem =
        acceptpreviousnovelsymb
          (function
             STILE s -> true
           | _ -> false)
          "as semantic turnstile"
      in
      sequent.setsemanticturnstile syn sem
    and parseRule report axiom =
      if member
           (currsymb (),
            [BRA "("; SHYID "WHERE"; SHYID "FROM"; SHYID "INFER"; SHYID "IS"])
      then
        parseUnnamedRule report NONE axiom
      else
        let heading = parseRuleHeading true ISWORD in
        let (antes, conseq) = parseRulebody () in
        let _ =
          checkvalidruleheading report "rule" "body of the rule" heading antes
            conseq (fun _ -> [])
        in
        heading, antes, conseq, axiom
    and parseUnnamedRule report nopt axiom =
      let params = if itisaparamlist () then parseformals true else [] in
      let provisos = parseProvisos () in
      let _ = ignore ISWORD in
      let (antes, conseq) = parseRulebody () in
      let name =
        Name
          (match nopt with
             SOME n -> n
           | NONE ->
               match antes, conseq with
                 [], _ -> seqstring conseq
               | _ ->
                   implode
                     ["FROM ";
                      implode
                        (interpolate " AND " (( <* ) (seqstring, antes)));
                      " INFER "; seqstring conseq])
      in
      let heading = RuleHeading (name, params, provisos) in
      let _ =
        checkvalidruleheading report "rule" "body of the rule" heading antes
          conseq (fun _ -> [])
      in
      heading, antes, conseq, axiom
    and parseRulebody () =
      match currsymb () with
        SHYID "FROM" ->
          scansymb (); parseAntecedents (fun _ -> true), parseSeq ()
      | _ -> ignore (SHYID "INFER"); [], parseSeq ()
    and parseParaList report query =
      match parseParagraph report query with
        SOME p -> p :: parseParaList report query
      | NONE ->
          match currsymb () with
            SHYID "END" -> scansymb (); []
          | EOF -> []
          | sy ->
              raise
                (ParseError_
                   ["Error: expecting symbol beginning paragraph; found ";
                    smlsymbolstring sy])
    and parseMenu report query =
      let parastarters =
        ["RULE"; "RULES"; "DERIVED"; "TACTIC"; "THEOREM"; "THEOREMS";
         "THEORY"; "PROOF"; "CURRENTPROOF"]
      in
      let starters =
        ["ENTRY"; "BUTTON"; "RADIOBUTTON"; "CHECKBOX"; "SEPARATOR"] @
          parastarters
      in
      let parasymbs = ( <* ) (SHYID, parastarters) in
      let symbs = ( <* ) (SHYID, starters) in
      let rec mkentry canstart getitem prefix toitem =
        let itemname = currsymb_as_name () in
        let menukey =
          match currsymb () with
            SHYID "MENUKEY" -> scansymb (); SOME (currsymb_as_string ())
          | _ -> NONE
        in
        let item =
          prefix ^
            (match currsymb () with
               SHYID "IS" -> scansymb (); getitem ()
             | _ ->
                 if canstart (currsymb ()) then getitem ()
                 else toitem itemname)
        in
        Menustuff (Mentry (itemname, menukey, item))
      in
      let rec parseentry () =
        if member (currsymb (), parasymbs) then
          Menupara (unSOME (parseParagraph report query))
        else
          match currsymb () with
            SHYID "ENTRY" ->
              scansymb ();
              mkentry canstartTerm (fun () -> parseTacticEntry report)
                "apply " parseablenamestring
          | SHYID "BUTTON" ->
              scansymb (); mkentry canstartCommand parseCommand "" namestring
          | SHYID "RADIOBUTTON" ->
              scansymb ();
              Menustuff (parseRadioButton report query Mradiobutton)
          | SHYID "CHECKBOX" ->
              scansymb (); Menustuff (parseCheckBox report query Mcheckbox)
          | SHYID "SEPARATOR" -> scansymb (); Menustuff Mseparator
          | s ->
              showInputError report
                ["internal error in parseMenu: found "; smlsymbolstring s];
              raise Use_
      in
      let mlabel = currsymb_as_name () in
      let _ = ignore ISWORD in
      let m =
        Menu
          (mlabel,
           parseUnsepList (fun s -> member (s, symbs))
             (fun _ -> parseentry ()))
      in
      match currsymb () with
        SHYID "END" -> scansymb (); m
      | s ->
          showInputError report
            ((["found symbol "; smlsymbolstring s; " in menu description: ";
               "expecting one of "] @
                interpolate ", " starters) @
               [" or END"]);
          raise Use_
    and parseTacticEntry report =
      try tacticstring (transTactic (asTactic parseTerm EOF)) with
        ParseError_ ss -> showInputError report ss; raise Use_
    and parseConjecturePanel report query =
      parsePanel report query ConjecturePanelkind
        ["ENTRY"; "BUTTON"; "RADIOBUTTON"; "CHECKBOX"]
        ["THEOREM"; "THEOREMS"; "DERIVED"; "PROOF"; "CURRENTPROOF"]
    and parseTacticPanel report query =
      parsePanel report query TacticPanelkind
        ["ENTRY"; "BUTTON"; "RADIOBUTTON"; "CHECKBOX"]
        ["THEORY"; "RULE"; "RULES"; "DERIVED"; "TACTIC"; "THEOREM";
         "THEOREMS"; "PROOF"; "CURRENTPROOF"]
    and parseGivenPanel report query =
      parsePanel report query GivenPanelkind
        ["BUTTON"; "RADIOBUTTON"; "CHECKBOX"] []
    and parsePanel report query panelkind entrystarters parastarters =
      let starters = entrystarters @ parastarters in
      let entrysymbs = ( <* ) (SHYID, entrystarters) in
      let parasymbs = ( <* ) (SHYID, parastarters) in
      let symbs = ( <* ) (SHYID, starters) in
      let rec parseentry () =
        if member (currsymb (), parasymbs) then
          Panelpara (unSOME (parseParagraph report query))
        else
          match currsymb () with
            SHYID "ENTRY" ->
              let itemname = scansymb (); currsymb_as_name () in
              let item =
                match currsymb () with
                  SHYID "IS" -> scansymb (); parseTacticEntry report
                | _ ->
                    if canstartTerm (currsymb ()) then parseTacticEntry report
                    else parseablenamestring itemname
              in
              Panelstuff (Pentry (itemname, item))
          | SHYID "BUTTON" ->
              let itemname =
                scansymb (); currsymb_as_name () before ignore ISWORD
              in
              let rec getcmd () =
                match currsymb () with
                  SHYID "LABEL" -> scansymb (); LabelInsert :: getcmd ()
                | SHYID "COMMAND" -> scansymb (); CommandInsert :: getcmd ()
                | sy ->
                    if canstartCommand sy then
                      StringInsert (parseCommand ()) :: getcmd ()
                    else []
              in
              let itemcmd =
                match getcmd () with
                  [] ->
                    raise
                      (ParseError_
                         ["command expected after BUTTON, found ";
                          symbolstring (currsymb ())])
                | cs -> cs
              in
              Panelstuff (Pbutton (itemname, itemcmd))
          | SHYID "RADIOBUTTON" ->
              scansymb ();
              Panelstuff (parseRadioButton report query Pradiobutton)
          | SHYID "CHECKBOX" ->
              scansymb (); Panelstuff (parseCheckBox report query Pcheckbox)
          | sy ->
              raise
                (Catastrophe_
                   ["internal error in parsePanel -- "; symbolstring sy])
      in
      let plabel = currsymb_as_name () in
      let _ = ignore ISWORD in
      let p =
        Panel
          (plabel,
           parseUnsepList (fun s -> member (s, symbs))
             (fun _ -> parseentry ()),
           panelkind)
      in
      match currsymb () with
        SHYID "END" -> scansymb (); p
      | s ->
          showInputError report
            ((["error in panel description: found "; smlsymbolstring s;
               ", expecting one of "] @
                interpolate ", " starters) @
               [" or END"]);
          raise Use_
    and canstartCommand sy = opt2bool (namefromsymbol sy)
    and parseCommand () =
      (* always protected by canstartCommand *)
      try
        parseablenamestring
          (unSOME (namefromsymbol (currsymb ())) before scansymb ()) ^
          (if canstartCommand (currsymb ()) then " " ^ parseCommand ()
           else "")
      with
        UnSOME_ -> raise (Catastrophe_ ["UnSOME_ in parseCommand"])
    and parseHitDef sense =
      let pattern = parseSeq () in
      let action = ignore ISWORD; transTactic (asTactic parseTerm EOF) in
      HitDef (sense, action, pattern)
    and parseInitialise () =
      let name = currsymb_as_name () in
      let value = asTactic parseTerm EOF in InitVar (name, value)
    and parseAutoRule sense =
      AutoRule
        (sense,
         ( <* )
           (transTactic,
            parseList canstartTerm (asTactic parseTerm) commasymbol))
    and parseUse report query =
      match currsymb () with
        STRING s -> File (s, file2paragraphs report query s)
      | s ->
          raise
            (ParseError_ ["USE expects a string; found: "; smlsymbolstring s])
    and parseTheorems report =
      let n = ref 0 in
      let (RuleHeading (name, formals, provisos) as p) =
        parseRuleHeading true AREWORD
      in
      let rec parseThm () = parseUnnamedTheorem report NONE in
      let t =
        Theory
          (p, parseList (fun _ -> true) (fun _ -> parseThm ()) (SHYID "AND"))
      in
      match currsymb () with
        SHYID "END" -> scansymb (); t
      | s ->
          showInputError report
            ["error in THEOREMS description: found "; smlsymbolstring s;
             ", expecting AND or END"];
          raise Use_
    and parseTheorem report =
      if member
           (currsymb (), [BRA "("; SHYID "WHERE"; SHYID "IS"; SHYID "INFER"])
      then
        parseUnnamedTheorem report NONE
      else
        let heading = parseRuleHeading true ISWORD in
        let body = parseSeq () in
        let _ =
          checkvalidruleheading report "theorem" "body of the theorem" heading
            [] body (fun _ -> [])
        in
        Conjecture (heading, body)
    and parseUnnamedTheorem report nopt =
      let params = if itisaparamlist () then parseformals true else [] in
      let provisos = parseProvisos () in
      let _ = ignore ISWORD; ignore (SHYID "INFER") in
      let s = parseSeq () in
      let n =
        match nopt with
          SOME n -> n
        | NONE -> Name (seqstring s)
      in
      let heading = RuleHeading (n, params, provisos) in
      let _ =
        checkvalidruleheading report "theorem" "body of the theorem" heading
          [] s (fun _ -> [])
      in
      Conjecture (heading, s)
    (* PROOF <name> { <params> } { <provisos> } 
                    { FROM <antecedents> } INFER <sequent> 
                    { FORMULAE <numbered formulae> }
                    IS <tactic>
                    { <countermodel> }
       END
     *)
    and parseProof report stage =
      try
        let n = currsymb_as_name () in
        let params = parseformals true in
        let pros = parseProvisos () in
        let (givens, seq) = parseRulebody () in
        let _ =
          consolereport ["reading "; proofstage2word stage; " "; namestring n]
        in
        let fs =
          match currsymb () with
            SHYID "FORMULAE" ->
              scansymb ();
              let fs =
                parseList
                  (function
                     NUM _ -> true
                   | _ -> false)
                  (fun _ -> scansymb (); parseTerm commasymbol) commasymbol
              in
              readintasarg := SOME (Array.arrayoflist fs); SOME fs
          | _ -> NONE
        in
        let tacterm = check ISWORD; asTactic parseTerm EOF in
        let tac = transTactic tacterm before readintasarg := NONE in
        let _ =
          checkvalidruleheading report (proofstage2word stage)
            "body of the conjecture or the body of the proof"
            (RuleHeading (n, params, pros)) givens seq
            (* this argument is slowing down proof reading no end - so I lazified it *)
            (fun _ ->
               match fs with
                 NONE -> termvars tacterm
               | SOME fs ->
                   (* bodyvars from old-style proof *)
                   fold tmerge (( <* ) (termvars, fs))
                     (( <| ) (isUnknown, termvars tacterm)))
        in
        let disproofopt = parsemodel () in
        Proof (n, stage, seq, (givens, params, pros, tac), disproofopt)
      with
        exn -> readintasarg := NONE; raise exn
    and parseRules report axiom =
      let n = ref 0 in
      let (RuleHeading (name, formals, provisos) as p) =
        parseRuleHeading true AREWORD
      in
      let rec nextname () =
        (namestring name ^ "'") ^ makestring !n before inc n
      in
      let rec parseRle () =
        RuleDef (parseUnnamedRule report (SOME (nextname ())) axiom)
      in
      let t =
        RuleGroup
          (p, parseList (fun _ -> true) (fun _ -> parseRle ()) (SHYID "AND"))
      in
      match currsymb () with
        SHYID "END" -> scansymb (); t
      | s ->
          showInputError report
            ["error in RULES description: found "; smlsymbolstring s;
             ", expecting AND or END"];
          raise Use_
    and parseDerived report =
      match currsymb () with
        SHYID "RULE" -> scansymb (); RuleDef (parseRule report false)
      | SHYID "RULES" -> scansymb (); parseRules report false
      | sy ->
          showInputError report
            ["expecting RULE or RULES after DERIVED: found ";
             smlsymbolstring sy];
          raise Use_
    and parseTheory report query =
      let starters =
        ["RULE"; "RULES"; "TACTIC"; "THEOREM"; "THEOREMS"; "THEORY"]
      in
      let parastarters = ( <* ) (SHYID, starters) in
      let t =
        Theory
          (parseRuleHeading true ISWORD,
           parseUnsepList (fun s -> member (s, parastarters))
             (fun _ -> unSOME (parseParagraph report query)))
      in
      match currsymb () with
        SHYID "END" -> scansymb (); t
      | s ->
          showInputError report
            ((["error in THEORY description: found "; smlsymbolstring s;
               ", expecting one of "] @
                interpolate ", " starters) @
               [" or END"]);
          raise Use_
    and parseFontSpec () = FontSpec (currsymb_as_string ())
    and parseForceDefSpec () =
      let t = parseTerm EOF in
      let _ = ignore (SHYID "IFF") in
      let d = parseForceDef () in ForceDef (t, d)
    and parseStructureRule () =
      let rec bang s =
        raise
          (ParseError_ ["rule type expected after STRUCTURERULE, found "; s])
      in
      match currsymb () with
        SHYID s ->
          if member (s, structurerulestrings) then
            begin scansymb (); StructureRule (s, currsymb_as_name ()) end
          else bang s
      | sy -> bang (smlsymbolstring sy)
    and parseStructure s =
      (* [CUT|IDENTITY|...] [RULE] name; ... *)
      ignore (SHYID "RULE");
      ignore ISWORD;
      if member (s, structurerulestrings) then
        StructureRule (s, currsymb_as_name ())
      else raise (Catastrophe_ ["parseStructure "; s])
    and file2paragraphs report query s =
      let s = makerelative s in
      let st =
        pushlex s
          (try open_in s with
             Io m ->
               showInputError report
                 ["Cannot read file: \""; s; "\""; " ("; Io_explain m; ")"];
               raise Use_)
          before startusing s
      in
      let rec cleanup () =
        poplex st; consolereport ["[CLOSING \""; s; "\"]"]; stopusing ()
      in
      consolereport ["[OPENING \""; s; "\"]"];
      (try parseParaList report query with
         ParseError_ m -> showInputError report m; cleanup (); raise Use_
       | Catastrophe_ ss ->
           showInputError report ("Catastrophic input error: " :: ss);
           cleanup ();
           raise Use_
       | exn -> cleanup (); raise exn)
        (* including Use_, at it happens *)
        before cleanup ()
    let rec string2paragraph report query s =
      let rec getpara () =
        match parseParagraph report query with
          SOME p -> p
        | NONE ->
            raise
              (ParseError_
                 ["Error: expecting paragraph beginning; found ";
                  smlsymbolstring (currsymb ())])
      in
      tryparse (fun _ -> getpara ()) s
    let parsename = currsymb_as_name
  end


