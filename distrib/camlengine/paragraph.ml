(* 
    $Id$

    Copyright (C) 2003-4 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the jape proof engine, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*)

open Doubleclick
open Forcedef
open Idclass
open Idclassfuns
open Listfuns
open Menu
open Nametype
open Name
open Panelkind
open Paraparam
open Proofstage
open Proviso
open Sequent
open Sml
open Symbol
open Symboltype
open Termfuns
open Termstring
open Termparse
open Usefile
open UTF

let addbindingdirective = Binding.addbindingdirective
let argstring_of_tactic = Tactic.argstring_of_tactic
let atoi = Miscellaneous.atoi
let autoAdditiveLeft = Miscellaneous.autoAdditiveLeft
let consolereport = Miscellaneous.consolereport
let enQuote = Stringfuns.enQuote
let string_of_list = Listfuns.string_of_list
let bool_of_opt = Optionfuns.bool_of_opt
let readintasarg = Tactic.readintasarg
let stripextrabag = Tactic.stripextrabag
let _The = Optionfuns._The
let transTactic = Tactic.transTactic
let uncurry2 = Miscellaneous.uncurry2

exception ParseError_  = Miscellaneous.ParseError_
exception Catastrophe_ = Miscellaneous.Catastrophe_
exception None_        = Optionfuns.None_

type ruleheading = RuleHeading of (name * paraparam list * proviso list)
type tacticheading = TacticHeading of (name * paraparam list)

type paragraph =
    AutoRule of (bool * tactic list)
  | Conjecture of (ruleheading * seq)
  | File of (string * paragraph list)
  | FontSpec of string
  | ForceDef of (term * forcedef)
  | HitDef of (dclick * tactic * seq)
  | InitVar of (name * term)
  | MacroDef of (tacticheading * term)
  | Menu of (bool * name * menupara list)
  | Panel of (name * panelpara list * panelkind)
  | Proof of (name * proofstage * seq *
                (seq list * paraparam list * proviso list * tactic) *
                (seq * model) option)
  | RuleDef of (ruleheading * seq list * seq * bool)
  | RuleGroup of (ruleheading * paragraph list)
  | StructureRule of (string * name)
  | TacticDef of (tacticheading * tactic)
  | Theory of (ruleheading * paragraph list)
and panelpara = Panelstuff of paneldata | Panelpara of paragraph
and menupara = Menustuff of menucommand | Menupara of paragraph

exception Use_ (* so-called because it is normally raised by a bad USE "file" *)
       
   (************************************************************************************)
   
let rec name_of_stringsymbol sy =
  let rec ok v = Some (Name v) in
  match sy with
    ID       (s, _) -> ok s
  | UNKNOWN  (s, _) -> ok (metachar_as_string ^ s)
  | PREFIX        s -> ok s
  | POSTFIX       s -> ok s
  | LEFTFIX       s -> ok s
  | BRA           s -> ok s
  | SEP           s -> ok s
  | KET           s -> ok s
  | INFIX         s -> ok s
  | INFIXC        s -> ok s
  | NUM           s -> ok s
  | STRING        s -> ok s
  | _               -> None

let rec stringfromsymbol sy =
  match name_of_stringsymbol sy with
    Some (Name s) -> Some s
  | None -> None

let rec name_of_currsymb () =
  let sy = let r = currsymb () in scansymb (); r in
  match name_of_stringsymbol sy with
    Some s -> s
  | None   ->
      raise (ParseError_
               ["Identifier or string expected; found "; debugstring_of_symbol sy])

let currsymb_as_string = string_of_name <.> name_of_currsymb

let _SHYID v = SHYID v
  
(************************************************************************************)

let _ISWORD = SHYID "IS"
let _AREWORD = SHYID "ARE"

let rec parseProvisos () =
  match currsymb () with
    SHYID "WHERE" ->
      scansymb ();
      List.concat
        (parseList (fun _ -> true) (fun _ -> Proviso.parseProvisos ())
           (SHYID "AND"))
  | _ -> []

exception Matchinparseformal of string
(* no longer spurious *)
   
let rec parseRuleHeading mustdecl ignoreable =
  let name = name_of_currsymb () in
  let formals = parseformals mustdecl in
  let provisos = parseProvisos () in
  ignore ignoreable; RuleHeading (name, formals, provisos)

and parseTacticHeading ignoreable =
  (* shouldn't use parseformals, but one step at a time ... *)
  let name = name_of_currsymb () in
  let formals = parseformals false in
  ignore ignoreable; TacticHeading (name, formals)

(* we no longer permit WHERE inside a parameter list *)
and parseformals mustdecl =
  match currsymb () with
    BRA "(" ->
      scansymb ();
      let _r = parseList (fun sy -> sy <> KET ")") (fun _ -> parseformal mustdecl) commasymbol in
      check (KET ")"); _r
  | _ -> []

and parseformal mustdecl =
  try
    match currsymb () with
      SHYID "ABSTRACTION" ->
        begin match scansymb (); currsymb() with
          ID (s, Some c) -> scansymb (); Abstractionparam (vid_of_string s, c)
        | _ -> raise (Matchinparseformal "an identifer after ABSTRACTION")
        end
    | SHYID "OBJECT" ->
        begin match scansymb(); currsymb() with
          ID (s, Some c) -> scansymb (); Objectparam (vid_of_string s, c)
        | _ -> raise (Matchinparseformal "an identifer after OBJECT")
        end
    | ID (s, Some c) -> scansymb (); Ordinaryparam (vid_of_string s, c)
    | ID (s, None) ->
        if mustdecl then
          raise (ParseError_ ["unclassified identifier "; s; " in formal parameter list"])
        else begin scansymb (); Ordinaryparam (vid_of_string s, NoClass) end
    | UNKNOWN (s, Some c) -> scansymb (); Unknownparam (vid_of_string s, c)
    | UNKNOWN (s, None) ->
        if mustdecl then
          raise
            (ParseError_
               ["unclassified unknown "; metachar_as_string; s; " in formal parameter list"])
        else begin scansymb (); Unknownparam (vid_of_string s, NoClass) end
    | sy ->
        raise (Matchinparseformal "an identifier in formal parameter list")
  with
    Matchinparseformal s ->
      raise
        (ParseError_
           ["Expecting "; s; ", found "; debugstring_of_symbol (currsymb ())])

let rec itisaparamlist () =
  match currsymb () with
    BRA "(" ->
      let rec res ps (v : bool) = List.iter (fun s -> putbacksymb s) ps; v in
      let rec param ps =
        match currsymb () with
          ID _ -> sep (let r = currsymb () :: ps in scansymb (); r)
        | UNKNOWN _ -> sep (let r = currsymb () :: ps in scansymb (); r)
        | SHYID "ABSTRACTION" -> res ps true
        | SHYID "OBJECT" -> res ps true
        | _ -> res ps false
      and sep ps =
        if currsymb () = commasymbol then
          param ( scansymb (); commasymbol :: ps)
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
      (ParseError_ [debugstring_of_symbol sy; " shouldn't appear "; message])
  in
  match sy with
    ID (s, _) -> scansymb (); s
  | BRA   "(" -> bang ()
  | BRA   bra -> scansymb (); bra
  | SEP     s -> scansymb (); s
  | KET   ")" -> bang ()
  | KET     s -> scansymb (); s
  | INFIX   s -> (* |    INFIX(",",_,s)  => bang() *)
                 scansymb (); s
  | INFIXC  s -> scansymb (); s
  | LEFTFIX s -> scansymb (); s
  | POSTFIX s -> scansymb (); s
  | MIDFIX  s -> scansymb (); s
  | PREFIX  s -> scansymb (); s
  | STRING  s -> scansymb (); s
  | sy        -> bang ()

let rec parseFixitySYMB () = parsenovelsymb "in a Fixity directive"

let rec acceptpreviousnovelsymb ok message =
  let newsy = currnovelsymb () in
  if ok newsy then begin scansymb (); string_of_symbol newsy end
  else parsenovelsymb message

let rec parseNUM () =
  match currsymb () with
    NUM n -> scansymb (); atoi n
  | s ->
      raise
        (ParseError_ ["Expecting a number, found "; debugstring_of_symbol s])

let rec parseClassID () =
  match currnovelsymb () with
    ID (s, _) -> scansymb (); s
  | s         -> raise (ParseError_ ["Expecting an identifier, found "; debugstring_of_symbol s])

let rec processKEYBOARD report query =
  let _ = ignore _ISWORD in
  let symbols =
    parseUnsepList canstartnovelsymb
      (fun _ -> let r = currnovelsymb () in scansymb (); r)
  in
  set_oplist ((string_of_symbol <* symbols))

let rec processClassDecl report query class__ =
  let rec doit s =
    match declareIdClass class__ s with
      Some (s', class') ->(* showInputError report ["warning: ", unparseidclass class, " ", s,
                                " may be confused with earlier CLASS ", 
                                unparseidclass class', " ", s', " directive"]
       *)
       ()
    | None -> ()
  in
  List.iter doit
    (parseUnsepList
       (function ID _ -> true | _ -> false)
       (fun _ -> parseClassID ()))

let rec processCLASS report query =
  let class__ = parseidclass "after CLASS" in
  let rec doit s = List.iter (warn s) (declareIdPrefix class__ s)
  and warn s (s', class') = 
        showInputError report ["warning: CLASS "; unparseidclass class__; " "; s; 
                            " may be confused with earlier ";
                            unparseidclass class'; " "; s'; " directive"]
  in
  List.iter doit
    (parseUnsepList (function ID _ -> true | _ -> false) (fun _ -> parseClassID ()))

let rec processInfix _SYMBCON =
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
  List.iter (fun s -> enter s (Some l) (Some a) (_SYMBCON s)) symbs

let rec processUnifix con =
  let n = parseNUM () in
  let symbs =
    parseUnsepList canstartnovelsymb (fun _ -> parseFixitySYMB ())
  in
  List.iter (fun s -> enter s (Some n) None (con s)) symbs

let processPushSyntax () =
  match currsymb () with
    STRING s ->  
      scansymb ();
      Symbol.pushSyntax s; Termparse.pushSyntax s; Sequent.pushSyntax s
  | s        ->
      raise
        (ParseError_ ["PUSHSYNTAX expects a string; found: "; debugstring_of_symbol s])

let processPopSyntax () =
  Symbol.popSyntax(); Termparse.popSyntax(); Sequent.popSyntax()
  
let popAllSyntaxes () =
  Symbol.popAllSyntaxes(); Termparse.popAllSyntaxes(); Sequent.popAllSyntaxes()
  
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
    enter bra None None SUBSTBRA;
    enter sep None None SUBSTSEP;
    enter ket None None SUBSTKET;
    match idclass t1, idclass t2 with
      VariableClass, FormulaClass -> substsense := true
    | FormulaClass, VariableClass -> substsense := false
    | _ ->
        raise
          (ParseError_
             ["in SUBSTFIX definition, one of "; string_of_term t1; " and ";
              string_of_term t2;
              " must be CLASS VARIABLE, and the other CLASS FORMULA"])

let rec parseUntilSHYID parser__ =
  if match currsymb () with
       EOF -> true
     | SHYID _ -> true
     | _ -> false
  then
    []
  else 
    let v = parser__ () in v :: parseUntilSHYID parser__

let rec processLeftMidfix con =
  let n = parseNUM () in
  let bra = parseFixitySYMB () in
  let symbs = parseUntilSHYID parseFixitySYMB in
  List.iter (fun sep -> enter sep None None (SEP sep)) symbs;
  enter bra (Some n) None (con bra);
  declareLeftMidfix ((lookup <* bra :: symbs))

let rec processOutRightfix name econ =
  let bra = parseFixitySYMB () in
  let ms = parseUntilSHYID parseFixitySYMB in
  match List.rev ms with
    [] -> raise (ParseError_ [name; " "; bra; " has no closing bracket"])
  | ket :: ms' ->
      List.iter (fun m -> enter m None None (SEP m)) ms';
      econ bra;
      enter ket None None (KET ket);
      declareOutRightfix ((lookup <* bra :: List.rev ms')) (lookup ket)

let rec processRightfix () =
  let n = parseNUM () in
  processOutRightfix "RIGHTFIX" (fun bra -> enter bra (Some n) None (RIGHTFIX bra))

let rec processOutfix () = processOutRightfix "OUTFIX" (fun v->enter v None None (BRA v))

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
  let varsinbody = (ismetav <| termvars body) in
  if not
       ((subset (vars, varsinbody) && subset (scope, varsinbody)) &&
        null (_INTER scope vars))
  then
    let names xs =
       (if List.length xs=1 then "is " else "are ") ^
       sentencestring_of_list (enQuote <.> string_of_term) ", " " and "  xs
    in
    let numstring s t xs =
      s ^ (if List.length xs>1 then "s" else "") ^ t ^ names xs
    in
    raise
      (ParseError_
         ["Component names badly chosen in BIND directive. ";
          numstring "Bound variable" " " vars;
          numstring "; scope" " " scope;
          numstring "; name" " in formula " varsinbody;
          "; formula is "; string_of_term body])
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
          KET ")" -> scansymb (); b, None
        | s ->
            if s = commasymbol then
              begin
                scansymb ();
                (let r = b, Some (parsealertspec ()) in
                  (match currsymb () with
                     KET ")" -> scansymb ()
                   | s ->
                       raise
                         (ParseError_
                            ["bracket expected after alert spec in PATCHALERT, found ";
                             string_of_symbol (currsymb ())])); r)
              end
            else
              raise
                (ParseError_
                   ["comma or bracket expected after button label in PATCHALERT, found ";
                    string_of_symbol (currsymb ())])
        end
    | _ ->
        raise
          (ParseError_
             ["button label expected in PATCHALERT, found ";
              string_of_symbol (currsymb ())])
  
  and parsealertspec _ =
    match currsymb () with
      STRING m ->
        scansymb ();
        begin match parsebuttonspecs () with
          [] ->
            raise
              (ParseError_ ["alert in PATCHALERT has no button specs"])
        | bs -> Alert.Alert (m, bs, parsedefaultindex ())
        end
    | s ->
        let rec bang () =
          raise
            (ParseError_
               ["string, HowToSelect, HowToFormulaSelect or HowToDrag \
                                      expected in alert spec, found ";
                string_of_symbol s])
        in
        match stringfromsymbol s with
          Some "HowToFormulaSelect"     -> scansymb (); Alert.HowToFormulaSelect
        | Some "HowToTextSelect"        -> scansymb (); Alert.HowToTextSelect
        | Some "HowToDragFormulae"      -> scansymb (); Alert.HowToDragFormulae
        | Some "HowToDragDisproofStuff" -> scansymb (); Alert.HowToDragDisproofStuff
        | _                             -> bang ()
  
  and parsebuttonspecs () =
    parseUnsepList
      (function
         BRA "(" -> true
       | _ -> false)
      parsebuttonspec
  
  and parsedefaultindex () =
    match currsymb () with
      NUM s -> scansymb (); atoi s
    | _ -> 0
  
  in
  match currsymb () with
    STRING h ->
      scansymb ();
      begin match currsymb () with
        BRA "(" ->
          (* no replacement message, but there are buttons *)
          Alert.patchalert
            (h,
             Alert.Alert ("", parsebuttonspecs (), parsedefaultindex ()))
      | _ -> Alert.patchalert (h, parsealertspec ())
      end
  | _ ->
      raise
        (ParseError_
           ["string expected after PATCHALERT, found ";
            string_of_symbol (currsymb ())])

let rec parseAntecedents starter =
  let r = parseList starter (fun _ -> parseSeq ()) (SHYID "AND") in 
  check (SHYID "INFER"); r

let rec parseButtonDefault f =
  match currsymb () with
    SHYID "INITIALLY" -> scansymb (); Some (f ())
  | _ -> None

let rec parseCheckBox report query con =
  let var = name_of_currsymb () in
  let label = name_of_currsymb () in
  let defval = parseButtonDefault currsymb_as_string in
  con (var, label, ("true", "false"), defval)

let rec parseRadioButton report query con =
  let var = name_of_currsymb () in
  let rec parseentry sep =
    let label = name_of_currsymb () in
    let value = ignore _ISWORD; currsymb_as_string () in label, value
  in
  ignore _ISWORD;
  match parseList (fun _ -> true) parseentry (SHYID "AND") with
    [] ->
      showInputError report ["null value/label list in RADIOBUTTON directive"];
      raise Use_
  | entries ->
      let defval = parseButtonDefault currsymb_as_string in
      match currsymb () with
        SHYID "END" -> scansymb (); con (var, entries, defval)
      | s           ->
          showInputError report
            ["found symbol "; debugstring_of_symbol s;
             " in RADIOBUTTON directive - expecting END"];
          raise Use_

let structurerulestrings =
  ["CUT"; "IDENTITY"; "LEFTWEAKEN"; "REFLEXIVE"; "RIGHTWEAKEN";
   "TRANSITIVE"; "WEAKEN"]

(* at the moment all we do is to check that names in params and provisos are from the rule *)
let rec checkvalidruleheading report objkind wherekind =
  fun (RuleHeading (name, params, provisos)) antes conseq fbvs ->
    let bodyvars =
      nj_fold (uncurry2 tmerge) (seqvars termvars tmerge <* (conseq :: antes)) []
    in
    let paramvars = sort earliervar ((paramvar <* params)) in
    let provars = nj_fold (uncurry2 tmerge) (provisovars termvars tmerge <* provisos) []
    in
    (* don't evaluate fbvs unless there is an apparent error, and don't do it twice *)
    let obvs : term list option ref = ref None in
    let rec check vs errorf =
      match !obvs with
        None ->
          (match sorteddiff earliervar vs bodyvars with
             [] -> () (* nothing wrong - the usual case *)
           | _  -> obvs := Some (tmerge bodyvars (fbvs ())); 
                   check vs errorf)
      | Some bvs ->
          (match sorteddiff earliervar vs bvs with
             []     -> () (* definitely nothing wrong *)
           | extras -> errorf extras)
    in
    let rec add_an_s s b = if b then s ^ "s" else s in
    let rec error place has vs =
      let place = place() in
      let has = has() in
      (match split (fun v -> member (v, bodyvars)) vs with
         [], _ ->
           showInputError report
             [place; " "; objkind; " "; string_of_name name; " "; has; " ";
              add_an_s "variable" (List.length vs > 1); " ";
              sentencestring_of_list string_of_term ", " " and " vs;
              " which ";
              (if List.length vs =1 then "isn't" else "aren't"); " in the "; wherekind; "."]
       | _, [] ->
           showInputError report
             [place; " "; objkind; " "; string_of_name name; " "; has;
              " duplicate "; add_an_s "variable" (List.length vs > 1); " ";
              sentencestring_of_list string_of_term ", " " and " vs; "."]
       | dups, rogues ->
           showInputError report
             [place; " "; objkind; " "; string_of_name name; " "; has;
              " duplicate "; add_an_s "variable" (List.length dups > 1); " ";
              sentencestring_of_list string_of_term ", " " and " dups; ", and also ";
              add_an_s "variable" (List.length rogues > 1); " ";
              sentencestring_of_list string_of_term ", " " and " rogues;
              " which aren't in the "; wherekind; "."]);
      raise Use_
    in
    check paramvars (error (fun _ -> implode ["the parameter list ("; 
                                              string_of_list string_of_paraparam "," params; 
                                              ") of"]) 
                           (fun _ -> "includes"));
    check provars (error (fun _ -> implode [add_an_s "the proviso" (List.length provisos > 1); " "; 
                                            string_of_list string_of_proviso " AND " provisos;
                                            " of"])
                         (fun _ -> add_an_s "contain" (List.length provisos = 1)))

let rec parseParagraph report query =
  let sy = currsymb () in
  let rec more () = parseParagraph report query in
  match sy with
    SHYID "AUTOMATCH"       -> scansymb (); Some (parseAutoRule true)
  | SHYID "AUTOUNIFY"       -> scansymb (); Some (parseAutoRule false)
  | SHYID "BIND"            -> scansymb (); processBind (); more ()
  | SHYID "CONCHIT"         -> scansymb (); Some (parseHitDef DClickConc)
  | SHYID "CONSTANT"        -> scansymb (); processClassDecl report query ConstantClass; more ()
  | SHYID "CLASS"           -> scansymb (); processCLASS report query; more ()
  | SHYID "CONJECTUREPANEL" -> scansymb (); Some (parseConjecturePanel report query)
  | SHYID "CURRENTPROOF"    -> scansymb (); Some (parseProof report InProgress)
  | SHYID "CUT"             -> scansymb (); Some (parseStructure "CUT")
  | SHYID "DERIVED"         -> scansymb (); Some (parseDerived report)
  | SHYID "DISPROOF"        -> scansymb (); Some (parseProof report Complete) (* some legacy files will have this word *)
  | SHYID "FONTS"           -> scansymb (); Some (parseFontSpec ())
  | SHYID "FORCEDEF"        -> scansymb (); Some (parseForceDefSpec ())
  | SHYID "FORMULA"         -> raise (ParseError_ ["FORMULA without CLASS is meaningless"])
  | SHYID "GIVENPANEL"      -> scansymb (); Some (parseGivenPanel report query)
  | SHYID "HYPHIT"          -> scansymb (); Some (parseHitDef DClickHyp)
  | SHYID "IDENTITY"        -> scansymb (); Some (parseStructure "IDENTITY")
  | SHYID "INFIX"           -> scansymb (); processInfix (fun v->INFIX v); more ()
  | SHYID "INFIXC"          -> scansymb (); processInfix (fun v->INFIXC v); more ()
  | SHYID "JUXTFIX"         -> scansymb (); appfix := parseNUM (); more ()
  | SHYID "KEYBOARD"        -> scansymb (); processKEYBOARD report query; more ()
  | SHYID "INITIALISE"      -> scansymb (); Some (parseInitialise ())
  | SHYID "LEFTFIX"         -> scansymb (); processLeftMidfix (fun v->LEFTFIX v); more ()
  | SHYID "LEFTWEAKEN"      -> scansymb (); Some (parseStructure "LEFTWEAKEN")
  | SHYID "MACRO"           -> let h = scansymb (); parseTacticHeading _ISWORD in
                               Some (MacroDef (h, asTactic parseTerm EOF))
  | SHYID "MENU"            -> scansymb (); Some (parseMenu report query true)
  | SHYID "MIDFIX"          -> scansymb (); processLeftMidfix (fun v->MIDFIX v); more ()
  | SHYID "NUMBER"          -> scansymb (); processClassDecl report query NumberClass; more ()
  | SHYID "OUTFIX"          -> scansymb (); processOutfix (); more ()
  | SHYID "PATCHALERT"      -> scansymb (); processPatchAlert (); more ()
  | SHYID "POPSYNTAX"       -> scansymb (); processPopSyntax (); more ()
  | SHYID "POSTFIX"         -> scansymb (); processUnifix (fun v->POSTFIX v); more ()
  | SHYID "PREFIX"          -> scansymb (); processUnifix (fun v->PREFIX v); more ()
  | SHYID "PROOF"           -> scansymb (); Some (parseProof report Complete)
  | SHYID "PUSHSYNTAX"      -> scansymb (); processPushSyntax(); more ()
  | SHYID "REFLEXIVE"       -> scansymb (); Some (parseStructure "REFLEXIVE")
  | SHYID "RIGHTFIX"        -> scansymb (); processRightfix (); more ()
  | SHYID "RIGHTWEAKEN"     -> scansymb (); Some (parseStructure "RIGHTWEAKEN")
  | SHYID "RULE"            -> scansymb (); Some (RuleDef (parseRule report true))
  | SHYID "RULES"           -> scansymb (); Some (parseRules report true)
  | SHYID "STRING"          -> scansymb (); processClassDecl report query StringClass; more ()
  | SHYID "SEMANTICTURNSTILE" -> scansymb (); processSemanticTurnstileSpec (); more ()
  | SHYID "SEQUENT"         -> scansymb (); processSequentSpec (); more ()
  | SHYID "STRUCTURERULE"   -> scansymb (); Some (parseStructureRule ())
  | SHYID "SUBSTFIX"        -> scansymb (); processSubstfix report query; more ()
  | SHYID "TACTIC"          -> let h = scansymb (); parseTacticHeading _ISWORD in
                               Some (TacticDef (h, transTactic (asTactic parseTerm EOF)))
  | SHYID "TACTICPANEL"     -> scansymb (); Some (parseTacticPanel report query)
  | SHYID "THEOREM"         -> scansymb (); Some (parseTheorem report)
  | SHYID "THEOREMS"        -> scansymb (); Some (parseTheorems report)
  | SHYID "THEORY"          -> scansymb (); Some (parseTheory report query)
  | SHYID "TRANSITIVE"      -> scansymb (); Some (parseStructure "TRANSITIVE")
  | SHYID "UMENU"           -> scansymb (); Some (parseMenu report query false)
  | SHYID "USE"             -> scansymb (); Some (let r = parseUse report query in scansymb (); r)
  | SHYID "VARIABLE"        -> scansymb (); processClassDecl report query VariableClass; more ()
  | SHYID "WEAKEN"          -> scansymb (); Some (parseStructure "WEAKEN")
  | _                       -> None

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
  ignore _ISWORD; describeSeqs (parseList (fun _ -> true) f (SHYID "AND"))

and processSemanticTurnstileSpec () =
  let syn =
    match currsymb () with
      STILE s -> scansymb (); s
    | sy ->
        raise
          (ParseError_
             ["turnstile expected after SEMANTICTURNSTILE: found ";
              string_of_symbol sy])
  in
  let _ = ignore _ISWORD in
  let sem =
    acceptpreviousnovelsymb
      (function
         STILE s -> true
       | _ -> false)
      "as semantic turnstile"
  in
  Sequent.setsemanticturnstile syn sem

and parseRule report axiom =
  if member
       (currsymb (),
        [BRA "("; SHYID "WHERE"; SHYID "FROM"; SHYID "INFER"; SHYID "IS"])
  then
    parseUnnamedRule report None axiom
  else
    let heading = parseRuleHeading true _ISWORD in
    let (antes, conseq) = parseRulebody () in
    let _ =
      checkvalidruleheading report "rule" "body of the rule" heading antes conseq (fun _ -> [])
    in
    heading, antes, conseq, axiom

and parseUnnamedRule report nopt axiom =
  let params = if itisaparamlist () then parseformals true else [] in
  let provisos = parseProvisos () in
  let _ = ignore _ISWORD in
  let (antes, conseq) = parseRulebody () in
  let name = Name (match nopt with
                     Some n -> n
                   | None   -> match antes with
                                 [] -> string_of_thmbody " " [] conseq
                               | _  -> string_of_rulebody " " [] antes conseq)
  in
  let heading = RuleHeading (name, params, provisos) in
  let _ =
    checkvalidruleheading report "rule" "body of the rule" heading antes conseq (fun _ -> [])
  in
  (heading, antes, conseq, axiom)

and parseRulebody () =
  match currsymb () with
    SHYID "FROM" ->
      scansymb (); 
      let antes = parseAntecedents (fun _ -> true) in
      (antes, parseSeq ())
  | _ -> ignore (SHYID "INFER"); ([], parseSeq ())

and parseParaList report query =
  match parseParagraph report query with
    Some p -> p :: parseParaList report query
  | None ->
      match currsymb () with
        SHYID "END" -> scansymb (); []
      | EOF -> []
      | sy ->
          raise
            (ParseError_
               ["Error: expecting symbol beginning paragraph; found ";
                debugstring_of_symbol sy])

and parseMenu report query mproof =
  let parastarters =
    ["RULE"; "RULES"; "DERIVED"; "TACTIC"; "THEOREM"; "THEOREMS";
     "THEORY"; "PROOF"; "CURRENTPROOF"]
  in
  let starters =
    ["BEFOREENTRY"; "RENAMEENTRY"; "ENTRY"; "BUTTON"; "RADIOBUTTON"; "CHECKBOX"; "SEPARATOR"] @
      parastarters
  in
  let parasymbs = (_SHYID <* parastarters) in
  let symbs = (_SHYID <* starters) in
  let rec mkentry canstart getitem prefix toitem =
    let itemname = name_of_currsymb () in
    let menukey =
      match currsymb () with
        SHYID "MENUKEY" -> scansymb (); Some (currsymb_as_string ())
      | _ -> None
    in
    let item =
      prefix ^
        (match currsymb () with
           SHYID "IS" -> scansymb (); getitem ()
         | _ ->
             if canstart (currsymb ()) then getitem ()
             else toitem itemname)
    in
    Mentry (itemname, menukey, item)
  in
  let rec parseentry () =
    if member (currsymb (), parasymbs) 
    then Menupara (_The (parseParagraph report query))
    else Menustuff (parsemenucommand())
  and parsemenucommand () = 
      match currsymb () with
        SHYID "BEFOREENTRY" -> 
          scansymb(); 
          let beforename = name_of_currsymb() in
          MCbefore (beforename, parsesimpleentry())
      | SHYID "RENAMEENTRY" -> 
          scansymb(); 
          let oldname = name_of_currsymb() in
          MCrename (oldname, name_of_currsymb())
      | _ -> MCdata (parsesimpleentry())
  and parsesimpleentry () = 
      match currsymb () with
        SHYID "ENTRY" ->
          scansymb ();
          mkentry canstartTerm (fun () -> parseTacticEntry report)
            "apply " parseablestring_of_name
      | SHYID "BUTTON" ->
          scansymb (); mkentry canstartCommand parseCommand "" string_of_name
      | SHYID "RADIOBUTTON" ->
          scansymb ();
          parseRadioButton report query (fun v->Mradiobutton v)
      | SHYID "CHECKBOX" ->
          scansymb (); parseCheckBox report query (fun v->Mcheckbox v)
      | SHYID "SEPARATOR" -> scansymb (); Mseparator
      | s ->
          showInputError report
            ["internal error in parseMenu: found "; debugstring_of_symbol s];
          raise Use_
  in
  let mlabel = name_of_currsymb () in
  let _ = ignore _ISWORD in
  let m =
    Menu (mproof, mlabel,
          parseUnsepList (fun s -> member (s, symbs)) (fun _ -> parseentry ()))
  in
  match currsymb () with
    SHYID "END" -> scansymb (); m
  | s ->
      showInputError report
        ((["found symbol "; debugstring_of_symbol s; " in menu description: ";
           "expecting one of "] @
            interpolate ", " starters) @
           [" or END"]);
      raise Use_

and parseConjectureEntry report =
  try match name_of_stringsymbol (currsymb()) with
        Some _ -> argstring_of_tactic (transTactic (asTactic parseTerm EOF))
      | None   ->
          raise (ParseError_ ["conjecture name expected in ENTRY; found ";
                              string_of_symbol (currsymb())])
  with ParseError_ ss -> showInputError report ss; raise Use_
  
and parseConjecturePanel report query =
  parsePanel report query ConjecturePanelkind parseConjectureEntry
    ["ENTRY"; "BUTTON" (*; "RADIOBUTTON"; "CHECKBOX"*)]
    ["THEOREM"; "THEOREMS"; "DERIVED"; "PROOF"; "CURRENTPROOF"]

and parseTacticEntry report =
  try argstring_of_tactic (transTactic (asTactic parseTerm EOF)) with
    ParseError_ ss -> showInputError report ss; raise Use_

and parseTacticPanel report query =
  parsePanel report query TacticPanelkind parseTacticEntry
    ["ENTRY"; "BUTTON" (*; "RADIOBUTTON"; "CHECKBOX"*)]
    ["THEORY"; "RULE"; "RULES"; "DERIVED"; "TACTIC"; "THEOREM";
     "THEOREMS"; "PROOF"; "CURRENTPROOF"]

and parseGivenPanel report query =
  parsePanel report query GivenPanelkind parseTacticEntry
    ["BUTTON" (*; "RADIOBUTTON"; "CHECKBOX"*)] []

and parsePanel report query panelkind parseEntry entrystarters parastarters =
  let starters = entrystarters @ parastarters in
  let entrysymbs = (_SHYID <* entrystarters) in
  let parasymbs = (_SHYID <* parastarters) in
  let symbs = (_SHYID <* starters) in
  let rec parseentry () =
    if member (currsymb (), parasymbs) then
      Panelpara (_The (parseParagraph report query))
    else
      match currsymb () with
        SHYID "ENTRY" ->
          let itemname = scansymb (); name_of_currsymb () in
          let item =
            match currsymb () with
              SHYID "IS" -> scansymb (); parseEntry report
            | _ ->
                if canstartTerm (currsymb ()) then parseEntry report
                else parseablestring_of_name itemname
          in
          Panelstuff (Pentry (itemname, item))
      | SHYID "BUTTON" ->
          let itemname =
            scansymb (); 
            let r = name_of_currsymb () in ignore _ISWORD; r
          in
          let rec getcmd () =
            match currsymb () with
              SHYID "LABEL"   -> scansymb (); LabelInsert :: getcmd ()
            | SHYID "COMMAND" -> scansymb (); CommandInsert :: getcmd ()
            | sy              -> if canstartCommand sy then
                                   let v = StringInsert (parseCommand ()) in v :: getcmd ()
                                 else []
          in
          let itemcmd =
            match getcmd () with
              [] -> raise
                      (ParseError_ ["command expected after BUTTON, found "; string_of_symbol (currsymb ())])
            | cs -> cs
          in
          Panelstuff (Pbutton (itemname, itemcmd))
      (* | SHYID "RADIOBUTTON" ->
             scansymb ();
             Panelstuff (parseRadioButton report query (fun v->Pradiobutton v))
         | SHYID "CHECKBOX" ->
             scansymb (); Panelstuff (parseCheckBox report query (fun v->Pcheckbox v))
       *)
      | sy -> raise (Catastrophe_ ["internal error in parsePanel -- "; string_of_symbol sy])
  in
  let plabel = name_of_currsymb () in
  let _ = ignore _ISWORD in
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
        ((["error in panel description: found "; debugstring_of_symbol s;
           ", expecting one of "] @
            interpolate ", " starters) @
           [" or END"]);
      raise Use_

and canstartCommand sy = bool_of_opt (name_of_stringsymbol sy)

and parseCommand () =
  (* always protected by canstartCommand *)
  try
    let f = match currsymb() with
              STRING s -> enQuote s
            | sy       -> string_of_symbol sy 
    in
    scansymb();
    if canstartCommand (currsymb ()) then f ^ " " ^ parseCommand ()
    else f
  with
    None_ ->  raise (Catastrophe_ ["name_of_stringsymbol failed in parseCommand, looking at "; 
                                   string_of_symbol(currsymb())])

and parseHitDef sense =
  let pattern = parseSeq () in
  let action = ignore _ISWORD; transTactic (asTactic parseTerm EOF) in
  HitDef (sense, action, pattern)

and parseInitialise () =
  let name = name_of_currsymb () in
  let value = asTactic parseTerm EOF in InitVar (name, value)

and parseAutoRule sense =
  AutoRule
    (sense,
       (transTactic <* parseUnsepList canstartTerm (asTactic parseTerm)))

and parseUse report query =
  match currsymb () with
    STRING s -> File (s, paragraphs_of_file report query s)
  | s ->
      raise
        (ParseError_ ["USE expects a string; found: "; debugstring_of_symbol s])

and parseTheorems report =
  let n = ref 0 in
  let (RuleHeading (name, formals, provisos) as p) =
    parseRuleHeading true _AREWORD
  in
  let rec parseThm () = parseUnnamedTheorem report None in
  let t =
    Theory
      (p, parseList (fun _ -> true) (fun _ -> parseThm ()) (SHYID "AND"))
  in
  match currsymb () with
    SHYID "END" -> scansymb (); t
  | s ->
      showInputError report
        ["error in THEOREMS description: found "; debugstring_of_symbol s;
         ", expecting AND or END"];
      raise Use_

and parseTheorem report =
  if member
       (currsymb (), [BRA "("; SHYID "WHERE"; SHYID "IS"; SHYID "INFER"])
  then
    parseUnnamedTheorem report None
  else
    let heading = parseRuleHeading true _ISWORD in
    let body = parseSeq () in
    let _ =
      checkvalidruleheading report "theorem" "body of the theorem" heading [] body (fun _ -> [])
    in
    Conjecture (heading, body)

and parseUnnamedTheorem report nopt =
  let params = if itisaparamlist () then parseformals true else [] in
  let provisos = parseProvisos () in
  let _ = ignore _ISWORD; ignore (SHYID "INFER") in
  let s = parseSeq () in
  let n =
    match nopt with
      Some n -> n
    | None   -> Name (string_of_seq s)
  in
  let heading = RuleHeading (n, params, provisos) in
  let _ =
    checkvalidruleheading report "theorem" "body of the theorem" heading [] s (fun _ -> [])
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
    let n = name_of_currsymb () in
    let params = parseformals true in
    let pros = parseProvisos () in
    let (givens, seq) = parseRulebody () in
    consolereport ["reading "; word_of_proofstage stage; " "; string_of_name n];
    let fs =
      match currsymb () with
        SHYID "FORMULAE" ->
          scansymb ();
          let fs =
            parseList
              (function NUM _ -> true | _ -> false)
              (fun _ -> scansymb (); parseTerm commasymbol) commasymbol
          in
          readintasarg := Some (Array.of_list fs); Some fs
      | _ -> None
    in
    (* there's a missing word SYNTAX somewhere about here *)
    let tacterm = check _ISWORD; asTactic parseTerm EOF in
    (* this is part of half of a TEMPORARY hack designed to make replay of proofs 
       linear.  See prooftree.sml, tactic.sml for the rest of the sandwich. 
     *)
    let _ = if !autoAdditiveLeft then stripextrabag:= true else () in
    let tac = let r = transTactic tacterm in readintasarg := None; r in
    let _ =
      checkvalidruleheading report (word_of_proofstage stage)
        "body of the conjecture or the body of the proof"
        (RuleHeading (n, params, pros)) givens seq
        (* this argument is slowing down proof reading no end - so I lazified it *)
        (fun _ ->
           match fs with
             None    -> termvars tacterm
           | Some fs ->
               (* bodyvars from old-style proof *)
               nj_fold (uncurry2 tmerge) (termvars <* fs) (isUnknown <| termvars tacterm))
    in
    let disproofopt = parsemodel () in
    Proof (n, stage, seq, (givens, params, pros, tac), disproofopt)
  with
    exn -> readintasarg := None; raise exn

and parseRules report axiom =
  let n = ref 0 in
  let (RuleHeading (name, formals, provisos) as p) =
    parseRuleHeading true _AREWORD
  in
  let rec nextname () =
    let r = (string_of_name name ^ "'") ^ string_of_int !n in incr n; r
  in
  let rec parseRle () =
    RuleDef (parseUnnamedRule report (Some (nextname ())) axiom)
  in
  let t =
    RuleGroup
      (p, parseList (fun _ -> true) (fun _ -> parseRle ()) (SHYID "AND"))
  in
  match currsymb () with
    SHYID "END" -> scansymb (); t
  | s ->
      showInputError report
        ["error in RULES description: found "; string_of_symbol s; ", expecting AND or END"];
      raise Use_

and parseDerived report =
  match currsymb () with
    SHYID "RULE"  -> scansymb (); RuleDef (parseRule report false)
  | SHYID "RULES" -> scansymb (); parseRules report false
  | sy            ->
      showInputError report
        ["expecting RULE or RULES after DERIVED: found "; string_of_symbol sy];
      raise Use_

and parseTheory report query =
  let starters =
    ["RULE"; "RULES"; "TACTIC"; "THEOREM"; "THEOREMS"; "THEORY"]
  in
  let parastarters = (_SHYID <* starters) in
  let heading = parseRuleHeading true _ISWORD in
  let body = parseUnsepList (fun s -> member (s, parastarters))
                            (fun _ -> _The (parseParagraph report query))
  in
  match currsymb () with
    SHYID "END" -> scansymb (); Theory (heading,body)
  | s           ->
      showInputError report
        ["error in THEORY description: found "; string_of_symbol s;
         ", expecting one of "; string_of_list string_of_symbol ", " parastarters; " or END"];
      raise Use_

and parseFontSpec () = FontSpec (currsymb_as_string ())

and parseForceDefSpec () =
  let t = parseTerm EOF in
  let _ = ignore _ISWORD in
  let d = parseForceDef () in ForceDef (t, d)

and parseStructureRule () =
  let rec bang s =
    raise
      (ParseError_ ["rule type expected after STRUCTURERULE, found "; s])
  in
  match currsymb () with
    SHYID s ->
      if member (s, structurerulestrings) then
        begin scansymb (); StructureRule (s, name_of_currsymb ()) end
      else bang s
  | sy -> bang (string_of_symbol sy)

and parseStructure s =
  (* [CUT|IDENTITY|...] [RULE] name; ... *)
  ignore (SHYID "RULE");
  ignore _ISWORD;
  if member (s, structurerulestrings) then
    StructureRule (s, name_of_currsymb ())
  else raise (Catastrophe_ ["parseStructure "; s])

and paragraphs_of_file report query s =
  let s = makerelative s in
  let ic = 
    try Usefile.open_input_file s 
    with Sys_error e ->
           showInputError report
             ["Cannot read file: \""; Usefile.normalizePath s; "\""; " ("; e; ")"];
           raise Use_
  in
  let st = pushlex s (of_utfchannel ic) in
  let _ = startusing s in
  let rec cleanup () =
    poplex st; consolereport ["[CLOSING \""; Usefile.normalizePath s; "\"]"]; 
    stopusing (); close_in ic
  in
  let error_cleanup () = 
    popAllSyntaxes(); cleanup()
  in
  consolereport ["[OPENING \""; Usefile.normalizePath s; "\"]"];
  let r = 
    (try parseParaList report query with
       ParseError_ m -> showInputError report m; error_cleanup (); raise Use_
     | Catastrophe_ ss ->
         showInputError report ("Catastrophic input error: " :: ss);
         error_cleanup (); raise Use_
     | MalformedUTF_ ss ->
        showInputError report ["Malformed UTF-8 input: "; string_of_list (fun s -> s) "" ss;
                               ".\n\n\
                                Jape now works only with unicode (UTF-8/16/32) files.\n\n\
                                (Are you perhaps reading a old unconverted non-Unicode file? If so, \
                                try using the file converter that comes with Jape.)\n\n\
                                (Or, if this is the first time you've run the new version of \
                                Jape, check that it isn't picking up an unconverted file by \
                                default. The file dialogue starts in the last directory \
                                where you opened a file: it may be prompting you to pick \
                                up the wrong file.)"];
        error_cleanup (); raise Use_
     | exn -> error_cleanup (); raise exn)
    (* including Use_, at it happens *)
  in cleanup (); r
  
(* this is where rules, theorems, tactics and macros get parsed, so this is the right place
   to generate their parseable representation. Odd, innit?
   
   linesep is normally a space, but can be a newline as well (see prooftree.ml).
 *)

and catelim_string_of_rulebody linesep provisos antes conseq ss =
  let body = "INFER " :: catelim_separatedstring_of_seq 
                                   (if linesep="\n" then "\n     " else linesep)
                                   conseq ss in
  let antes_body =
    if null antes then body
    else 
      "FROM " :: 
        catelim_string_of_list (catelim_separatedstring_of_seq 
                                  (if linesep="\n" then "\n     " else linesep))
                               (linesep^"AND ") antes 
          (linesep :: body)
  in
  if null provisos then antes_body
  else
    "WHERE " ::
      catelim_string_of_list catelim_string_of_proviso (linesep^"AND ") provisos
        (linesep :: antes_body)

and catelim_string_of_thmbody linesep provisos seq ss =
  if null provisos then catelim_separatedstring_of_seq linesep seq ss
  else catelim_string_of_rulebody linesep provisos [] seq ss

and string_of_rulebody linesep provisos antes conseq =
  implode (catelim_string_of_rulebody linesep provisos antes conseq [])

and string_of_thmbody linesep provisos seq =
  implode (catelim_string_of_thmbody linesep provisos seq [])
  
(* **************************************** export *********************************** *)

let rec paragraph_of_string report query s =
  let rec getpara () =
    match parseParagraph report query with
      Some p -> p
    | None ->
        raise
          (ParseError_
             ["Error: expecting paragraph beginning; found ";
              string_of_symbol (currsymb ())])
  in
  tryparse (fun _ -> getpara ()) s

let parsename = name_of_currsymb

