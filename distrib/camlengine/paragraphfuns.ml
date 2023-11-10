(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
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

open Japeenv
open Listfuns
open Menu
open Paragraph
open Runproof
open Sml
open Tactictype
open Thing
open Checkthing (* provides new addthing *)
open Structurerule

exception Catastrophe_ = Miscellaneous.Catastrophe_

let addautorule = Proofstate.addautorule
let adddoubleclick = Doubleclick.adddoubleclick
let addforcedef = Disproof.addforcedef
let atmapping = Mappingfuns.(<@>)
let cleanup x = x
let consolereport = Miscellaneous.consolereport
let empty = Mappingfuns.empty
let enQuote = Stringfuns.enQuote
let freezesaved = Proofstore.freezesaved
let idf = fun x -> x
let isQuoted = Stringfuns.isQuoted
let mkmap = Mappingfuns.mkmap
let string_of_name = Name.string_of_name
let newcxt = Cxtfuns.newcxt
let optionfilter = Optionfuns.optionfilter
let paramvar = Paraparam.paramvar
let parseablestring_of_name = Name.parseablestring_of_name
let word_of_proofstage = Proofstage.word_of_proofstage
let string_of_proviso = Proviso.string_of_proviso
let provisovars =
  Proviso.provisovars Termfuns.termvars Termfuns.tmerge
let string_of_seq = Sequent.string_of_seq
let seqvars = Sequent.seqvars Termfuns.termvars Termfuns.tmerge
let setfontstuff = Button.setfontstuff
let setmenuentry = Japeserver.menuentry
(* let setpanelbutton = Japeserver.setpanelbutton *)
let string_of_tactic = Tactic.string_of_tactic
let string_of_term = Termstring.string_of_term
let thawsaved = Proofstore.thawsaved
let tickmenuitem = Japeserver.tickmenuitem
let tmerge = Termfuns.tmerge
let uncurry2 = Miscellaneous.uncurry2
let disQuote = Stringfuns.disQuote
(* let _VALFROM = Termparse.asTactic Termparse.term_of_string *)

let rec addstructurerule report query stype rule =
  let thingtype =
    match stype with
    | "CUT"         -> CutRule
    | "WEAKEN"      -> LeftWeakenRule
    | "LEFTWEAKEN"  -> LeftWeakenRule
    | "RIGHTWEAKEN" -> RightWeakenRule
    | "IDENTITY"    -> IdentityRule
    | "TRANSITIVE"  -> TransitiveRule
    | "REFLEXIVE"   -> ReflexiveRule
    | _ -> raise (Catastrophe_ ["bad structure rule type "; stype])
  in
  if Thing.addstructurerule thingtype rule then ()
  else
    report
      ["STRUCTURERULE "; stype; " "; parseablestring_of_name rule; " failed"]

let rulename p =
  match p with
    Conjecture (RuleHeading (name, _, _), _)       -> Some name
  | MacroDef   (TacticHeading (name, _), _)        -> Some name
  | Proof      (name, _, _, _, _)                  -> Some name
  | RuleDef    (RuleHeading (name, _, _), _, _, _) -> Some name
  | RuleGroup  (RuleHeading (name, _, _), _)       -> Some name
  | TacticDef  (TacticHeading (name, _), _)        -> Some name
  | Theory     (RuleHeading (name, _, _), _)       -> Some name
  | _ -> None

let paragraphid p =
  match p with
    AutoRule      _ -> "AutoRule"
  | Conjecture    _ -> "Conjecture"
  | File          _ -> "File"
  | FontSpec      _ -> "FontSpec"
  | ForceDef      _ -> "ForceDef"
  | HitDef        _ -> "HitDef"
  | InitVar       _ -> "InitVar"
  | MacroDef      _ -> "Macro"
  | Menu          _ -> "Menu"
  | Panel         _ -> "Panel"
  | Proof         _ -> "Proof"
  | RuleDef       _ -> "RuleDef"
  | RuleGroup     _ -> "RuleGroup"
  | StructureRule _ -> "StructureRule"
  | TacticDef     _ -> "TacticDef"
  | Theory        _ -> "Theory"

let rec entrynames place con (p, es) =
  match p, rulename p with
    Theory (_, tps), _      -> nj_fold (entrynames place con) tps es
  | _              , Some n -> con n :: es (* without the theory name *)
  | _ -> raise (Catastrophe_ (paragraphid p :: " found in " :: place))
  
(* now designed for nj_revfold, cos it produces japeenv and proofs and buttonfns *)
let rec interpret
  report query where params provisos enter
    (paragraph, (env, proofs, buttonfns as res)) =
  let rec _Par =
    function
      [] -> params
    | ps -> ps
  in
  let rec _Pro =
    function
      [] -> provisos
    | ps -> ps
  in
  let rec newbuttonenv env text var settings defval =
    let defval' =
      match defval, env <@> var, settings with
        Some v, _, _ -> v
      | _, Some v, s :: _ -> string_of_term v
      | _, _, s :: _ -> s
      | _ -> raise (Catastrophe_ [text; " without settings"])
    in
    let env' =
      try
        begin try checkrange env var settings with
          OutOfRange_ range ->
            report
              [text; " settings are ";
               bracketed_string_of_list idf ", " settings;
               " - variable "; parseablestring_of_name var;
               " can only be set to "; range];
            raise Use_
        end;
        begin try checkrange env var [defval'] with
          OutOfRange_ range ->
            report
              [text; " default value is "; defval'; " - variable ";
               parseablestring_of_name var; " can only be set to "; range];
            raise Use_
        end;
        env
      with
        NotJapeVar_ ->
          (env ++ (var ||-> japerefvar settings defval' (ref "")))
    in
    Japeenv.stringset env' var defval'; env', defval'
  in
  let rec processCheckBox env (var, label, (vval1, vval2), defvalopt) =
    let (env, _) =
      newbuttonenv env "CHECKBOX" var [vval1; vval2] defvalopt
    in
    let rec doit a1 a2 =
      match a1, a2 with
        f, ("true", true) -> f true
      | f, ("false", true) -> f false
      | _, _ -> ()
    in
    env, doit
  in
  let rec processRadioButton env (var, entries, defval) =
    let settings = (snd <* entries) in
    let (env, _) = newbuttonenv env "RADIOBUTTON" var settings defval in
    let map = mkmap ((fun (l, v) -> v, l) <* entries) in
    let rec basefn f (v, tf) =
      match atmapping map v with
        Some label -> f (label, tf)
      | None -> ()
    in
    env, basefn
  in
  let rec filterpros vars ps =
    match ps with
      [] ->
           (fun p ->
              not
                (List.exists (fun v -> not (member (v, vars)))
                   (provisovars p))) <|
           provisos
    | ps -> ps
  in
  let visprovisos ps = (fun p -> true, p) <* ps in
  let filterparams vars ps =
    match ps with
      [] -> (fun p -> member (paramvar p, vars)) <| params
    | ps -> ps
  in
  match paragraph with
  | AutoRule (sense, commands) ->
      List.iter (fun com -> addautorule (sense, com)) commands; res
  | Conjecture (RuleHeading (name, params, provisos), sequent) ->
      let svs = seqvars sequent in
      let thing =
        Theorem
          (filterparams svs params, visprovisos (filterpros svs provisos),
           sequent)
      in
      addthing (name, thing, where); res
  | File (_, paras) ->
      nj_revfold (interpret report query where [] [] enter) paras res
  | FontSpec stuff -> setfontstuff stuff; res
  | ForceDef stuff -> addforcedef stuff; res
  | HitDef stuff   -> adddoubleclick stuff; res
  | InitVar (name, term) ->
      let lreport ss =
        report ("can't INITIALISE " :: parseablestring_of_name name :: ss);
        raise Use_
      in
      begin try Japeenv.termset env name term with
        OutOfRange_ range ->
          lreport
            [" to "; string_of_term term; " - variable can only be set to ";
             range]
      | NotJapeVar_ ->
          lreport [" - it isn't a variable in the environment"]
      | ReadOnly_ ->
          lreport [" - it can't be altered, given the state of other stored values"]
      end;
      res
  | MacroDef (TacticHeading (name, params), macro) ->
      addthing (name, Macro (params, macro), where); res
  | Menu (mproof, mlabel, mparas) ->
      let rec tacentry name =
        MCdata (Mentry (name, None, "apply " ^ parseablestring_of_name name))
      in
      let rec processcommand (mp, (env, buttonfns, mcs, mps (* as res *))) =
        match mp with
          Menustuff c ->
            (match c with 
               MCdata   e         -> let env, buttonfns = processdata e env buttonfns in
                                     (env, buttonfns, c::mcs, mps)
             | MCbefore (name, e) -> let env, buttonfns = processdata e env buttonfns in
                                     (env, buttonfns, c::mcs, mps)
             | MCrename _         -> (env, buttonfns, c::mcs, mps))
        | Menupara p ->
            env, buttonfns,
            entrynames ["Menu "; string_of_name mlabel] tacentry (p, mcs),
            p :: mps
      and processdata e env buttonfns =          
        match e with
          Mcheckbox (var, label, _, _ as stuff) ->
            let (env, basefn) = processCheckBox env stuff in
            let rec tickfn tf =
              setmenuentry
                (string_of_name mlabel) mproof (string_of_name label) None
                 ("assign " ^ parseablestring_of_name var ^
                   (if tf then " false" else " true"));
              tickmenuitem mproof (string_of_name mlabel) (string_of_name label) tf
            in
            env, (var, basefn tickfn) :: buttonfns
        | Mradiobutton (var, _, _ as stuff) ->
            let (env, basefn) = processRadioButton env stuff in
            let rec tickfn (label, tf) =
              tickmenuitem mproof (string_of_name mlabel) (string_of_name label) tf
            in
            env, (var, basefn tickfn) :: buttonfns
        | _ -> env, buttonfns
      in
      let (env, buttonfns, mes, mps) =
        nj_fold processcommand mparas (env, buttonfns, [], [])
      in
      Menu.addmenu mproof mlabel;
      Menu.addmenudata mproof mlabel mes;
      nj_revfold
        (interpret report query (InMenu mlabel) params provisos enter) mps
        (env, proofs, buttonfns)
  | Panel (plabel, pparas, kind) ->
      let rec tacentry name = Pentry (name, parseablestring_of_name name) in
      let rec process (pp, (env, buttonfns, pes, pps)) =
        match pp with
          Panelstuff e ->
             (*
               begin match e with
                 Pcheckbox (var, label, _, _ as stuff) ->
                   let (env, basefn) = processCheckBox env stuff in
                   let rec tickfn tf =
                     setpanelbutton (string_of_name plabel) (string_of_name label) tf
                   in
                   env, (var, basefn tickfn) :: buttonfns, e :: pes, pps
               | Pradiobutton (var, _, _ as stuff) ->
                   let (env, basefn) = processRadioButton env stuff in
                   let rec tickfn (label, tf) =
                     setpanelbutton (string_of_name plabel) (string_of_name label) tf
                   in
                   env, (var, basefn tickfn) :: buttonfns, e :: pes, pps
               | _ -> env, buttonfns, e :: pes, pps
               end
              *) env, buttonfns, e :: pes, pps
        | Panelpara p ->
            env, buttonfns,
            entrynames ["Panel "; string_of_name plabel] tacentry (p, pes),
            p :: pps
      in
      let (env, buttonfns, pes, pps) =
        nj_fold process pparas (env, buttonfns, [], [])
      in
      Menu.addpanel kind plabel;
      Menu.addpaneldata plabel pes;
      nj_revfold
        (interpret report query (InPanel plabel) params provisos enter)
        pps (env, proofs, buttonfns)
  | StructureRule (stype, rule) ->
      addstructurerule report query stype rule; res
  | Proof
      (name, stage, seq, (givens, params, provisos, tac), disproofopt (* as p *)) ->
      let rec updateplace (thing, oldplace) =
        if where <> oldplace && where <> InLimbo then
          addthing (name, thing, where)
      in
      (match givens, thingnamed name with
	   | [], Some (Theorem _, _ as info) -> updateplace info
	   | _ :: _, Some ((Theorem _ as thm), _) ->
		   report
			 ["processing proof of rule "; string_of_name name;
			  " - found theorem"; string_of_thing thm;
			  " stored under that name"];
		   raise Use_
	   | _, Some (Rule _, _ as info) -> updateplace info
	   | [], None ->
		   addthing
			 (name, Theorem (params, visprovisos provisos, seq), where)
	   | _ :: _, None ->
		   addthing
			 (name,
			  Rule ((params, visprovisos provisos, givens, seq), false),
			  where)
	   | _, Some (thing, _) ->
		   report
			 ["processing proof of "; string_of_name name; " - found ";
			  string_of_thing thing; " stored under that name"];
		   raise Use_
      );
      consolereport
        ["checking "; word_of_proofstage stage; " "; string_of_name name];
      let res =
        match doProof report query env name stage seq 
                      (params, givens, provisos, tac) disproofopt
        with
          Some r -> env, r :: proofs, buttonfns
        | None -> res
      in
      res
  | RuleDef (RuleHeading (name, params, provisos), top, bottom, axiom) ->
      let rvs = nj_fold (uncurry2 tmerge) ((seqvars <* top)) (seqvars bottom) in
      let thing =
        Rule
          ((filterparams rvs params,
            visprovisos (filterpros rvs provisos), top, bottom),
           axiom)
      in
      addthing (name, thing, where); res
  | RuleGroup (RuleHeading (name, params, provisos), paras) ->
      addalttactic name paras params where;
      (* in the menu/panel *)
      nj_revfold
        (interpret report query where (_Par params) (_Pro provisos) false)
        paras res
  | TacticDef (TacticHeading (name, params), tac) ->
      addthing (name, Tactic (params, tac), where); res
  | Theory (RuleHeading (name, params, provisos), paras) ->
      addalttactic name paras params InLimbo;
      (* not in a menu/panel *)
      nj_revfold
        (interpret report query where (_Par params) (_Pro provisos) enter)
        paras res

and addalttactic name paras params where =
  addthing
    (name, Tactic (params, TheoryAltTac (optionfilter rulename paras)),
     where)

and interpretParasFrom report query (env, proofs, buttonfuns as res) filenames =
  freezesaved ();
  let r = (try
             nj_revfold (interpret report query InLimbo [] [] true)
               (nj_fold (fun (x, y) -> x @ y)
                        ((paragraphs_of_file report query env <.> disQuote) <*
                        filenames)
                  [])
               res
           with
             Catastrophe_ ss ->
               report ("Catastrophic error: " :: ss); thawsaved (); raise Use_
           | CompileThing_ ss -> report ss; thawsaved (); raise Use_
           | exn -> thawsaved (); raise exn
          )
    (* including Use_ *)
  in thawsaved (); r
  
let rec conjecturename =
  function
    Conjecture (RuleHeading (name, _, _), _) -> name
  | _                                        -> raise Use_

let interpret report query params provisos res paragraph = (* for export *)
  interpret report query InLimbo params provisos true (paragraph, res)

