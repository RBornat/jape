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

open Japeenv
open Listfuns
open Menu
open Paragraph
open Runproof
open Sml
open Tactictype
open Thing

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
let isQuoted = Stringfuns.isQuoted
let mkmap = Mappingfuns.mkmap
let namestring = Name.namestring
let newcxt = Cxtfuns.newcxt
let optionfilter = Optionfuns.optionfilter
let paramvar = Paraparam.paramvar
let parseablenamestring = Name.parseablenamestring
let proofstage2word = Proofstage.proofstage2word
let provisostring = Proviso.provisostring
let provisovars =
  Proviso.provisovars Termfuns.termvars Termfuns.tmerge
let seqstring = Sequent.seqstring
let seqvars = Sequent.seqvars Termfuns.termvars Termfuns.tmerge
let setfontstuff = Button.setfontstuff
let setmenuentry = Japeserver.menuentry
(* let setpanelbutton = Japeserver.setpanelbutton *)
let tacticstring = Tactic.tacticstring
let termstring = Termstring.termstring
let thawsaved = Proofstore.thawsaved
let tickmenuitem = Japeserver.tickmenuitem
let tmerge = Termfuns.tmerge
let uncurry2 = Miscellaneous.uncurry2
let disQuote = Stringfuns.disQuote
let _VALFROM = Termparse.asTactic Termparse.term_of_string

(* let profileOff = Profile.profileOff
   let profileOn = Profile.profileOn
   let profileReset = Profile.reset
 *)

let rec addstructurerule report query stype rule =
  let thingtype =
	match stype with
	  "CUT" -> CutRule
	| "WEAKEN" -> LeftWeakenRule
	| "LEFTWEAKEN" -> LeftWeakenRule
	| "RIGHTWEAKEN" -> RightWeakenRule
	| "IDENTITY" -> IdentityRule
	| "TRANSITIVE" -> TransitiveRule
	| "REFLEXIVE" -> ReflexiveRule
	| _ -> raise (Catastrophe_ ["bad structure rule type "; stype])
  in
  if Thing.addstructurerule thingtype rule then ()
  else
	report
	  ["STRUCTURERULE "; stype; " "; parseablenamestring rule; " failed"]
let rec rulename p =
  match p with
	Conjecture (RuleHeading (name, _, _), _) -> Some name
  | MacroDef (TacticHeading (name, _), _) -> Some name
  | Proof (name, _, _, _, _) -> Some name
  | RuleDef (RuleHeading (name, _, _), _, _, _) -> Some name
  | RuleGroup (RuleHeading (name, _, _), _) -> Some name
  | TacticDef (TacticHeading (name, _), _) -> Some name
  | Theory (RuleHeading (name, _, _), _) -> Some name
  | _ -> None
let rec paragraphid p =
  match p with
	AutoRule _ -> "AutoRule"
  | Conjecture _ -> "Conjecture"
  | File _ -> "File"
  | FontSpec _ -> "FontSpec"
  | ForceDef _ -> "ForceDef"
  | HitDef _ -> "HitDef"
  | InitVar _ -> "InitVar"
  | MacroDef _ -> "Macro"
  | Menu _ -> "Menu"
  | Panel _ -> "Panel"
  | Proof _ -> "Proof"
  | RuleDef _ -> "RuleDef"
  | RuleGroup _ -> "RuleGroup"
  | StructureRule _ -> "StructureRule"
  | TacticDef _ -> "TacticDef"
  | Theory _ -> "Theory"
let rec entrynames place con (p, es) =
  match p, rulename p with
	Theory (_, tps), _ -> nj_fold (entrynames place con) tps es
  | _, Some n ->(* without the theory name *)
	 con n :: es
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
	  | _, Some v, s :: _ -> termstring v
	  | _, _, s :: _ -> s
	  | _ -> raise (Catastrophe_ [text; " without settings"])
	in
	let env' =
	  try
		begin try checkrange env var settings with
		  OutOfRange_ range ->
			report
			  [text; " settings are ";
			   bracketedliststring (fun s -> s) ", " settings;
			   " - variable "; parseablenamestring var;
			   " can only be set to "; range];
			raise Use_
		end;
		begin try checkrange env var [defval'] with
		  OutOfRange_ range ->
			report
			  [text; " default value is "; defval'; " - variable ";
			   parseablenamestring var; " can only be set to "; range];
			raise Use_
		end;
		env
	  with
		NotJapeVar_ ->
		  (env ++ (var ||-> japerefvar settings defval' (ref "")))
	in
	Japeenv.set (env', var, _VALFROM defval'); env', defval'
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
  let rec visprovisos ps = (fun p -> true, p) <* ps in
  let rec filterparams vars ps =
	match ps with
	  [] -> (fun p -> member (paramvar p, vars)) <| params
	| ps -> ps
  in
  match paragraph with
	AutoRule (sense, commands) ->
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
  | HitDef stuff -> adddoubleclick stuff; res
  | InitVar (name, term) ->
	  let rec lreport ss =
		report ("can't INITIALISE " :: parseablenamestring name :: ss);
		raise Use_
	  in
	  begin try Japeenv.set (env, name, term) with
		OutOfRange_ range ->
		  lreport
			[" to "; termstring term; " - variable can only be set to ";
			 range]
	  | NotJapeVar_ ->
		  lreport [" - it isn't a variable in the environment"]
	  | ReadOnly_ ->
		  if Japeenv.(<@>) env name = Some term then ()
		  else
		    lreport [" - it can't be altered, given the state of other stored values"]
	  end;
	  res
  | MacroDef (TacticHeading (name, params), macro) ->
	  addthing (name, Macro (params, macro), where); res
  | Menu (mproof, mlabel, mparas) ->
	  let rec tacentry name =
		Mentry (name, None, "apply " ^ parseablenamestring name)
	  in
	  let rec process (mp, (env, buttonfns, mes, mps as res)) =
		match mp with
		  Menustuff e ->
			begin match e with
			  Mcheckbox (var, label, _, _ as stuff) ->
				let (env, basefn) = processCheckBox env stuff in
				let rec tickfn tf =
				  setmenuentry
					(namestring mlabel) (namestring label) None
					 ("assign " ^ parseablenamestring var ^
					   (if tf then " false" else " true"));
				  tickmenuitem (namestring mlabel) (namestring label) tf
				in
				env, (var, basefn tickfn) :: buttonfns, e :: mes, mps
			| Mradiobutton (var, _, _ as stuff) ->
				let (env, basefn) = processRadioButton env stuff in
				let rec tickfn (label, tf) =
				  tickmenuitem (namestring mlabel) (namestring label) tf
				in
				env, (var, basefn tickfn) :: buttonfns, e :: mes, mps
			| _ -> env, buttonfns, e :: mes, mps
			end
		| Menupara p ->
			env, buttonfns,
			entrynames ["Menu "; namestring mlabel] tacentry (p, mes),
			p :: mps
	  in
	  let (env, buttonfns, mes, mps) =
		nj_fold process mparas (env, buttonfns, [], [])
	  in
	  Menu.addmenu mproof mlabel;
	  Menu.addmenudata mlabel mes;
	  nj_revfold
		(interpret report query (InMenu mlabel) params provisos enter) mps
		(env, proofs, buttonfns)
  | Panel (plabel, pparas, kind) ->
	  let rec tacentry name = Pentry (name, parseablenamestring name) in
	  let rec process (pp, (env, buttonfns, pes, pps)) =
		match pp with
		  Panelstuff e ->
		     (*
               begin match e with
                 Pcheckbox (var, label, _, _ as stuff) ->
                   let (env, basefn) = processCheckBox env stuff in
                   let rec tickfn tf =
                     setpanelbutton (namestring plabel) (namestring label) tf
                   in
                   env, (var, basefn tickfn) :: buttonfns, e :: pes, pps
               | Pradiobutton (var, _, _ as stuff) ->
                   let (env, basefn) = processRadioButton env stuff in
                   let rec tickfn (label, tf) =
                     setpanelbutton (namestring plabel) (namestring label) tf
                   in
                   env, (var, basefn tickfn) :: buttonfns, e :: pes, pps
               | _ -> env, buttonfns, e :: pes, pps
               end
              *) env, buttonfns, e :: pes, pps
		| Panelpara p ->
			env, buttonfns,
			entrynames ["Panel "; namestring plabel] tacentry (p, pes),
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
	  (name, stage, seq, (givens, params, provisos, tac), disproofopt as
		 p) ->
	  let rec updateplace (thing, oldplace) =
		if where <> oldplace && where <> InLimbo then
		  addthing (name, thing, where)
	  in
	  begin match givens, thingnamed name with
		[], Some (Theorem _, _ as info) -> updateplace info
	  | _ :: _, Some ((Theorem _ as thm), _) ->
		  report
			["processing proof of rule "; namestring name;
			 " - found theorem"; thingstring thm;
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
			["processing proof of "; namestring name; " - found ";
			 thingstring thing; " stored under that name"];
		  raise Use_
	  end;
	  consolereport
		["checking "; proofstage2word stage; " "; namestring name];
	  (* edit this bit of code to profile the checking bit of proof reload *)
	  (* profileOn(); *)
	  let res =
		match
		  doProof report query env name stage seq (givens, provisos, tac)
			disproofopt
		with
		  Some r -> env, r :: proofs, buttonfns
		| None -> res
	  in
	  (* profileOff(); *) res
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
(* edit comments in this function to profile the whole proof reload operation *)
and interpretParasFrom report query res filenames =
  freezesaved ();
  (* profileOff(); profileReset(); *) (* profileOn(); *)
  let r = (try
			 nj_revfold (interpret report query InLimbo [] [] true)
			   (nj_fold (fun (x, y) -> x @ y)
					    ((file2paragraphs report query <.> disQuote) <*
					    filenames)
				  [])
			   res
		   with
			 Catastrophe_ ss ->
			   (* profileOff(); *) report ("Catastrophic error: " :: ss); thawsaved (); raise Use_
		   | CompileThing_ ss ->(* profileOff(); *)  report ss; thawsaved (); raise Use_
		   | exn ->(* profileOff(); *)  thawsaved (); raise exn)
	(* including Use_ *)
in (* profileOff(); *)  (thawsaved ()); r
let rec conjecturename =
  function
	Conjecture (RuleHeading (name, _, _), _) -> name
  | _ -> raise Use_

let interpret report query params provisos res paragraph = (* for export *)
  interpret report query InLimbo params provisos true (paragraph, res)

