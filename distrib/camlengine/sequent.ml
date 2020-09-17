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

open Idclass
open Idclassfuns
open Listfuns
open Match
open Mappingfuns
open Miscellaneous
open Optionfuns
open Sml
open Symbol
open Symboltype
open Termfuns
open Termstore
open Termstring
open Termtype
open Termparse    

let error = Miscellaneous.error

let bagkind = BagClass FormulaClass
and listkind = ListClass FormulaClass
and formulakind = FormulaClass

type kind = Syntactic | Semantic

type seq = Seq of (string * term * term) (* stile, left, right *)

(* Because everything in a tree has to have a resource number, single-formula
 * sides of sequents are encoded as lists (not bags because the unification of
 * lists is simpler :-)).  This causes difficulties, because
 * I keep forgetting ....
 * RB 17/x/1996
 *)
let trueclass =
  function
    FormulaClass -> listkind
  | c            -> c

let validforms = [bagkind; listkind; formulakind]

let string_of_syntaxclass =
  function
    FormulaClass -> "FORMULA"
  | BagClass FormulaClass -> "BAG"
  | ListClass FormulaClass -> "LIST"
  | c -> raise (Catastrophe_ ["string_of_syntaxclass "; unparseidclass c])

let string_of_syntax (kind, hyps, stile, concs) =
  (match kind with
     Syntactic -> "SEQUENT "
   | Semantic -> "(SEMANTIC)SEQUENT ") ^
  string_of_syntaxclass hyps ^ " " ^ stile ^ " " ^ string_of_syntaxclass concs
    
let sequent_descriptions : (kind * idclass * string * idclass) list ref = ref []

let semanticturnstiles : (string, string) mapping ref = ref empty

let rec syntacticsequents () =
     (function
        Syntactic, _, _, _ -> true
      | _                  -> false) <| !sequent_descriptions

let rec getsyntacticturnstiles () = ((fun(_,_,stile,_)->stile) <* syntacticsequents ())

let rec lookupsyntax stilestring =
  findfirst
    (fun (_, _, stile, _ as syn) ->
       if stile = stilestring then Some syn else None)
    !sequent_descriptions

let rec lookupSTILE st =
  match lookupsyntax st with
    Some syn -> syn
  | None -> raise (Catastrophe_ ["couldn't find syntax for "; st])

let rec getsemanticturnstile syn = (!semanticturnstiles <@> syn)

let rec resetsyntaxandturnstiles () =
  sequent_descriptions := []; semanticturnstiles := empty

let alwaysshowturnstile = ref false

let rec sqs linesep tf (Seq (st, hs, gs)) ss =
  let rec default () =
    let tail =
      st :: " " :: (if isemptycollection gs then ss else tf gs ss)
    in
    if isemptycollection hs then tail else tf hs (linesep :: tail)
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

let catelim_string_of_seq = 
  sqs " " (catelim_string_of_collection ", ")
let catelim_invisbracketedstring_of_seq b = 
  sqs " " (catelim_invisbracketedstring_of_collection b ", ") 
let catelim_debugstring_of_seq = sqs " " catelim_debugstring_of_term
let catelim_separatedstring_of_seq linesep =
  sqs linesep (catelim_string_of_collection (","^linesep))

let catelim_elementstring_of_seq =
  sqs " " (function
             Collection (_, _, es) ->
               catelim_string_of_list (catelim_debugstring_of_element catelim_string_of_term)
                 ", " es
           | t -> catelim_string_of_term t)
     
let string_of_seq = stringfn_of_catelim catelim_string_of_seq
let invisbracketedstring_of_seq = stringfn_of_catelim <.> catelim_invisbracketedstring_of_seq
let debugstring_of_seq = stringfn_of_catelim catelim_debugstring_of_seq
let elementstring_of_seq = stringfn_of_catelim catelim_elementstring_of_seq

let rec seqexplode = fun (Seq s) -> s

(* this is never going to go wrong, and if it does, I can instrument the calls to pin it down *)
let seq_entrails (Seq (st, hs, gs) as seq) = 
  match hs, gs with
  | Collection (_, hkind, hes), Collection (_, gkind, ges) -> st, hkind, hes, gkind, ges
  | _ -> raise (Catastrophe_ ["sequent "; string_of_seq seq;
                              " exploded into ("; debugstring_of_term hs; ", ";
                              st; ", ";  debugstring_of_term gs; ")"])


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
              string_of_symbol (currsymb ())])
    | (_, els), FormulaClass ->
        raise
          (ParseError_
             ["single formula expected on left-hand side of sequent; found ";
              string_of_term (Collection (None, listkind, els))])
    | (None, els), c -> registerCollection (c, els)
    | (Some c', els), c ->
        if c = c' then registerCollection (c, els)
        else
          raise
            (ParseError_
               [string_of_syntaxclass c;
                " expected on left-hand side of sequent; found ";
                string_of_term (Collection (None, c', els))])
  in
  let rec parseside =
    function
      BagClass FormulaClass as c -> parseCollection c
    | ListClass FormulaClass as c -> parseCollection c
    | FormulaClass -> formside (registerElement (Nonum, parseTerm EOF))
    | x -> error ["internal error parseSeq parseside "; unparseidclass x]
  in
  match currsymb () with
  | STILE st ->
      (* we can sometimes start with a stile ... *)
      let (_, hypform, _, concform) = lookupSTILE st in
      if hypform = formulakind then
        raise
          (ParseError_
             ["badly formed sequent - lhs formula missing before "; st])
      else
        let _ = scansymb () in
        Seq (st, conformside ((None, []), hypform), parseside concform)
  | _->
      let first =
        parseElementList canstartTerm parseTerm commasymbol None
      in
      match currsymb (), first, syntacticsequents () with
      | STILE st, _, _ ->
          let (_, hypform, _, concform) = lookupSTILE st in
          let _ = scansymb () in
          Seq (st, conformside (first, hypform), parseside concform)
      | _, (None, [el]), [_, hypform, st, concform] ->
          begin match hypform, concform with
            FormulaClass, _ ->
              raise
                (ParseError_
                   ["badly formed sequent - "; st; " expected after ";
                    string_of_term (formside el)])
          | _, FormulaClass ->
              Seq (st, registerCollection (hypform, []), formside el)
          | _ ->
              raise
                (ParseError_
                   ["badly formed sequent - "; st; " expected after ";
                    string_of_term (Collection (None, hypform, [el]))])
          end
      | sy, (_,[]), _  -> raise (ParseError_ ["sequent expected - saw "; string_of_symbol sy])
      | _, (_, els), _ -> raise (ParseError_ ["badly formed sequent - some kind of turnstile expected after ";
                                              string_of_term (Collection (None, listkind, els))])

let canstartSeq = canstartTerm

let rec mkSeq (st, hs, gs) =
  let (_, hypform, _, concform) = lookupSTILE st in
  let sh = trueclass hypform in
  let sg = trueclass concform in
  Seq (st, registerCollection (sh, hs), registerCollection (sg, gs))

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
  
let rec sequent_of_string s = tryparse (fun _ -> parseSeq ()) s

let rec seqvars termvars tmerge =
  fun (Seq (st, hs, gs)) -> tmerge (termvars hs) (termvars gs)
let rec seqVIDs s = orderVIDs ((vid_of_var <* seqvars termvars tmerge s))

let rec eqseqs =
  fun (Seq (st1, h1s, g1s), Seq (st2, s_of_h, s_of_g)) ->
    (st1 = st2 && eqterms (h1s, s_of_h)) && eqterms (g1s, s_of_g)
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
    nj_fold (uncurry2 max) ((int_of_resnum <* elementnumbers hs))
      (nj_fold (uncurry2 max) ((int_of_resnum <* elementnumbers gs)) 0)

let syntaxes = ref []

let pushSyntax name = 
  syntaxes := (name, !sequent_descriptions, !semanticturnstiles, !alwaysshowturnstile) :: !syntaxes;
  resetsyntaxandturnstiles ()

let popSyntax () =
  match !syntaxes with
    (_, sqds, sems, show) :: sys ->
      sequent_descriptions := sqds; semanticturnstiles := sems;
      alwaysshowturnstile := show;
      syntaxes := sys
  | _ -> raise (ParseError_ ["sequent popSyntax stack empty"])

let popAllSyntaxes () =
  while !syntaxes<>[] do popSyntax () done

let rec describeSeqs ds =
  let rec f (hyps, stile, concs) =
    let description = (Syntactic, hyps, stile, concs) in
    (match !syntaxes with 
       (name, sqds, _, _) :: _ ->
         if not (List.mem description sqds) then
           raise (ParseError_ ["After PUSHSYNTAX "; Stringfuns.enQuote name; 
                               " attempt to declare novel sequent syntax ";
                               string_of_syntax description])
     | _ -> ());
    match lookupsyntax stile with
      Some (kind, hyps', _, concs' as syn') ->
        if (kind = Syntactic && hyps = hyps') && concs = concs' then ()
        else
          error
            ["you cannot redeclare "; string_of_syntax syn'; " as ";
             string_of_syntax description]
    | None ->
        if member (hyps, validforms) && member (concs, validforms) then
          begin
            (* consolereport ["accepting ", show syn]; *)
            sequent_descriptions := description :: !sequent_descriptions;
            enter stile None None (STILE stile)
          end
        else
          error
            ["bad syntactic sequent syntax description ";
             string_of_syntax (Syntactic, hyps, stile, concs)]
  in
  (* consolereport ["describing ", bracketed_string_of_list show "," ds]; *)
  List.iter f ds

let rec setsemanticturnstile syn sem =
  (match !syntaxes with 
     (name, _, sems, _) :: _ ->
       if sems<@>syn <> (Some sem) then
         raise (ParseError_ ["After PUSHSYNTAX "; Stringfuns.enQuote name; 
                             " attempt to declare novel turnstile pair ";
                             syn; " "; sem])
   | _ -> ());
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
          None -> (Semantic, hyps, sem, concs) :: !sequent_descriptions
        | Some (Syntactic, _, _, _) ->
            bad [sem; " is a syntactic turnstile"]
        | Some (Semantic, hyps', _, concs' as sem') ->
            if hyps = hyps' && concs = concs' then !sequent_descriptions
            else
              bad
                ["you cannot redeclare "; string_of_syntax sem'; " as ";
                 string_of_syntax (Semantic, hyps, sem, concs)]
  in
  let newturnstiles =
    match (!semanticturnstiles <@> syn) with
      None ->
        enter sem None None (STILE sem);
        (!semanticturnstiles ++ (syn |-> sem))
    | Some sem' ->
        if sem = sem' then !semanticturnstiles
        else bad ["semantic turnstile for "; syn; " is "; sem']
  in
  sequent_descriptions := newsyntaxes; semanticturnstiles := newturnstiles
      