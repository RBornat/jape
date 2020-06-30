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

open List
open Miscellaneous
open Listfuns
open Searchtree
open Symbol
open Symboltype
open Idclass
open Idclassfuns
open Termtype
open Termstring
open Termfuns
open Termstore
open Binding
open Optionfuns
open Stringfuns
open UTF

let termparsedebug = ref false

(* stuff to handle variable leftfix/outfix syntax *)

let symbeq : symbol * symbol -> bool = fun (x, y) -> x = y

let mkalt sts def s =
  match findfirst (fun (s', t) -> if s = s' then Some t else None) sts with
  | Some t -> t
  | None   -> def

let outrightfixtree : (symbol, symbol) searchtree ref =
  ref (emptysearchtree mkalt)

let leftmidfixtree : (symbol, unit) searchtree ref =
  ref (emptysearchtree mkalt)

let outrightfixes = ref []
let leftmidfixes  = ref []

let reset_termparse_vars () =
  outrightfixtree := emptysearchtree mkalt;
  leftmidfixtree := emptysearchtree mkalt;
  outrightfixes := []; leftmidfixes := []

let syntaxes = ref []

let pushSyntax name =
  syntaxes := (name, !outrightfixtree, !leftmidfixtree, !outrightfixes, !leftmidfixes) :: !syntaxes;
  reset_termparse_vars ()
  
let popSyntax () =
  match !syntaxes with
    (s, orT, lmT, ors, lms) :: sys ->
      outrightfixtree := orT; leftmidfixtree := lmT;
      outrightfixes := ors; leftmidfixes := lms;
      syntaxes := sys
  | _ -> 
      raise (ParseError_ ["termparse popSyntax stack empty"])
      
let popAllSyntaxes () =
  while !syntaxes<>[] do popSyntax() done
  
let declareOutRightfix braseps ket =
  (match !syntaxes with 
     (name, _, _, ors, _) :: _ ->
       if not (mem (braseps, ket) ors) then
         raise (ParseError_ ["After PUSHSYNTAX "; Stringfuns.enQuote name; 
                             " attempt to define novel OUTFIX/RIGHTFIX ";
                             string_of_list string_of_symbol " " (braseps @ [ket])
                             ])
   | _ -> ());
  outrightfixes := (braseps, ket) :: !outrightfixes;
  outrightfixtree :=
    addtotree (fun (x, y) -> x = y) !outrightfixtree (braseps, ket, false)

let declareLeftMidfix syms =
  (match !syntaxes with 
   | (name, _, _, _, lms) :: _ ->
       if not (mem syms lms) then
         raise (ParseError_ ["After PUSHSYNTAX "; Stringfuns.enQuote name; 
                             " attempt to define novel LEFTFIX/MIDFIX ";
                             string_of_list string_of_symbol " " syms])
   | _ -> ()
  );
  leftmidfixes := syms :: !leftmidfixes;
  leftmidfixtree :=
    addtotree (fun (x, y) -> x = y) !leftmidfixtree (syms, (), false)

let resettermparse () =
  reset_termparse_vars ();
  declareOutRightfix [BRA "("] (KET ")"); (* oh dear this was an sml bug -- what next? *)
  ()

let ignore s = 
  if !termparsedebug then
    consolereport ["ignore "; debugstring_of_symbol s; " (currsymb="; debugstring_of_symbol (currsymb()); ")"];
  Symbol.ignore s

let rec parseUnsepList start f =
  if start (currsymb ()) then 
    let v = f EOF in v :: parseUnsepList start f 
  else []

let parseList start f sep =
  if start (currsymb ()) then
    let rec more () = if currsymb () = sep then (scansymb (); one ()) else []
    and one () = let v = f sep in v :: more () in
    one ()
  else []

let canstartTerm sy =
  match sy with
    ID _ -> true
  | UNKNOWN _ -> true
  | NUM _ -> true
  | STRING _ -> true
  | BRA _ -> true
  | PREFIX _ -> true
  | LEFTFIX _ -> true
  | _ ->(* temporary hack to allow Collection formulae *)
     canstartCollectionidclass sy

let canstartAtom s =
  match s with
    ID _ -> true
  | UNKNOWN _ -> true
  | NUM _ -> true
  | STRING _ -> true
  | BRA _ -> true
  | LEFTFIX _ -> true
  | PREFIX _ -> true
  | _ -> false

let canstartidentifier s =
  match s with
    ID _ -> true
  | UNKNOWN _ -> true
  | BRA "(" -> true
  | _ ->(* because it can be (op) ?? *)
     false

let undecl : (string -> term -> term) ref =
  ref (fun s t -> raise (ParseError_ ["unclassified "; s; " "; string_of_term t]))

let unvar : (term -> term) ref =
  ref (fun t -> raise (ParseError_ [string_of_term t; " is not a variable"]))

let unclass : (string -> term -> term) ref =
  ref (fun id t ->
         raise (ParseError_ [unparseidclass (idclass t); " "; id; " "; string_of_term t;
                             " in formula"]))
let badstart s = 
  raise (ParseError_ ["beginning of formula expected, found "; debugstring_of_symbol s])

let unstart : (symbol -> term) ref = ref badstart

let checkclass (id, t) =
  match idclass t with
  | BagClass _  -> !unclass id t
  | ListClass _ -> !unclass id t
  | _           -> t

(* moved from let block for OCaml *)
  
let rec parseAtom () =
  let sy = currsymb () in
  scansymb ();
  match sy with
    ID      (s, Some c) -> checkclass ("identifier", registerId (vid_of_string s, c))
  | ID      (s, None  ) -> !undecl "identifier" (registerId (vid_of_string s, NoClass))
  | UNKNOWN (s, Some c) -> checkclass ("unknown", registerUnknown (vid_of_string s, c))
  | UNKNOWN (s, None  ) -> !undecl "unknown" (registerUnknown (vid_of_string s, NoClass))
  | NUM s ->
      begin try registerLiteral (Number (atoi s)) with
        _ -> raise (Catastrophe_ ["parseAtom can't convert "; s; " to an integer"])
      end
  | STRING  s -> registerLiteral (String s)
  | BRA   "(" -> parseBRA ()
  | BRA     _ -> (* special cos of (op) *) parseOutfix sy
  | LEFTFIX _ -> parseLeftfix (prio sy) sy
  | PREFIX  s ->
      let m = prio sy in
      checkAfterBra sy m;
      registerApp (registerId (vid_of_string s, OperatorClass), parseExpr m true)
  | s -> !unstart s

and checkAfterBra pre n =
  let sy = currsymb () in
  let checkpre m =
    if m < n && not (!utpecua) then
      raise
        (ParseError_
           [debugstring_of_symbol sy; " (priority "; string_of_int m;
            ") found after "; debugstring_of_symbol pre; " (priority ";
            string_of_int n; ")"])
  in
  match sy with
  | PREFIX  _ -> checkpre (prio sy)
  | LEFTFIX _ -> checkpre (prio sy)
  | _         -> ()

and checkAfterKet pre n =
  let sy = currsymb () in
  let checkpre m =
    if n < m && not (!utpecua) then
      raise
        (ParseError_
           [debugstring_of_symbol sy; " (priority "; string_of_int m;
            ") found after "; debugstring_of_symbol pre; " (priority ";
            string_of_int n; ")"])
  in
  match sy with
  | INFIX  _ -> checkpre (prio sy)
  | INFIXC _ -> checkpre (prio sy)
  | _        -> if canstartAtom sy then checkpre !appfix

and parseOutfix bra =
  (* check for empty bracketed form first *)
  match
    (fsmpos (rootfsm outrightfixtree) [bra] &~~
       (fun t ->
          match currsymb () with
            KET _ as ket ->
              (match scanfsm (fun _ -> ket) t [] ket with
               | Found (ket', []) ->
                   check ket';
                   Some (registerFixapp ((string_of_symbol <* [bra; ket]), []))
              | _ -> None
             )
          | _ -> None))
  with
    Some t -> t
  | None -> putbacksymb bra (* can this be right? *); snd (parseOutRightfixTail 0 [])

and parseRightfix m t = (* m is irrelevant *)
  let (ket, t) = parseOutRightfixTail 0 [t] in checkAfterKet ket m; t

and parseOutRightfixTail m ts = (* m is irrelevant *)
  match scanstatefsm treecurr (fun _ -> treenext 0 false) (* ignore maybelast *) (rootfsm outrightfixtree) ([], ts)
  with
  | Found (ket, (ss, ts)) ->
      check ket;
      ket, registerFixapp ((string_of_symbol <* rev (ket :: ss)), rev ts)
  | NotFound (ss, _) ->
      raise
        (ParseError_
           ["expected "; string_of_list string_of_symbol " or " ss; "; found ";
            string_of_symbol (currsymb ())])

and parseLeftfix m bra = putbacksymb bra; parseLeftMidfixTail m []

and parseMidfix m t = parseLeftMidfixTail m [t]

and parseLeftMidfixTail m ts =
  let tn m a maybelast =
    uncurry2 treenext (if maybelast then m,a else 0,false)
  in
  match scanstatefsm treecurr (tn m true) (rootfsm leftmidfixtree) ([], ts) with
  | Found (_, (ss, ts)) ->
      registerFixapp ((string_of_symbol <* rev ss), rev ts)
  | NotFound (ss, _) ->
      raise
        (ParseError_
           ["expected "; string_of_list string_of_symbol " or " ss; "; found ";
            string_of_symbol (currsymb ())])

and treecurr _ = currsymb ()

and treenext m a (sys, ts) =
  let s = currsymb () in
  let t = (let _ = scansymb () in parseExpr m a) in 
  s :: sys, t :: ts

and parseVariable () =
  let okv =
    function
      Id (_, _, VariableClass) as t -> t
    | Unknown (_, _, VariableClass) as t -> t
    | Id _ as t -> !unvar t
    | Unknown _ as t -> !unvar t
    | t -> raise (ParseError_ [string_of_term t; " is not a variable"])
  in
  okv (parseAtom ())

and parseBRA () =
  let defop s =
    let r = registerId (vid_of_string s, OperatorClass) in
    scansymb (); check (KET ")"); r
  in
  let defexpr () =
    let r =
      (let t = parseterm (KET ")") in
       match t with
       | Collection _             as t' -> t'
       | Id (_, _, OperatorClass) as t' -> t'
       | _                              -> enbracket t
      )
    in check (KET ")"); r
  in
  match currsymb () with
  | INFIX  s  -> defop s
  | INFIXC s  -> defop s
  | POSTFIX s -> defop s
  | PREFIX s  -> if peeksymb () = KET ")" then defop s else defexpr ()
  | sy        -> if sy = KET ")" then
				   (scansymb (); enbracket (registerTup (",", [])))
				 else defexpr ()

(* here a is not an associativity, it is 'left operator gets it' *)

and parseExpr n a =
  if !termparsedebug then
    consolereport ["parseExpr "; string_of_int n; " "; string_of_bool a; " (currsymb="; debugstring_of_symbol (currsymb()); ")"];
  let ( >> ) m n = m > n || m = n && not a in
  let rec pe t =
    let sy = currsymb () in
    let pq n' a' s down =
      let nextt a = scansymb (); checkAfterBra sy n'; parseExpr n' a
      in
      let rec pts () =
        if currsymb () = sy then 
          let v = nextt true in v :: pts () 
        else []
      in
      if n' >> n then
        pe (if a' = TupleAssoc then registerTup (s, t :: pts ())
            else down (s, nextt (a' = LeftAssoc)))
      else t
    in
    let pr n' down =
      if n' >> n then pe (down n' t) else t
    in
    match sy with
    | INFIX s ->
        pq (prio sy) (assoc sy) s
           (fun (s, t') ->
              registerApp
                (registerId (vid_of_string s, OperatorClass),
                 registerTup (",", [t; t'])))
    | INFIXC s ->
        pq (prio sy) (assoc sy) s
           (fun (s, t') ->
              registerApp
                (registerApp (registerId (vid_of_string s, OperatorClass), t), t'))
    | POSTFIX s ->
        let n' = prio sy in
        if n' >> n then
          (let _ = scansymb () in
            checkAfterBra sy n';
            pe (registerApp (registerId (vid_of_string s, OperatorClass), t))
          )
        else t
    | RIGHTFIX _ -> pr (prio sy) parseRightfix
    | MIDFIX   _ -> pr (prio sy) parseMidfix
    | SUBSTBRA   ->
        if !substfix >> n then
          let t = registerSubst (true, t, parseSubststuff ()) in
          checkAfterKet SUBSTKET !substfix; pe t
        else t
    | _ ->
        if canstartAtom sy && !appfix >> n then
          pe (registerApp (t, parseExpr !appfix true))
        else t
  in
  pe (parseAtom ())

and parseSubststuff () =
  let parseside b =
    if b then
      parseList canstartidentifier (fun _ -> parseVariable ()) commasymbol
    else parseList canstartTerm parseterm commasymbol
  in
  let _ = check SUBSTBRA in
  let xs = parseside !substsense in
  let _ = check SUBSTSEP in
  let ys = parseside (not !substsense) in
  let _ = check SUBSTKET in
  if length xs = length ys then
    if !substsense then (xs ||| ys) else (ys ||| xs)
  else
    (* Zip_ can't happen *)
    raise
      (ParseError_
         ["Substitution "; string_of_symbol SUBSTBRA;
          string_of_list string_of_term "," xs; string_of_symbol SUBSTSEP;
          string_of_list string_of_term "," ys; string_of_symbol SUBSTKET;
          " is unbalanced"])

and parseterm fsy =
  if !termparsedebug then
    consolereport ["parseterm "; debugstring_of_symbol fsy; " (currsymb="; debugstring_of_symbol (currsymb()); ")"];
  if canstartCollectionidclass (currsymb ()) then
    let c = parseidclass "" in
    let (_, els) =
      parseElementList canstartTerm parseterm commasymbol (Some c)
    in
    registerCollection (c, els)
  else
    match fsy with
      INFIX  _ -> parseExpr (prio fsy) true
    | INFIXC _ -> parseExpr (prio fsy) true
    | _        -> parseExpr 0 false

and parseElementList starter parser__ sep k =
  let kind : idclass option ref = ref k in
  let iscoll c =
    match c with
      BagClass _ -> true
    | ListClass _ -> true
    | _ -> false
  in
  let parseElement sep =
    let rec getSegvar () =
      let id con s k' =
        let def k = scansymb (); Some ([], con (s, k)) in
        match !kind with
          Some k ->
            if k = k' then def k
            else if iscoll k' then
              raise
                (ParseError_
                   [unparseidclass k'; " identifier found with ";
                    unparseidclass k; " identifier"])
            else None
        | None ->
            if iscoll k' then begin kind := Some k'; def k' end else None
      in
      match currsymb () with
        PREFIX sp as sy ->
          (scansymb ();
           match getSegvar () with
             Some (ps, v) -> Some (registerId (vid_of_string sp, OperatorClass) :: ps, v)
           | None -> putbacksymb sy; None
          )
      | ID (s, Some k') -> id registerId (vid_of_string s) k'
      | UNKNOWN (s, Some k') -> id registerUnknown (vid_of_string s) k'
      | _ -> None
    in
    match getSegvar () with
      Some r -> registerSegvar r
    | None -> registerElement (Nonum, parser__ sep)
  in
  let elements = parseList starter parseElement sep in !kind, elements

and parseTerm sy =
  let t = parseterm sy in
  let rec doit t = mapterm findbinding t
  and findbinding t =
    match bindingstructure t with
      Some ((bs, us, ss), env, pat) ->
        Some
          (registerBinding
             ((bs, (doit <* us), (doit <* ss)), env, pat))
    | None -> None
  in
  doit t

(* -------------------------------------------------------------------------------- *)
(*                               the interface functions                            *)
 
let parseBindingpattern () = parseterm EOF

(* no binding processing, please ... *)
   
let parseidentifier _ =
  let okv =
    function
      Id _ as t -> t
    | Unknown _ as t -> t
    | t -> raise (ParseError_ [string_of_term t; " is not an identifier"])
  in
  okv (parseAtom ())

(* for parsing sequents *)

let parseCollection k =
  let (_, els) =
    parseElementList canstartTerm parseTerm commasymbol (Some k)
  in
  registerCollection (k, els)

(* a desperate attempt to get around the fact that we can't actually give 
 * argument lists to commands.  Respect their bracketing, but treat unbracketed
 * applications as successive arguments
 *)

let parsecurriedarglist _ =
  if canstartTerm (currsymb ()) then
    let rec flatten a1 a2 =
      match a1, a2 with
        App (_, f, a), rs -> flatten f (a :: rs)
      | t, rs -> t :: rs
    in
    flatten (parseTerm (KET ")")) []
  else []

let tryparse _R s =
  let s = pushlex "" (stream_of_utf8string s) in
  let r = 
    (try (let r = _R EOF in check EOF; r) with exn -> poplex s; raise exn)
  in poplex s; r

let tryparse_dbug _R p s =
  let lex_s = pushlex "" (stream_of_utf8string s) in
  let r =
    (try
       let _ = consolereport ["tryparse_dbug \""; s; "\""] in
       let r = _R EOF in
       consolereport ["tryparse_dbug found "; p r];
       check EOF;
       consolereport ["tryparse_dbug EOF ok"];
       r
     with
       exn -> poplex lex_s; raise exn)
   in poplex lex_s; r

(* hack to allow negative numerals in tactic strings *)
let try_negnum s =
  if string_of_symbol s = "-" && 
       (match currsymb() with NUM s -> true | _ -> false) then
     registerApp(registerId(vid_of_string (string_of_symbol s), NoClass), parseAtom())
   else
     badstart s
  
let asTactic f a =
  let old_undecl = !undecl in
  let old_unvar = !unvar in
  let old_unclass = !unclass in
  let old_unstart = !unstart in
  let cleanup () = undecl := old_undecl; unvar := old_unvar; 
                   unclass := old_unclass; unstart := old_unstart 
  in
  undecl := (fun _ t -> t);
  unvar := (fun x -> x);
  unclass := (fun _ t -> t);
  unstart := try_negnum;
  let r =  (try f a with exn -> cleanup (); raise exn)
  in cleanup (); r

let checkTacticTerm t =
  let bang reason t = raise (Tacastrophe_ [reason; " "; string_of_term t])
  in
  let badvar =
    function
      Id (_, _, class__) as t ->
        if class__ <> VariableClass then
          bang "non-variable in substitution" t
        else None
    | Unknown (_, _, class__) as t ->
        if class__ <> VariableClass then
          bang "non-variable in substitution" t
        else None
    | _ -> None
  in
  let badterm =
    function
      Id (_, _, class__) as t ->
        if class__ = NoClass then bang "undeclared variable" t else None
    | Unknown (_, _, class__) as t ->
        if class__ = NoClass then bang "undeclared variable" t else None
    | Subst (_, _, _, vts) -> findfirst badvar ((fst <* vts))
    | _ -> None
  in
  let _ = findterm badterm t in
  ()

let term_of_string s = tryparse parseTerm s

let tactic_of_string s = tryparse (asTactic parseTerm) s
