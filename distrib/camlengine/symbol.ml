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

(* I've made this a bit of a mess - all confused between the enter/lookup store and
 * the operator and id prefix search trees.  Still, it will do for a temporary lashup.
 * The problem is that the enter/lookup store preserves the thing that you first put into
 * it - so that if you find 'shift' say, it looks like an ID but you have already entered
 * it as an operator, when you do checkentry you see the operator definition.  And that is
 * as it should be.  But when you first see an identifier, it is likely to be in a prefix
 * declaration: you don't want that first sight to put you off.  So I don't enter IDs with
 * None in the idclass position, and I never enter UNKNOWNs at all.
 *)
 
open Symboltype

open Idclass
open Searchtree
open Prestring
open Listfuns
open Miscellaneous
open Optionfuns
open Stringfuns
open Sml
open UTF

type idclass = Idclass.idclass
 and associativity = Symboltype.associativity
 and symbol = Symboltype.symbol

(* smlnj 0.93 had no notion of char, only single-character strings.  In this first
   porting, I've used caml streams and converted all input to single-character strings.
   RB
 *)
(* and that's lucky, because it means I can deal in utf8 items -- strings representing a
   unicode character.
 *)
open Stream

let symboldebug = ref false
let appfix_default = 10000
let substfix_default = 20000
let substsense_default = true
let appfix = ref appfix_default
let substfix = ref substfix_default
let substsense = ref substsense_default

(* to make test parses silent, parsing functions raise ParseError_,
 * which you can catch and translate into a call of showInputError 
 * if you want to.  RB
 *)

(* previously, charpred applied to a string got the first character out of the string and
   tested it. Not any longer ... so we have to be careful about all the charpred functions 
   -- isdigit, islcletter, isletter, isucletter, isIDhead, isIDtail, isreserved, reservedpunct
   -- and the things that are derived from them (sigh) like ispunct
 *)
 
let (isIDhead, updateIDhead) = charpred "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'"
let (isIDtail, updateIDtail) = charpred "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'0123456789"

let decIDheads : string list ref = ref []
let decIDtails : string list ref = ref []

let (isreserved, _) = charpred "\"_() \n\t" (* the chars we give special meaning to *)
(* we can't use a list of the chars we allow as PUNCT because that kills 8-bit fonts *)
(* recently deleted from isreserved: "$,[]" *)

let metachar = "_"   (* not a punct *)

(* this could be quicker, could it not? *)
let rec ispunct c =
  not (c = "" || c = metachar || isdigit c || isIDhead c || isreserved c)

let (reservedpunct, _) = charpred "(),[]"

(* for fast-ish lookup of declared operators and identifiers, and for some error reporting *)
let rec mkalt cts def =
  let aa = Hashtbl.create (List.length cts * 2) in
  let rec lookup c = try Hashtbl.find aa c with Not_found -> def in
  List.iter (fun (c, t) -> Hashtbl.add aa c t) cts; lookup

let optree : (string, symbol) searchtree ref = ref (emptysearchtree mkalt)
let idprefixtree : (string, idclass) searchtree ref =
  ref (emptysearchtree mkalt)
let idfixedtree : (string, idclass) searchtree ref =
  ref (emptysearchtree mkalt)

(* list of 'funny symbols' to be used in on-screen keyboards and the like *)
let oplist : string list option ref = ref None

let rec get_oplist () =
  match !oplist with
    Some ss -> ss
  | None ->
      let ops =
        List.map (implode <.> (fun(r,_,_)->r)) (summarisetree !optree)
      in
      sortunique (<) ((ops @ !decIDheads) @ !decIDtails)

let rec set_oplist ss = oplist := Some ss

(* and similar stuff for prefixes of variables  -
 * it would be possible to do the whole thing this way, but it would mean 
 * backtracking, so for the moment who cares ...
 *)
let rec lookinIdtree rt cs =
  match searchfsm (rootfsm rt) cs with
    Found (res, _) ->
      if !symboldebug then
        consolereport [implode cs; " class "; idclassstring res];
      res
  | NotFound _ ->
      if !symboldebug then
        begin
          List.iter
            (fun (cs', r, b) ->
               if b && isprefix (fun (x, y) -> x = y) cs' cs then
                 consolereport
                   ["missed prefix "; implode cs'; " "; idclassstring r])
            (summarisetree !rt);
          consolereport [implode cs; " class "; idclassstring NoClass]
        end;
      NoClass

let rec decIDhead c =
  isIDhead c ||
  not (isdigit c || isreserved c) &&
  (if !symboldebug then
     consolereport ["decIDhead "; enCharQuote (pre_Ascii c)];
   updateIDhead (c, true); decIDheads := c :: !decIDheads; true)

let rec decIDtail c =
  isIDtail c ||
  not (isreserved c) &&
  (if !symboldebug then
     consolereport ["decIDtail "; enCharQuote (pre_Ascii c)];
   updateIDtail (c, true); decIDtails := c :: !decIDtails; true)

(* when we explode a string, we must make it a sequence of utf8 items *)

let insertinIdtree what isprefix class__ tree s =
  let rec bang () =
    raise
      (Catastrophe_
         ["attempt to "; what; " "; idclassstring class__; " \""; s;
          "\""])
  in
  match utf8_explode s with
    [] -> bang ()
  | c :: cs ->
      if decIDhead c then () else bang ();
      List.iter (fun c -> if decIDtail c then () else bang ()) cs;
      (* may be a bad thing *)
      if !symboldebug then
        consolereport
          ["insertinIdtree "; idclassstring class__; " ";
           string_of_bool isprefix; " "; enQuote (pre_Ascii s)];
      tree :=
        addtotree (fun (x, y) -> x = y) !tree (c :: cs, class__, isprefix)

let friendlyLargeishPrime = 1231
let symboltable = (* ref (Store.new__ friendlyLargeishPrime) *)
                  Hashtbl.create friendlyLargeishPrime
let lookup string = (* Store.at (!symboltable, string) *)
   try Some (Hashtbl.find symboltable string) with Not_found -> None

let reversemapping (* : (symbol, string) Mappingfuns.mapping ref = ref Mappingfuns.empty *)
                  = Hashtbl.create friendlyLargeishPrime

exception Symclass_ of string
(* not spurious *)
  
let rec symclass s =
  match lookup s with
    Some (ID (_, Some class__)) -> class__
  | Some (PREFIX _) -> OperatorClass
  | Some (POSTFIX _) -> OperatorClass
  | Some (INFIX _) -> OperatorClass
  | Some (INFIXC _) -> OperatorClass
  | Some _ -> raise (Symclass_ s)
  | None -> lookinIdtree idprefixtree (utf8_explode s)

let rec reverselookup symbol =
  match symbol with
    ID (s, _) -> s
  | UNKNOWN (s, _) -> s
  | NUM s -> s
  | STRING s -> s
  | BRA s -> s
  | SEP s -> s
  | KET s -> s
  | EOF -> ""
  | PREFIX (_, s) -> s
  | POSTFIX (_, s) -> s
  | INFIX (_, _, s) -> s
  | INFIXC (_, _, s) -> s
  | LEFTFIX (_, s) -> s
  | MIDFIX (_, s) -> s
  | RIGHTFIX (_, s) -> s
  | STILE s -> s
  | SHYID s -> s
  | _ -> Hashtbl.find reversemapping symbol
let rec preclassopt =
  function
    Some c -> Prestrs ["Some("; idclassstring c; ")"]
  | None -> Prestrs ["None"]
let rec pre_assoc a =
  match a with
    LeftAssoc      -> Prestr "LeftAssoc"
  | RightAssoc     -> Prestr "RightAssoc"
  | AssocAssoc     -> Prestr "AssocAssoc"
  | TupleAssoc     -> Prestr "TupleAssoc"
  | CommAssocAssoc -> Prestr "CommAssocAssoc"

let rec pre_SYMBOL s =
  match s with
    ID (s'1, class__) ->
      Prepres
        [Prestr "ID("; Prestr "\""; Prestr s'1; Prestr "\""; pre__comma;
         preclassopt class__; Prestr ")"]
  | UNKNOWN (s'1, class__) ->
      Prepres
        [Prestr "UNKNOWN("; Prestr "\""; Prestr s'1; Prestr "\"";
         pre__comma; preclassopt class__; Prestr ")"]
  | NUM s'1 -> Prepres [Prestr "NUM "; pre_string s'1]
  | STRING s'1 -> Prepres [Prestr "STRING \""; Prestr s'1; Prestr "\""]
  | BRA s'1 -> Prestrs ["BRA \""; s'1; "\""]
  | SEP s'1 -> Prestrs ["SEP \""; s'1; "\""]
  | KET s'1 -> Prestrs ["KET \""; s'1; "\""]
  | SUBSTBRA -> Prestrs ["SUBSTBRA"; (try " \"" ^ reverselookup SUBSTBRA ^ "\"" with Not_found -> "")]
  | SUBSTSEP -> Prestrs ["SUBSTSEP"; (try " \"" ^ reverselookup SUBSTSEP ^ "\"" with Not_found -> "")]
  | SUBSTKET -> Prestrs ["SUBSTKET"; (try " \"" ^ reverselookup SUBSTKET ^ "\"" with Not_found -> "")]
  | EOF -> Prestr "EOF"
  | PREFIX (s'1, s'2) ->
      Prepres
        [Prestr "PREFIX ("; pre_int s'1; pre__comma; Prestr "\"";
         Prestr s'2; Prestr "\")"]
  | POSTFIX (s'1, s'2) ->
      Prepres
        [Prestr "POSTFIX ("; pre_int s'1; pre__comma; Prestr "\"";
         Prestr s'2; Prestr "\")"]
  | INFIX (s'1, s'2, s'3) ->
      Prepres
        [Prestr "INFIX("; pre_int s'1; pre__comma; pre_assoc s'2;
         pre__comma; Prestr "\""; Prestr s'3; Prestr "\")"]
  | INFIXC (s'1, s'2, s'3) ->
      Prepres
        [Prestr "INFIXC("; pre_int s'1; pre__comma; pre_assoc s'2;
         pre__comma; Prestr "\""; Prestr s'3; Prestr "\")"]
  | LEFTFIX (s'1, s'2) ->
      Prepres
        [Prestr "LEFTFIX("; pre_int s'1; pre__comma; Prestr "\"";
         Prestr s'2; Prestr "\")"]
  | MIDFIX (s'1, s'2) ->
      Prepres
        [Prestr "MIDFIX("; pre_int s'1; pre__comma; Prestr "\"";
         Prestr s'2; Prestr "\")"]
  | RIGHTFIX (s'1, s'2) ->
      Prepres
        [Prestr "RIGHTFIX("; pre_int s'1; pre__comma; Prestr "\"";
         Prestr s'2; Prestr "\")"]
  | STILE s'1 -> Prestrs ["STILE \""; s'1; "\""]
  | SHYID s'1 -> Prestrs ["RESERVED-WORD "; s'1]

let smlsymbolstring = pre_Ascii <.> pre_implode <.> pre_SYMBOL

let symbolstring = reverselookup

let rec register_op s sym =
  let rec bang s =
    raise (Catastrophe_ ["("; s; ") attempt to register_op \""; s; "\""])
  in
  if s = "" then bang "is_EOF";
  let cs = utf8_explode s in
  if List.exists (not <.> ispunct) cs then 
    bang ("notallpunct "^bracketedliststring (enQuote<.>pre_Ascii) ";" cs);
  if !symboldebug then
    consolereport
      ["register_op "; enQuote (pre_Ascii s); " "; smlsymbolstring sym];
  optree := addtotree (fun (x, y) -> x = y) !optree (cs, sym, false)

let rec deregister_op s sym =
  if !symboldebug then
    consolereport ["deregister_op "; enQuote s; " "; smlsymbolstring sym];
  optree := deletefromtree (fun (x, y) -> x = y) !optree (utf8_explode s, sym, false)

let rec enter (string, symbol) =
  let rec doit () =
    let isop s = ispunct (utf8_sub s 0) in
    let hidden =
      match symbol with
        SUBSTBRA -> true
      | SUBSTKET -> true
      | SUBSTSEP -> true
      | _ -> false
    in
    if !symboldebug then
      consolereport ["enter("; enQuote (pre_Ascii string); ","; smlsymbolstring symbol; ")"];
    (try
       let oldstring = reverselookup symbol in
       if isop oldstring && 
          (match searchfsm (rootfsm optree) (utf8_explode oldstring) with Found _ -> true | NotFound _ -> false)
       then 
         (try deregister_op oldstring symbol 
          with DeleteFromTree_ -> 
            let string_of_csrb =
              triplestring (bracketedliststring (enQuote <.> String.escaped) ",") smlsymbolstring string_of_bool ","
            in
            consolereport 
              ["DeleteFromTree_\n\t"; 
                 pairstring (bracketedliststring string_of_csrb "; ") string_of_csrb "\n\t" 
                    (summarisetree !optree, (utf8_explode oldstring, symbol, false))]
         );
       if hidden then (* Store.delete (!symboltable, oldstring) *)
                      Hashtbl.remove symboltable oldstring
     with Not_found -> ());
    (* Store.update (!symboltable, string, symbol) *)
    Hashtbl.add symboltable string symbol;
    if hidden then
      Hashtbl.add reversemapping symbol string;
    if isop string then register_op string symbol
  in
  match symbol with
    ID (_, None) -> ()
  | UNKNOWN _ -> ()
  | _ -> doit ()

(* function provided so that other functors don't need to know how operators are
 * represented
 *)
let rec lookupassoc string =
  match lookup string with
    Some (INFIXC (_, a, _)) -> Some (true, a)
  | Some (INFIX (_, a, _)) -> Some (false, a)
  | _ -> None

let rec badID s = ID (s, None)

let rec checkentry con s =
  let v = con s in
  match lookup s with
    None -> enter (s, v); v
  | Some v -> v

let checkbadID = checkentry badID

let rec carefullyEnter (s, t) =
  match lookup s with
    None -> enter (s, t)
  | Some (ID _) -> enter (s, t)
  | Some other ->
      if other = t then ()
      else
        raise
          (ParseError_
             ["Attempt to redefine the syntactic role of Prestrs"; s;
              "'' to "; smlsymbolstring t])

let decVarPrefixes : (idclass, string) Mappingfuns.mapping ref =
  ref Mappingfuns.empty

let rec declareIdPrefix class__ s =
  begin match Mappingfuns.(<@>) !decVarPrefixes class__ with
    None ->
      decVarPrefixes :=
        Mappingfuns.(++) !decVarPrefixes (Mappingfuns.(|->) class__ s)
  | Some _ -> ()
  end;
  insertinIdtree "declareIdPrefix" true class__ idprefixtree s;
  (* no warnings about clashes any more
     case fsmpos (rootfsm idfixedtree) (utf8_explode s) of
       Some t => List.map ((fn t => s^implode t) o #1) (summarisetree t)
     | None   => []
  *)
  []

let rec autoID class__ prefix =
  match Mappingfuns.(<@>) !decVarPrefixes class__ with
    Some s -> s
  | None ->
      (* we just add underscores to prefix till it isn't in the IdPrefix tree *)
      match fsmpos (rootfsm idprefixtree) (utf8_explode prefix) with
        None   -> (let _ = declareIdPrefix class__ prefix in prefix)
      | Some _ -> autoID class__ (prefix ^ "_")

let rec declareIdClass class__ s =
  insertinIdtree "declareIdClass" false class__ idfixedtree s;
  enter (s, ID (s, Some class__));
  (* no warnings about clashes any more *)
  None

let rec isnumber s =
  not (List.exists (not <.> isdigit) (utf8_explode s))

let rec isextensibleID s =
  lookup s = None && lookinIdtree idprefixtree (utf8_explode s) <> NoClass

let commasymbol = INFIX (0, TupleAssoc, ",") (* comma is now an operator, and may we be lucky *)

let rec resetSymbols () =
  let enterclass f s = enter (s, f s) in
  let debug = !symboldebug in
  symboldebug := false;
  (* symboltable := Store.new__ friendlyLargeishPrime *)
  Hashtbl.clear symboltable;
  (* reversemapping := Mappingfuns.empty *)
  Hashtbl.clear reversemapping;
  if !symboldebug then consolereport ["about to make optree"];
  optree := emptysearchtree mkalt;
  oplist := None;
  if !symboldebug then  consolereport ["clearing IDheads"];
  List.iter (fun c -> updateIDhead (c, false)) !decIDheads;
  decIDheads := [];
  List.iter (fun c -> updateIDtail (c, false)) !decIDtails;
  decIDtails := [];
  idprefixtree := emptysearchtree mkalt;
  idfixedtree := emptysearchtree mkalt;
  decVarPrefixes := Mappingfuns.empty;
  if !symboldebug then  consolereport ["defining SHYIDs"];
  List.iter (enterclass (fun s -> SHYID s))
    ["ABSTRACTION"; "AND"; "ARE"; "AUTOMATCH"; "AUTOUNIFY"; 
     "BAG"; "BIND"; "BUTTON"; 
     "CHECKBOX"; "CHILDREN"; "CLASS"; "COMMAND";
     	"CONCFRESH"; "CONCHIT"; "CONJECTUREPANEL"; "CONSTANT";
     	"CURRENTPROOF"; "CUT"; 
     "DERIVED"; "DISPROOF"; (* keep this for a year or so. RB 15.xi.2002 *)
     "END"; "ENTRY";
     "FONTS"; "FORCEDEF"; "FORMULA"; "FORMULAE"; "FRESH"; "FROM"; 
     "GIVENPANEL"; 
     "HYPFRESH"; "HYPHIT"; 
     "IDENTITY"; "IMPCONCFRESH"; "IMPFRESH"; "IMPHYPFRESH"; "IN"; "INFER";
     	"INFIX"; "INFIXC"; "INITIALLY"; "INITIALISE"; "IS"; 
     "JUXTFIX";
     "KEYBOARD"; 
     "LABEL"; "LABELS"; "LEFTFIX"; "LEFTWEAKEN"; "LIST";
     "MACRO"; "MENU"; "MENUKEY"; "MIDFIX"; 
     "NOTIN"; "NOTONEOF"; "NUMBER"; 
     "OBJECT"; "OUTFIX"; 
     "POSTFIX"; "PATCHALERT"; "PREFIX"; "PROOF"; 
     "RADIOBUTTON"; "REFLEXIVE"; "RIGHTFIX"; "RIGHTWEAKEN"; "RULE"; "RULES"; 
     "SCOPE"; "SEMANTICS"; "SEMANTICTURNSTILE"; "SEPARATOR"; "SEQUENT"; 
     	"STRING"; "STRUCTURERULE"; "SUBSTFIX";
     "TACTIC"; "TACTICPANEL"; "THEOREM";
     "THEOREMS"; "THEORY"; "TRANSITIVE"; 
     "UMENU"; "UNIFIESWITH"; "USE"; 
     "VARIABLE"; "VIEW"; 
     "WEAKEN"; "WHERE"; "WORLD"];
  if !symboldebug then  consolereport ["defining BRA"];
  enter ("(", BRA "(");
  (* these two still need special treatment ... *)
  if !symboldebug then  consolereport ["defining KET"];
  enter (")", KET ")");
  enter ("[", SUBSTBRA);
  enter ("\\", SUBSTSEP);
  enter ("]", SUBSTKET);
  enter ("", EOF);
  if !symboldebug then  consolereport ["defining commasymbol"];
  enter (",", commasymbol);
  appfix := appfix_default;
  substfix := substfix_default;
  substsense := substsense_default;
  symboldebug := debug

(* probably this can all be simply camlised *)
let rec escapechar c =
  match c with
    "n" -> "\n"
  | "r" -> "\r"
  | "t" -> "\t"
  (* next two lines not strictly necessary, but worth it for notification value *)
  | "\"" -> "\""
  | "\\" -> "\\"
  | c    -> c

let rec unescapechar c =
  match c with
    "\n" -> "\\n"
  | "\t" -> "\\t"
  | "\"" -> "\\\""
  | "\\" -> "\\\\"
  | "\r" -> "\\r"
  | c    -> c

(* I collected all these things into one place, and wrapped them in local, 
 * because these variables form all of the state of the lexer.  
 * Then you can properly set and reset the state, as in tryparse and 
 * other places, with pushlex and poplex.
 * RB 14/xii/94
 *)
(* but then translation to ocaml delocalised them, and I didn't yet realise how to 
   undo that. RB 15/vii/2002
 *)
let lexin = ref (of_channel stdin)
let lexinfile = ref ""
let linenum = ref 0
let symb = ref EOF
let peekedsymb : symbol list ref = ref []

let rec showInputError f msg =
  let loc =
    if !lexinfile = "" then []
    else [!lexinfile; " (line "; string_of_int !linenum; "): "]
  in
  f (loc @ msg)

(* get utf8 items; translate 85, 2028 and 2029 into Unix newline *)
let char () = match utf8_peek !lexin with 
                  Some s -> if s="\xc2\x85" || s="\xe2\x80\xa8" || s="\xe2\x80\xa9" then "\n" else s
                | None   -> "" 

let next () = utf8_junk !lexin

let rec scanwhile =
  fun pp rcs con ->
    let c = char () in
    if c <> "" && pp c then (next (); scanwhile pp (c :: rcs) con)
    else con (implode (List.rev rcs))

let rec scanwatch f =
  if !symboldebug then
    fun v ->
      let c = f v in if !symboldebug then consolereport ["scanfsm '"; pre_Ascii c; "'"]; c
  else f

let rec scannext () = next (); char ()

let rec scanop fsm rcs =
  if !symboldebug then 
    consolereport ["scanop "; bracketedliststring (fun c -> enCharQuote (pre_Ascii c)) ";" rcs];
  match scanfsm (scanwatch scannext) fsm rcs (scanwatch char ()) with
    Found (sy, _) -> sy
  | NotFound rcs' ->
      if rcs' = [] then let c = char () in next (); checkbadID c
      else checkbadID (implode (List.rev rcs')) (* at this point we need backtracking ... *)
                  
let rec scanid conf =
  let rec q rcs classopt =
    scanwhile isIDtail
      (if rcs=[] then (let r = [char ()] in next (); r) else rcs)
      (conf classopt)
  in
  if !symboldebug then 
    consolereport ["scanid "];
  match
    scanfsm (scanwatch scannext) (rootfsm idprefixtree) [] (scanwatch char ())
  with
    Found (class__, rcs) -> q rcs (Some class__)
  | NotFound rcs -> q rcs None

let rec scanreport s =
  if !symboldebug then begin consolereport [smlsymbolstring s]; s end
  else s

let rec checkidclass con takeit class__ s =
  match lookup s with
    Some (ID (v, class')) -> con (v, class')
  | Some sy -> if takeit then sy else con (s, class__)
  | None -> con (s, class__)

let rec scan () =
  match char () with
    ""   -> scanreport EOF
  | " "  -> next (); scan ()
  | "\t" -> next (); scan ()
  | "\n" -> incr linenum; next (); scan ()
  | "("  -> next (); scanreport (BRA "(")
  | ")"  -> (* necessary ? - I think so *)
     next (); scanreport (KET ")")
  | "/" ->
      (* "/" must be a punct *)
      next ();
      begin match char () with
        "*" ->
          next ();
          scanComment (0, !linenum);
          if !symboldebug then consolereport ["... comment ..."];
          scan ()
      | c ->
          scanreport
            (match fsmpos (rootfsm optree) ["/"] with
               Some t -> scanop t ["/"]
             | None -> checkbadID "/")
      end
  | "\"" -> next (); scanreport (scanString !linenum [])
  | c ->
      if c = metachar then
        let rec goodunknown class__ s =
          if isextensibleID s then 
            checkidclass (fun sc->UNKNOWN sc) false class__ s
          else 
            raise (ParseError_ ["non-CLASS unknown "; metachar; s])
        in
        next (); 
        if isIDhead (char ()) then
          scanreport (scanid (checkidclass (fun sc->UNKNOWN sc) false))
        else raise (ParseError_ ["ID expected following "; metachar])
      else if isIDhead c then scanreport (scanid (checkidclass (fun sc->ID sc) true))
      else if isdigit c then scanreport (scanwhile isdigit [] (fun s->NUM s))
      else if ispunct c then scanreport (scanop (rootfsm optree) [])
      else raise (Catastrophe_ ["scan can't see class of "; c])

and scanComment (n, k) =
  match char () with
    "" ->
      raise
        (ParseError_
           ["End of file inside level "; string_of_int n;
            " comment starting on line "; string_of_int k])
  | "\n" -> incr linenum; next (); scanComment (n, k)
  | "*" ->
      next (); 
      begin match char () with
        "/" -> next ()
      | "\n" -> incr linenum; next (); scanComment (n, k)
      | _ -> scanComment (n, k)
      end
  | "/" ->
      next (); 
      begin match char () with
        "*" ->
          next (); 
          begin try scanComment (n + 1, !linenum) with
            ParseError_ ss ->
              raise
                (ParseError_
                   (ss @
                      [", within a level "; string_of_int n;
                       " comment starting on line"; string_of_int k]))
          end;
          scanComment (n, k)
      | "\n" -> incr linenum; next (); scanComment (n, k)
      | _ -> scanComment (n, k)
      end
  | _ -> next (); scanComment (n, k)

and scanString k rcs =
  match (let r = char () in next (); r) with
    "" ->
      raise
        (ParseError_
           ["end of file inside string starting on line "; string_of_int k])
  | "\n" -> raise (ParseError_ ["end of line inside string"])
  | "\"" -> STRING (implode (List.rev rcs))
  | "\\" ->
      if char () = "\n" then
        begin
          incr linenum;
          next ();
          while
            match char () with
              " " -> true
            | "\t" -> true
            | "\n" -> incr linenum; true
            | _ -> false
          do next ()
          done;
          if char () = "\\" then next ()
          else
            raise
              (ParseError_
                 ["expected \\ to restart broken string starting on line ";
                  string_of_int k]);
          scanString k rcs
        end
      else scanString k (escapechar (let r = char () in next (); r) :: rcs)
  | c -> scanString k (c :: rcs)

let showInputError = showInputError

let rec scansymb () =
  if !symboldebug then
    consolereport ["scansymb -- peekedsymb is "; bracketedliststring smlsymbolstring ";" !peekedsymb];
  match !peekedsymb with
    [] -> symb := scan ()
  | sym :: more -> symb := sym; peekedsymb := more

let rec peeksymb () =
  match !peekedsymb with
    [] -> let sym = scan () in peekedsymb := [sym]; sym
  | sym :: _ -> sym

let rec currsymb () = !symb

let rec putbacksymb s = peekedsymb := !symb :: !peekedsymb; symb := s

let rec canstartnovelsymb sy =
  match sy with
    UNKNOWN _ -> false
  | NUM _     -> false
  | STRING _  -> false
  | EOF       -> false
  | SHYID _   -> false
  | _         -> not (isreserved (utf8_sub (reverselookup sy) 0))

let rec currnovelsymb () =
  if !symboldebug then
    consolereport ["currnovelsymb "; smlsymbolstring !symb];
  if canstartnovelsymb !symb then
    let symstr = reverselookup !symb in
    let cs = utf8_explode symstr in
    (* used to be ...
    if startspunct symstr then
       case !peekedsymb of
         [] => (symb := scanwhile ispunct (List.rev cs) checkbadID; 
                !symb
               )
       | _    => raise Catastrophe_ ["peeksymb(); currnovelsymb()"]
    else !symb
    *)
    match !peekedsymb with
      [] ->
        symb :=
          scanwhile (not <.> isreserved) (List.rev cs) checkbadID;
        !symb
    | _ -> raise (Catastrophe_ ["peeksymb(); currnovelsymb()"])
  else
    raise
      (ParseError_
         [symbolstring !symb;
          " can't start a new identifier or operator"])

type savedlex = char Stream.t * string * int * symbol * symbol list

let rec pushlex name newstream =
  let state = !lexin, !lexinfile, !linenum, !symb, !peekedsymb in
  lexin := newstream;
  lexinfile := name;
  linenum := 1;
  peekedsymb := [];
  let _ = scansymb () in 
  state

let rec poplex (lxin, lxinf, lnum, sy, pksy) =
  (* close_in !lexin; -- now user's responsibility *)
  lexin := lxin;
  lexinfile := lxinf;
  linenum := lnum;
  symb := sy;
  peekedsymb := pksy

(* some aid for pretty-printers *)

type charclass = LETTER | PUNCT | RESERVEDPUNCT | SPACE | OTHER

let rec charclass =
  function
    " " -> SPACE
  | c   ->
      if isIDtail           c then LETTER
      else if ispunct       c then PUNCT
      else if reservedpunct c then RESERVEDPUNCT
      else                         OTHER

let rec mustseparate =
  function
    "", _  -> false
  | _ , "" -> false
  | a1, a2 ->
      let c1 = utf8_presub a1 (String.length a1) in
      let c2 = utf8_sub a2 0 in
      let rec msp c =
        opt2bool (fsmpos (rootfsm optree) (utf8_explode a1) &~~ (fun t -> fsmpos t [c]))
      in
      let ms =
        function
          SPACE        , _             -> false
        | _            , SPACE         -> false
        | PUNCT        , _             -> msp c2
        | _            , PUNCT         -> false
        | RESERVEDPUNCT, _             -> false
        | _            , RESERVEDPUNCT -> false
        | _            , _             -> true
      in
      ms (charclass c1, charclass c2)

let enter = carefullyEnter

let lookup = checkbadID

let _ = (try resetSymbols () 
         with Catastrophe_ ss ->
                consolereport ["resetSymbols raised Catastrophe_ \""; implode ss; "\""]
         |    exn -> 
                consolereport ["resetSymbols raised "; Printexc.to_string exn]
        )

