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
(* and then I thought "how big's a string?" and I realised it ought to be dealing with
   unicode code points. So it does.
 *)
open Stream

let symboldebug = ref false
let appfix_default = 10000
let substfix_default = 20000
let substsense_default = true
let appfix = ref appfix_default
let substfix = ref substfix_default
let substsense = ref substsense_default

type synfixrec = {
   appF   : int;
   substF : int;
   substS : bool
}

type syntabrec = {
   symT    : (string, symbol) Hashtbl.t;
   revT    : (symbol, string) Hashtbl.t;
   prioT   : (symbol, int) Hashtbl.t;
   assocT  : (symbol, associativity) Hashtbl.t;
   decIDhs : ucode list;
   decIDts : ucode list;
   idprefT : (ucode, idclass) searchtree;
   idfixT  : (ucode, idclass) searchtree;
   opT     : (ucode, symbol) searchtree;
   ops     : string list option;
   decVs   : (idclass, string) Mappingfuns.mapping
}

let (syntaxes : (string * synfixrec * syntabrec) list ref) = ref []
  
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

let decIDheads : int list ref = ref []
let decIDtails : int list ref = ref []

let (isreserved, _) = charpred "\"_() \n\t" (* the chars we give special meaning to *)
(* we can't use a list of the chars we allow as PUNCT because that kills 8-bit fonts *)
(* recently deleted from isreserved: "$,[]" *)

let metachar = Char.code '_'   (* not a punct *)
let metachar_as_string = "_"

(* this could be quicker, could it not? *)
let rec ispunct c =
  not (c = -1 || c = metachar || isdigit c || isIDhead c || isreserved c)

let (reservedpunct, _) = charpred "(),[]"

(* for fast-ish lookup of declared operators and identifiers, and for some error reporting *)
let rec mkalt cts def =
  let aa = Hashtbl.create (List.length cts * 2) in
  let rec lookup c = try Hashtbl.find aa c with Not_found -> def in
  List.iter (fun (c, t) -> Hashtbl.add aa c t) cts; lookup

let optree : (int, symbol) searchtree ref = ref (emptysearchtree mkalt)
let idprefixtree : (int, idclass) searchtree ref =
  ref (emptysearchtree mkalt)
let idfixedtree : (int, idclass) searchtree ref =
  ref (emptysearchtree mkalt)

(* list of 'funny symbols' to be used in on-screen keyboards and the like *)
let oplist : string list option ref = ref None

let rec get_oplist () =
  match !oplist with
    Some ss -> ss
  | None ->
      let ops =
        List.map (utf8_implode <.> (fun(r,_,_)->r)) (summarisetree !optree)
      in
      sortunique (<) (ops @ List.map utf8_of_ucode !decIDheads @ List.map utf8_of_ucode !decIDtails)

let rec set_oplist ss = oplist := Some ss

(* and similar stuff for prefixes of variables  -
 * it would be possible to do the whole thing this way, but it would mean 
 * backtracking, so for the moment who cares ...
 *)
let rec lookinIdtree rt cs =
  match searchfsm (rootfsm rt) cs with
    Found (res, _) ->
      if !symboldebug then
        consolereport [utf8_implode cs; " class "; string_of_idclass res];
      res
  | NotFound _ ->
      if !symboldebug then
        begin
          List.iter
            (fun (cs', r, b) ->
               if b && isprefix (fun (x, y) -> x = y) cs' cs then
                 consolereport
                   ["missed prefix "; utf8_implode cs'; " "; string_of_idclass r])
            (summarisetree !rt);
          consolereport [utf8_implode cs; " class "; string_of_idclass NoClass]
        end;
      NoClass

let rec decIDhead c =
  isIDhead c ||
  not (isdigit c || isreserved c) &&
  (if !symboldebug then
     consolereport ["decIDhead "; enCharQuote (utf8_of_ucode c)];
   updateIDhead (c, true); decIDheads := c :: !decIDheads; true)

let rec decIDtail c =
  isIDtail c ||
  not (isreserved c) &&
  (if !symboldebug then
     consolereport ["decIDtail "; enCharQuote (utf8_of_ucode c)];
   updateIDtail (c, true); decIDtails := c :: !decIDtails; true)

(* when we explode a string, we must make it a sequence of utf8 items *)

let insertinIdtree what isprefix class__ tree s =
  let rec bang () =
    raise
      (Catastrophe_
         ["attempt to "; what; " "; string_of_idclass class__; " \""; s;
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
          ["insertinIdtree "; string_of_idclass class__; " ";
           string_of_bool isprefix; " "; enQuote s];
      tree :=
        addtotree (fun (x, y) -> x = y) !tree (c :: cs, class__, isprefix)

let rec preclassopt =
  function
    Some c -> Prestrs ["Some("; string_of_idclass c; ")"]
  | None -> Prestrs ["None"]

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
  | NUM    s'1 -> Prepres [Prestr "NUM "; pre_string s'1]
  | STRING s'1 -> Prepres [Prestr "STRING \""; Prestr s'1; Prestr "\""]
  | BRA    s'1 -> Prestrs ["BRA \""; s'1; "\""]
  | SEP    s'1 -> Prestrs ["SEP \""; s'1; "\""]
  | KET    s'1 -> Prestrs ["KET \""; s'1; "\""]
  | SUBSTBRA   -> Prestr "SUBSTBRA"
  | SUBSTSEP   -> Prestr "SUBSTSEP"
  | SUBSTKET   -> Prestr "SUBSTKET"
  | EOF        -> Prestr "EOF"
  | PREFIX   s -> Prepres [Prestr "PREFIX "; Prestr "\""; Prestr s; Prestr "\""]
  | POSTFIX  s -> Prepres [Prestr "POSTFIX "; Prestr "\""; Prestr s; Prestr "\""]
  | INFIX    s -> Prepres [Prestr "INFIX "; Prestr "\""; Prestr s; Prestr "\""]
  | INFIXC   s -> Prepres [Prestr "INFIXC "; Prestr "\""; Prestr s; Prestr "\""]
  | LEFTFIX  s -> Prepres [Prestr "LEFTFIX "; Prestr "\""; Prestr s; Prestr "\""]
  | MIDFIX   s -> Prepres [Prestr "MIDFIX "; Prestr "\""; Prestr s; Prestr "\""]
  | RIGHTFIX s -> Prepres [Prestr "RIGHTFIX "; Prestr "\""; Prestr s; Prestr "\""]
  | STILE s'1 ->  Prestrs ["STILE \""; s'1; "\""]
  | SHYID  s'1 -> Prestrs ["RESERVED-WORD "; s'1]

let debugstring_of_symbol = pre_implode <.> pre_SYMBOL

let string_of_associativity a = 
  match a with
    LeftAssoc      -> "LeftAssoc"
  | RightAssoc     -> "RightAssoc"
  | AssocAssoc     -> "AssocAssoc"
  | TupleAssoc     -> "TupleAssoc"
  | CommAssocAssoc -> "CommAssocAssoc"

let friendlyLargeishPrime = 1231
let friendlySmallishPrime = 127
let symboltable = ref (Hashtbl.create friendlyLargeishPrime)
let priotable   = ref (Hashtbl.create friendlySmallishPrime)
let assoctable  = ref (Hashtbl.create friendlySmallishPrime)

let lookup string = try Some (Hashtbl.find !symboltable string) with Not_found -> None
let prio sym = 
  try Hashtbl.find !priotable sym 
  with Not_found -> raise (Catastrophe_ ["Symbol.prio looking up "; debugstring_of_symbol sym])
let assoc sym = 
  try Hashtbl.find !assoctable sym 
  with Not_found -> raise (Catastrophe_ ["Symbol.assoc looking up "; debugstring_of_symbol sym])
  
let reversemapping = ref (Hashtbl.create 10)

exception Symclass_ of string
(* not spurious *)
  
let rec symclass s =
  match lookup s with
    Some (ID (_, Some class__)) -> class__
  | Some (PREFIX _)             -> OperatorClass
  | Some (POSTFIX _)            -> OperatorClass
  | Some (INFIX _)              -> OperatorClass
  | Some (INFIXC _)             -> OperatorClass
  | Some _                      -> raise (Symclass_ s)
  | None                        -> lookinIdtree idprefixtree (utf8_explode s)

let rec reverselookup symbol =
  match symbol with
    ID        (s, _) -> s
  | UNKNOWN   (s, _) -> s
  | NUM            s -> s
  | STRING         s -> s
  | BRA            s -> s
  | SEP            s -> s
  | KET            s -> s
  | EOF              -> ""
  | PREFIX         s -> s
  | POSTFIX        s -> s
  | INFIX          s -> s
  | INFIXC         s -> s
  | LEFTFIX        s -> s
  | MIDFIX         s -> s
  | RIGHTFIX       s -> s
  | STILE          s -> s
  | SHYID          s -> s
  | _                -> Hashtbl.find !reversemapping symbol

let string_of_symbol = reverselookup

let rec register_op s sym =
  let rec bang s =
    raise (Catastrophe_ ["("; s; ") attempt to register_op \""; s; "\""])
  in
  if s = "" then bang "is_EOF";
  let cs = utf8_explode s in
  if List.exists (not <.> ispunct) cs then 
    bang ("notallpunct "^bracketedstring_of_list (enQuote<.>utf8_of_ucode) ";" cs);
  if !symboldebug then
    consolereport
      ["register_op "; enQuote s; " "; debugstring_of_symbol sym];
  optree := addtotree (fun (x, y) -> x = y) !optree (cs, sym, false)

let rec deregister_op s sym =
  if !symboldebug then
    consolereport ["deregister_op "; enQuote s; " "; debugstring_of_symbol sym];
  optree := deletefromtree (fun (x, y) -> x = y) !optree (utf8_explode s, sym, false)

let isop s = ispunct (utf8_sub s 0)

let hidden symbol =
  match symbol with
    SUBSTBRA -> true
  | SUBSTKET -> true
  | SUBSTSEP -> true
  | _ -> false

let delete symbol =
  let doit () = 
    let oldstring = reverselookup symbol in
    if isop oldstring && 
       (match searchfsm (rootfsm optree) (utf8_explode oldstring) with Found _ -> true | NotFound _ -> false)
    then 
      (try deregister_op oldstring symbol 
       with DeleteFromTree_ -> 
         let string_of_csrb =
           string_of_triple (bracketedstring_of_list (fun i -> "0x"^hexstring_of_int i) ",") 
                        debugstring_of_symbol string_of_bool ","
         in
         consolereport 
           ["DeleteFromTree_\n\t"; 
              string_of_pair (bracketedstring_of_list string_of_csrb "; ") string_of_csrb "\n\t" 
                 (summarisetree !optree, (utf8_explode oldstring, symbol, false))]
      );
    if hidden symbol then 
      Hashtbl.remove !reversemapping symbol;
    Hashtbl.remove !symboltable oldstring;
    Hashtbl.remove !priotable symbol; Hashtbl.remove !assoctable symbol
  in
  match symbol with
    ID (_, None) -> ()
  | UNKNOWN _    -> ()
  | _            -> doit ()

let enter string n a symbol =
  let rec doit () =
    if !symboldebug then
      consolereport ["enter "; enQuote string; 
                     " "; string_of_option string_of_int n;
                     " "; string_of_option string_of_associativity a; 
                     " ("; debugstring_of_symbol symbol; ")"];
    (try delete symbol with Not_found -> ());
    (* Store.update (!symboltable, string, symbol) *)
    Hashtbl.add !symboltable string symbol;
    if hidden symbol then
      Hashtbl.add !reversemapping symbol string;
    if isop string then register_op string symbol;
    (match n with Some n -> Hashtbl.add !priotable symbol n | _ -> ());
    (match a with Some a -> Hashtbl.add !assoctable symbol a | _ -> ())
  in
  match n, a, symbol with
    None  , None  , ID (_, None) -> ()
  | None  , None  , UNKNOWN    _ -> ()
  | Some n, None  , PREFIX     _ -> doit()
  | Some n, None  , POSTFIX    _ -> doit()
  | Some n, Some a, INFIX      _ -> doit()
  | Some n, Some a, INFIXC     _ -> doit()
  | Some n, None  , LEFTFIX    _ -> doit()
  | Some n, None  , MIDFIX     _ -> doit()
  | Some n, None  , RIGHTFIX   _ -> doit()
  | None  , None  , _            -> doit()
  | _     , _     , _            ->
      raise (Catastrophe_ ["Symbol.enter ("; string_of_option string_of_int n;
                           ") ("; string_of_option string_of_associativity a; 
                           ") ("; debugstring_of_symbol symbol; ")"])
  
let commasymbol = INFIX "," (* comma is now an operator, and may we be lucky *)

let decVarPrefixes : (idclass, string) Mappingfuns.mapping ref =
  ref Mappingfuns.empty

let enterStdSymbols () =
  enter "("  None     None              (BRA "("); (* these two still need special treatment ... *)
  enter ")"  None     None              (KET ")");
  enter "["  None     None              SUBSTBRA;
  enter "\\" None     None              SUBSTSEP;
  enter "]"  None     None              SUBSTKET;
  enter ""   None     None              EOF;
  enter ","  (Some 0) (Some TupleAssoc) commasymbol;
  appfix     := appfix_default;
  substfix   := substfix_default;
  substsense := substsense_default

let rec resetSymbols () =
  let enterclass f s = enter s (f s) in
  let debug = !symboldebug in
  symboldebug := false;
  (* symboltable := Store.new__ friendlyLargeishPrime *)
  Hashtbl.clear !symboltable;
  Hashtbl.clear !priotable;
  Hashtbl.clear !assoctable;
  (* reversemapping := Mappingfuns.empty *)
  Hashtbl.clear !reversemapping;
  optree := emptysearchtree mkalt;
  oplist := None;
  List.iter (fun c -> updateIDhead (c, false)) !decIDheads;
  decIDheads := [];
  List.iter (fun c -> updateIDtail (c, false)) !decIDtails;
  decIDtails := [];
  idprefixtree := emptysearchtree mkalt;
  idfixedtree := emptysearchtree mkalt;
  decVarPrefixes := Mappingfuns.empty;
  if !symboldebug then  consolereport ["defining SHYIDs"];
  List.iter (fun s -> enter s None None (SHYID s))
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
     "PATCHALERT"; "POPSYNTAX"; "POSTFIX"; "PREFIX"; "PROOF"; "PUSHSYNTAX";
     "RADIOBUTTON"; "REFLEXIVE"; "RIGHTFIX"; "RIGHTWEAKEN"; "RULE"; "RULES"; 
     "SCOPE"; "SEMANTICS"; "SEMANTICTURNSTILE"; "SEPARATOR"; "SEQUENT"; 
     	"STRING"; "STRUCTURERULE"; "SUBSTFIX";
     "TACTIC"; "TACTICPANEL"; "THEOREM";
     "THEOREMS"; "THEORY"; "TRANSITIVE"; 
     "UMENU"; "UNIFIESWITH"; "USE"; 
     "VARIABLE"; "VIEW"; 
     "WEAKEN"; "WHERE"; "WORLD"];
  enterStdSymbols ();
  symboldebug := debug

(* the problem of running one syntax inside another (e.g. a syntax for logical formulae
   inside one for Hoare logic) is problematic. Essentially, the inner syntax should accept
   a subset of what the outer syntax does.
 *)

let get_syntax_tables () =
  { symT    = Hashtbl.copy !symboltable;
    revT    = Hashtbl.copy !reversemapping;
    prioT   = Hashtbl.copy !priotable;
    assocT  = Hashtbl.copy !assoctable;
    decIDhs = !decIDheads; 
    decIDts = !decIDtails;
    idprefT = !idprefixtree;
    idfixT  = !idfixedtree;
    opT     = !optree; 
    ops     = !oplist;
    decVs   = !decVarPrefixes
  }

let set_syntax_tables syntabs =
  symboltable := syntabs.symT; reversemapping := syntabs.revT;
  priotable := syntabs.prioT; assoctable := syntabs.assocT;
  decIDheads := syntabs.decIDhs; decIDtails := syntabs.decIDts;
  idprefixtree := syntabs.idprefT; idfixedtree := syntabs.idfixT;
  optree := syntabs.opT; oplist := syntabs.ops;
  decVarPrefixes := syntabs.decVs
  
(* function provided so that other functors don't need to know how operators are
 * represented
 *)
let rec lookupassoc string =
  match lookup string with
    Some (INFIXC _ as s)  -> Some (true,  assoc s)
  | Some (INFIX  _ as s)  -> Some (false, assoc s)
  | _                     -> None

let badID s = None, None, ID (s, None)

let checkentry con s =
  let n, a, sym = con s in
  match lookup s with
    None     -> enter s n a sym; sym
  | Some sym -> sym

let checkbadID = checkentry badID

let carefullyEnter s n a t =
  let bang_redef other ss =
    raise (ParseError_ (ss @ ["redefine the syntactic role of "; enQuote s;
                              " from "; debugstring_of_symbol other; " to "; debugstring_of_symbol t]))
  in
  let doit () =
    match !syntaxes with
      (sname, _, tbls) :: _ -> 
         let saved_tbls = get_syntax_tables() in
         (try set_syntax_tables tbls;
              match lookup s, t with
                               _ , SUBSTBRA   -> ()
              |                _ , SUBSTKET   -> ()
              |                _ , SUBSTSEP   -> ()
              | Some (PREFIX   _), PREFIX   _ -> ()
              | Some (POSTFIX  _), POSTFIX  _ -> ()
              | Some (INFIX    _), INFIX    _ -> ()
              | Some (INFIXC   _), INFIXC   _ -> ()
              | Some (LEFTFIX  _), LEFTFIX  _ -> ()
              | Some (MIDFIX   _), MIDFIX   _ -> ()
              | Some (RIGHTFIX _), RIGHTFIX _ -> ()
              | Some (SEP      _), SEP      _ -> ()
              | Some (BRA      _), BRA      _ -> ()
              | Some (KET      _), KET      _ -> ()
              | Some (STILE    _), STILE    _ -> ()
              | None             , _          -> 
                  raise
                    (ParseError_
                       ["After PUSHSYNTAX "; enQuote sname;
                        " attempt to define new symbol "; enQuote s; " as "; debugstring_of_symbol t])
              | Some other       , t         ->
                  bang_redef other ["After PUSHSYNTAX "; enQuote sname; " attempt to "]
          with exn -> set_syntax_tables saved_tbls; raise exn);
         set_syntax_tables saved_tbls;
         enter s n a t
    | [] -> enter s n a t
  in  
  match lookup s with
    None        -> doit ()
  | Some (ID _) -> doit ()
  | Some other  ->
      if other = t then () else bang_redef other ["Attempt to "]

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
       Some t => List.map ((fn t => s^utf8_implode t) o #1) (summarisetree t)
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
  enter s None None (ID (s, Some class__));
  (* no warnings about clashes any more *)
  None

let rec isnumber s =
  not (List.exists (not <.> isdigit) (utf8_explode s))

let rec isextensibleID s =
  lookup s = None && lookinIdtree idprefixtree (utf8_explode s) <> NoClass

(* probably this can all be simply camlised *)
let rec escapechar c =
  match c with
    c when c=Char.code 'n' -> Char.code '\n'
  | c when c=Char.code 'r' -> Char.code '\r'
  | c when c=Char.code 't' -> Char.code '\t'
  | c                      -> c

(* I collected all these things into one place, and wrapped them in local, 
 * because these variables form all of the state of the lexer.  
 * Then you can properly set and reset the state, as in tryparse and 
 * other places, with pushlex and poplex.
 * RB 14/xii/94
 *)
(* but then translation to ocaml delocalised them, and I didn't yet realise how to 
   undo that. RB 15/vii/2002
 *)
let lexin = ref utf_stdin
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

let char () = match peek !lexin with 
                Some 0x85   -> Char.code '\n'
              | Some 0x2028 -> Char.code '\n'
              | Some 0x2029 -> Char.code '\n'
              | Some i      -> i
              | None        -> uEOF

let next () = junk !lexin

let rec scanwhile =
  fun pp rcs con ->
    let c = char () in
    if c <> uEOF && pp c then (next (); scanwhile pp (c :: rcs) con)
    else con (utf8_implode (List.rev rcs))

let rec scanwatch f =
  if !symboldebug then
    fun v ->
      let c = f v in 
      if !symboldebug then consolereport ["scanfsm '"; (utf8_of_ucode c); "'"]; 
      c
  else f

let rec scannext () = next (); char ()

let rec scanop fsm rcs =
  if !symboldebug then 
    consolereport 
      ["scanop "; bracketedstring_of_list (fun c -> enCharQuote (utf8_of_ucode c)) ";" rcs];
  match scanfsm (scanwatch scannext) fsm rcs (scanwatch char ()) with
    Found (sy, _) -> sy
  | NotFound rcs' ->
      if rcs' = [] then let c = char () in next (); checkbadID (utf8_of_ucode c)
      else checkbadID (utf8_implode (List.rev rcs')) (* at this point we need backtracking ... *)
                  
let rec scanid conf =
  let rec q rcs classopt =
    scanwhile isIDtail
      (if rcs=[] then (let r = [char ()] in next (); r) else rcs)
      (conf classopt)
  in
  if !symboldebug then 
    consolereport ["scanid "; enQuote (utf8_of_ucode (char()))];
  match
    scanfsm (scanwatch scannext) (rootfsm idprefixtree) [] (scanwatch char ())
  with
    Found (class__, rcs) -> q rcs (Some class__)
  | NotFound rcs -> q rcs None

let rec scanreport s =
  if !symboldebug then begin consolereport [debugstring_of_symbol s]; s end
  else s

let rec checkidclass con takeit class__ s =
  match lookup s with
    Some (ID (v, class')) -> con (v, class')
  | Some sy -> if takeit then sy else con (s, class__)
  | None -> con (s, class__)

let rec scan () =
  match char () with
    eof   when eof  =uEOF            -> scanreport EOF
  | space when space=Char.code ' '   -> next (); scan ()
  | tab   when tab  =Char.code '\t'  -> next (); scan ()
  | nl    when nl   =Char.code '\n' -> incr linenum; next (); scan ()
  | bra   when bra  =Char.code '('  -> next (); scanreport (BRA "(")
  | ket   when ket  =Char.code ')'  -> (* necessary ? - I think so *)
     next (); scanreport (KET ")")
  | slash when slash=Char.code '/' ->
      (* "/" must be a punct *)
      next ();
      begin match char () with
        c when c=Char.code '*' ->
          next ();
          scanComment (0, !linenum);
          if !symboldebug then consolereport ["... comment ..."];
          scan ()
      | c ->
          scanreport
            (match fsmpos (rootfsm optree) [slash] with
               Some t -> scanop t [slash]
             | None -> checkbadID (utf8_of_ucode slash))
      end
  | dquote when dquote=Char.code '"' -> next (); scanreport (scanString !linenum [])
  | c ->
      if c = metachar then
        let rec goodunknown class__ s =
          if isextensibleID s then 
            checkidclass (fun sc->UNKNOWN sc) false class__ s
          else 
            raise (ParseError_ ["non-CLASS unknown "; utf8_of_ucode metachar; s])
        in
        next (); 
        if isIDhead (char ()) then
          scanreport (scanid (checkidclass (fun sc->UNKNOWN sc) false))
        else raise (ParseError_ ["ID expected following "; utf8_of_ucode metachar])
      else if isIDhead c then scanreport (scanid (checkidclass (fun sc->ID sc) true))
      else if isdigit c then scanreport (scanwhile isdigit [] (fun s->NUM s))
      else if ispunct c then scanreport (scanop (rootfsm optree) [])
      else raise (Catastrophe_ ["scan can't see class of "; utf8_of_ucode c])

and scanComment (n, k) =
  match char () with
    eof when eof=uEOF ->
      raise
        (ParseError_
           ["End of file inside level "; string_of_int n;
            " comment starting on line "; string_of_int k])
  | nl    when nl   =Char.code '\n' -> incr linenum; next (); scanComment (n, k)
  | star  when star =Char.code '*'  ->
      next (); 
      begin match char () with
        slash when slash=Char.code '/'  -> next ()
      | nl    when nl   =Char.code '\n' -> incr linenum; next (); scanComment (n, k)
      | _                               -> scanComment (n, k)
      end  
  | slash when slash=Char.code '/' ->
      next (); 
      begin match char () with
        star when star=Char.code '*' ->
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
      | nl when nl=Char.code '\n' -> incr linenum; next (); scanComment (n, k)
      | _                         -> scanComment (n, k)
      end
  | _ -> next (); scanComment (n, k)

and scanString k rcs =
  match (let r = char () in next (); r) with
    eof when eof=uEOF  ->
      raise
        (ParseError_
           ["end of file inside string starting on line "; string_of_int k])
  | nl when nl=Char.code '\n' -> raise (ParseError_ ["end of line inside string"])
  | dquote when dquote=Char.code '"' -> STRING (utf8_implode (List.rev rcs))
  | backslash when backslash=Char.code '\\' ->
      if char () = Char.code '\n' then
        begin
          incr linenum;
          next ();
          while
            match char () with
              space when space=Char.code ' ' -> true
            | tab   when tab  =Char.code '\t' -> true
            | nl    when nl   =Char.code '\n' -> incr linenum; true
            | _ -> false
          do next ()
          done;
          if char () = Char.code '\\' then next ()
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
    consolereport ["scansymb -- peekedsymb is "; bracketedstring_of_list debugstring_of_symbol ";" !peekedsymb];
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
    consolereport ["currnovelsymb "; debugstring_of_symbol !symb];
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
         [string_of_symbol !symb;
          " can't start a new identifier or operator"])

type savedlex = ucode Stream.t * string * int * symbol * symbol list

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
    c when c=Char.code ' ' -> SPACE
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
        bool_of_opt (fsmpos (rootfsm optree) (utf8_explode a1) &~~ (fun t -> fsmpos t [c]))
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

let pushSyntax name =
  let fixes = { appF = !appfix; substF = !substfix; substS = !substsense } in
  let tabs = get_syntax_tables() in
  syntaxes := (name, fixes, tabs) :: !syntaxes;
  resetSymbols();
  (* we only want to use one symbol table though! *)
  symboltable    := Hashtbl.copy tabs.symT;
  reversemapping := Hashtbl.copy tabs.revT;
  priotable      := Hashtbl.copy tabs.prioT;
  assoctable     := Hashtbl.copy tabs.assocT

let popSyntax () =
  match !syntaxes with 
    (name, fixes, tbls) :: sys ->
      let internal_symT = !symboltable in
      appfix:=fixes.appF; substfix:=fixes.substF; substsense:=fixes.substS;
      set_syntax_tables tbls;
      syntaxes := sys;
      (* because FORMULA CLASS and so on can sneak under the carefullyEnter radar,
         we run through the contents of the internal symbol table and make sure that
         every symbol is parsed the same in the outside syntax.
       *)
      let checksym str sym =
        match sym with
          (SUBSTBRA | SUBSTSEP | SUBSTKET) -> ()
        | _ -> 
            let newsym = 
              try let s = pushlex "" (stream_of_utf8string str) in
                  let r = !symb in
                  next(); 
                  let c = char () in
                  poplex s;
                  if c<>uEOF then raise (ParseError_ []);
                  r
              with ParseError_ _ ->
                     raise (ParseError_ ["After PUSHSYNTAX "; enQuote name; " the string "; enQuote str;
                                         " is read as "; debugstring_of_symbol sym;
                                         "; beforehand it would not have been a symbol."]) 
            in
            if newsym<>sym then
              raise (ParseError_ ["After PUSHSYNTAX "; enQuote name; " the string "; enQuote str;
                                  " is read as "; debugstring_of_symbol sym;
                                  "; beforehand it would have been "; 
                                  debugstring_of_symbol newsym])
      in
      Hashtbl.iter checksym internal_symT
  | _ -> raise (ParseError_ ["POPSYNTAX: stack empty"])
  
let popAllSyntaxes () =
  while !syntaxes<>[] do popSyntax () done

let resetSymbols() =  popAllSyntaxes(); resetSymbols()
