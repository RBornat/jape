(* $Id$ *)

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

open Idclass.M
open Searchtree
open Prestring.M
open Listfuns.M
open Miscellaneous.M
open Optionfuns.M
open Stringfuns.M
open Sml.M

type idclass = Idclass.M.idclass
 and associativity = Symboltype.associativity
 and symbol = Symboltype.symbol

(* smlnj 0.93 had no notion of char, only single-character strings.  In this first
   porting, I've used caml streams and converted all input to single-character strings.
   RB
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

let (isIDhead, updateIDhead) = charpred "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'"
let (isIDtail, updateIDtail) = charpred "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'0123456789"

let decIDheads : string list ref = ref []
let decIDtails : string list ref = ref []

let (isreserved, _) = charpred "\"_() \n\t" (* the chars we give special meaning to *)
(* we can't use a list of the chars we allow as PUNCT because that kills 8-bit fonts *)
(* recently deleted from isreserved: "$,[]" *)

let metachar = "_"
(* not a punct *)

let rec ispunct c =
  not (((c = metachar || isdigit c) || isIDhead c) || isreserved c)

let (reservedpunct, _) = charpred "(),[]"

(* for fast-ish lookup of declared operators and identifiers, and for some error reporting *)
let rec mkalt cts def =
  let aa = Array.make 256 def in
  let rec lookup c = Array.get aa (ord c) in
  List.iter (fun (c, t) -> Array.set aa (ord c) t) cts; lookup

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
        List.map (implode <*> (fun(r,_,_)->r)) (summarisetree !optree)
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
  begin updateIDhead (c, true); decIDheads := c :: !decIDheads; true end

let rec decIDtail c =
  isIDtail c ||
  not (isreserved c) &&
  begin updateIDtail (c, true); decIDtails := c :: !decIDtails; true end

let rec insertinIdtree what isprefix class__ tree s =
  let rec bang () =
    raise
      (Catastrophe_
         ["attempt to "; what; " "; idclassstring class__; " \""; s;
          "\""])
  in
  match explode s with
    [] -> bang ()
  | c :: cs ->
      if decIDhead c then () else bang ();
      List.iter (fun c -> if decIDtail c then () else bang ()) cs;
      (* may be a bad thing *)
      if !symboldebug then
        consolereport
          ["insertinIdtree "; idclassstring class__; " ";
           string_of_bool isprefix; " "; s];
      tree :=
        addtotree (fun (x, y) -> x = y) !tree (c :: cs, class__, isprefix)

let friendlyLargeishPrime = 1231
let symboltable = (* ref (Store.new__ friendlyLargeishPrime) *)
                  Hashtbl.create friendlyLargeishPrime
let lookup string = (* Store.at (!symboltable, string) *)
   try Some (Hashtbl.find symboltable string) with Not_found -> None

let reversemapping (* : (symbol, string) Mappingfuns.M.mapping ref = ref Mappingfuns.M.empty *)
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
  | None -> lookinIdtree idprefixtree (explode s)

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
    Some c -> BQuote2 ["Some("; idclassstring c; ")"]
  | None -> BQuote2 ["None"]
let rec pre_assoc a =
  match a with
    LeftAssoc -> BQuote1 "LeftAssoc"
  | RightAssoc -> BQuote1 "RightAssoc"
  | AssocAssoc -> BQuote1 "AssocAssoc"
  | TupleAssoc -> BQuote1 "TupleAssoc"
  | CommAssocAssoc -> BQuote1 "CommAssocAssoc"

let rec pre_SYMBOL s =
  match s with
    ID (s'1, class__) ->
      BQuote3
        [BQuote1 "ID("; BQuote1 "\""; BQuote1 s'1; BQuote1 "\""; pre__comma;
         preclassopt class__; BQuote1 ")"]
  | UNKNOWN (s'1, class__) ->
      BQuote3
        [BQuote1 "UNKNOWN("; BQuote1 "\""; BQuote1 s'1; BQuote1 "\"";
         pre__comma; preclassopt class__; BQuote1 ")"]
  | NUM s'1 -> BQuote3 [BQuote1 "NUM "; pre_string s'1]
  | STRING s'1 -> BQuote3 [BQuote1 "STRING \""; BQuote1 s'1; BQuote1 "\""]
  | BRA s'1 -> BQuote2 ["BRA \""; s'1; "\""]
  | SEP s'1 -> BQuote2 ["SEP \""; s'1; "\""]
  | KET s'1 -> BQuote2 ["KET \""; s'1; "\""]
  | SUBSTBRA -> BQuote2 ["SUBSTBRA"; (try " \"" ^ reverselookup SUBSTBRA ^ "\"" with Not_found -> "")]
  | SUBSTSEP -> BQuote2 ["SUBSTSEP"; (try " \"" ^ reverselookup SUBSTSEP ^ "\"" with Not_found -> "")]
  | SUBSTKET -> BQuote2 ["SUBSTKET"; (try " \"" ^ reverselookup SUBSTKET ^ "\"" with Not_found -> "")]
  | EOF -> BQuote1 "EOF"
  | PREFIX (s'1, s'2) ->
      BQuote3
        [BQuote1 "PREFIX ("; pre_int s'1; pre__comma; BQuote1 "\"";
         BQuote1 s'2; BQuote1 "\")"]
  | POSTFIX (s'1, s'2) ->
      BQuote3
        [BQuote1 "POSTFIX ("; pre_int s'1; pre__comma; BQuote1 "\"";
         BQuote1 s'2; BQuote1 "\")"]
  | INFIX (s'1, s'2, s'3) ->
      BQuote3
        [BQuote1 "INFIX("; pre_int s'1; pre__comma; pre_assoc s'2;
         pre__comma; BQuote1 "\""; BQuote1 s'3; BQuote1 "\")"]
  | INFIXC (s'1, s'2, s'3) ->
      BQuote3
        [BQuote1 "INFIXC("; pre_int s'1; pre__comma; pre_assoc s'2;
         pre__comma; BQuote1 "\""; BQuote1 s'3; BQuote1 "\")"]
  | LEFTFIX (s'1, s'2) ->
      BQuote3
        [BQuote1 "LEFTFIX("; pre_int s'1; pre__comma; BQuote1 "\"";
         BQuote1 s'2; BQuote1 "\")"]
  | MIDFIX (s'1, s'2) ->
      BQuote3
        [BQuote1 "MIDFIX("; pre_int s'1; pre__comma; BQuote1 "\"";
         BQuote1 s'2; BQuote1 "\")"]
  | RIGHTFIX (s'1, s'2) ->
      BQuote3
        [BQuote1 "RIGHTFIX("; pre_int s'1; pre__comma; BQuote1 "\"";
         BQuote1 s'2; BQuote1 "\")"]
  | STILE s'1 -> BQuote2 ["STILE \""; s'1; "\""]
  | SHYID s'1 -> BQuote2 ["RESERVED-WORD "; s'1]

let smlsymbolstring = pre_implode <*> pre_SYMBOL

let symbolstring = reverselookup

let rec register_op s sym =
  let rec bang () =
    raise (Catastrophe_ ["attempt to register_op \""; s; "\""])
  in
  if s = "" then bang ()
  else
    let cs = explode s in
    if List.exists (not <*> ispunct) cs then bang ()
    else
      begin
        if !symboldebug then
          consolereport
            ["register_op "; enQuote s; " "; smlsymbolstring sym];
        optree := addtotree (fun (x, y) -> x = y) !optree (cs, sym, false)
      end

let rec deregister_op s sym =
  if !symboldebug then
    consolereport ["deregister_op "; enQuote s; " "; smlsymbolstring sym];
  optree := deletefromtree (fun (x, y) -> x = y) !optree (explode s, sym, false)

let rec enter (string, symbol) =
  let rec doit () =
    let rec isop s = s <> "" && ispunct s in
    let hidden =
      match symbol with
        SUBSTBRA -> true
      | SUBSTKET -> true
      | SUBSTSEP -> true
      | _ -> false
    in
    if !symboldebug then
      consolereport ["enter("; enQuote string; ","; smlsymbolstring symbol; ")"];
    (try
       let oldstring = reverselookup symbol in
       if isop oldstring && 
          (match searchfsm (rootfsm optree) (explode oldstring) with Found _ -> true | NotFound _ -> false)
       then 
         (try deregister_op oldstring symbol 
          with DeleteFromTree_ -> 
            let string_of_csrb =
              triplestring (bracketedliststring (enQuote <*> String.escaped) ",") smlsymbolstring string_of_bool ","
            in
            consolereport 
              ["DeleteFromTree_\n\t"; 
                 pairstring (bracketedliststring string_of_csrb "; ") string_of_csrb "\n\t" 
                    (summarisetree !optree, (explode oldstring, symbol, false))]
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
             ["Attempt to redefine the syntactic role of BQuote2"; s;
              "'' to "; smlsymbolstring t])

let decVarPrefixes : (idclass, string) Mappingfuns.M.mapping ref =
  ref Mappingfuns.M.empty

let rec declareIdPrefix class__ s =
  begin match Mappingfuns.M.at (!decVarPrefixes, class__) with
    None ->
      decVarPrefixes :=
        Mappingfuns.M.( ++ )
          (!decVarPrefixes, Mappingfuns.M.( |-> ) (class__, s))
  | Some _ -> ()
  end;
  insertinIdtree "declareIdPrefix" true class__ idprefixtree s;
  (* no warnings about clashes any more
     case fsmpos (rootfsm idfixedtree) (explode s) of
       Some t => List.map ((fn t => s^implode t) o #1) (summarisetree t)
     | None   => []
  *)
  []

let rec autoID class__ prefix =
  match Mappingfuns.M.at (!decVarPrefixes, class__) with
    Some s -> s
  | None ->
      (* we just add underscores to prefix till it isn't in the IdPrefix tree *)
      match fsmpos (rootfsm idprefixtree) (explode prefix) with
        None   -> (let _ = declareIdPrefix class__ prefix in prefix)
      | Some _ -> autoID class__ (prefix ^ "_")

let rec declareIdClass class__ s =
  insertinIdtree "declareIdClass" false class__ idfixedtree s;
  enter (s, ID (s, Some class__));
  (* no warnings about clashes any more *)
  None

let rec isnumber s =
  not (List.exists (not <*> isdigit) (explode s))

let rec isextensibleID s =
  lookup s = None && lookinIdtree idprefixtree (explode s) <> NoClass

let commasymbol = INFIX (0, TupleAssoc, ",") (* comma is now an operator, and may we be lucky *)

let rec resetSymbols () =
  let rec enterclass f s = enter (s, f s) in
  let debug = !symboldebug in
  symboldebug := false;
  (* symboltable := Store.new__ friendlyLargeishPrime *)
  Hashtbl.clear symboltable;
  (* reversemapping := Mappingfuns.M.empty *)
  Hashtbl.clear reversemapping;
  optree := emptysearchtree mkalt;
  oplist := None;
  List.iter (fun c -> updateIDhead (c, false)) !decIDheads;
  decIDheads := [];
  List.iter (fun c -> updateIDtail (c, false)) !decIDtails;
  decIDtails := [];
  idprefixtree := emptysearchtree mkalt;
  idfixedtree := emptysearchtree mkalt;
  decVarPrefixes := Mappingfuns.M.empty;
  List.iter (enterclass (fun s->SHYID s))
    ["ABSTRACTION"; "ALL"; "AND"; "ARE"; "AUTOMATCH"; "AUTOUNIFY"; "BAG";
     "BIND"; "BUTTON"; "CHECKBOX"; "CHILDREN"; "CLASS"; "COMMAND";
     "CONCFRESH"; "CONCHIT"; "CONJECTUREPANEL"; "CONSTANT";
     "CURRENTPROOF"; "CUT"; "DERIVED"; "DISPROOF"; "END"; "ENTRY";
     "EVERYWHERE"; "FONTS"; "FORCE"; "FORMULA"; "FORMULAE"; "FRESH";
     "FROM"; "GIVENPANEL"; "HYPFRESH"; "HYPHIT"; "IDENTITY"; "IFF";
     "IMPCONCFRESH"; "IMPFRESH"; "IMPHYPFRESH"; "IMPLIES"; "IN"; "INFER";
     "INFIX"; "INFIXC"; "INITIALLY"; "INITIALISE"; "IS"; "JUXTFIX";
     "KEYBOARD"; "LABEL"; "LABELS"; "LEFTFIX"; "LEFTWEAKEN"; "LIST";
     "MACRO"; "MENU"; "MENUKEY"; "MIDFIX"; "NOTIN"; "NOWHERE"; "NOTONEOF";
     "NUMBER"; "OBJECT"; "OR"; "OUTFIX"; "POSTFIX"; "PATCHALERT";
     "PREFIX"; "PROOF"; "RADIOBUTTON"; "REFLEXIVE"; "RIGHTFIX";
     "RIGHTWEAKEN"; "RULE"; "RULES"; "SCOPE"; "SEMANTICS";
     "SEMANTICTURNSTILE"; "SEPARATOR"; "SEQUENT"; "Some"; "STRING";
     "STRUCTURERULE"; "SUBSTFIX"; "TACTIC"; "TACTICPANEL"; "THEOREM";
     "THEOREMS"; "THEORY"; "TRANSITIVE"; "UNIFIESWITH"; "USE"; "VARIABLE";
     "VIEW"; "WEAKEN"; "WHERE"; "WORLD"];
  enter ("(", BRA "(");
  (* these two still need special treatment ... *)
  enter (")", KET ")");
  enter ("[", SUBSTBRA);
  enter ("\\", SUBSTSEP);
  enter ("]", SUBSTKET);
  enter ("", EOF);
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
let errout = ref stderr
let linenum = ref 0
let symb = ref EOF
let peekedsymb : symbol list ref = ref []

let rec showInputError f msg =
  let loc =
    if !lexinfile = "" then []
    else [!lexinfile; " (line "; string_of_int !linenum; "): "]
  in
  f (loc @ msg)

let char () = match peek !lexin with Some c -> String.make 1 c | None -> "" 

let next () = try let _ = next !lexin in () with _ -> ()

let rec scanwhile =
  fun pp rcs con ->
    let c = char () in
    if c <> "" && pp c then (next (); scanwhile pp (c :: rcs) con)
    else con (implode (List.rev rcs))

let rec scanwatch f =
  if !symboldebug then
    fun v ->
      let c = f v in if !symboldebug then consolereport ["scanfsm "; c]; c
  else f

let rec scannext () = next (); char ()

let rec scanop fsm rcs =
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
    "" -> scanreport EOF
  | " " -> next (); scan ()
  | "\t" -> next (); scan ()
  | "\n" -> incr linenum; next (); scan ()
  | "(" -> next (); scanreport (BRA "(")
  | ")" ->(* necessary ? - I think so *)
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
  | NUM _ -> false
  | STRING _ -> false
  | EOF -> false
  | SHYID _ -> false
  | _ -> not (isreserved (reverselookup sy))

let rec currnovelsymb () =
  if canstartnovelsymb !symb then
    let symstr = reverselookup !symb in
    let cs = explode symstr in
    (* used to be ...
    if ispunct symstr then
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
          scanwhile (not <*> isreserved) (List.rev cs) checkbadID;
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
  | c ->
      if isIDtail c then LETTER
      else if ispunct c then PUNCT
      else if reservedpunct c then RESERVEDPUNCT
      else OTHER

let rec mustseparate =
  function
    "", _ -> false
  | _, "" -> false
  | a1, a2 ->
      let rec msp c =
        opt2bool (fsmpos (rootfsm optree) (explode a1) &~~ (fun t -> fsmpos t [c]))
      in
      let rec ms =
        function
          SPACE, _ -> false
        | _, SPACE -> false
        | PUNCT, _ -> msp (String.sub (a2) (0) (1))
        | _, PUNCT -> false
        | RESERVEDPUNCT, _ -> false
        | _, RESERVEDPUNCT -> false
        | _, _ -> true
      in
      ms (charclass (String.sub (a1) (String.length a1 - 1) (1)),
          charclass (String.sub (a2) (0) (1)))

let enter = carefullyEnter

let lookup = checkbadID

let _ = (try resetSymbols () with exn -> consolereport ["resetSymbols raised "; Printexc.to_string exn]; ())

