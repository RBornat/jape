(* $Id$ *)

module type Symbol =
  sig
    type vid and symbol and associativity and idclass
    val symboldebug : bool ref
    val enter : string * symbol -> unit
    val lookup : string -> symbol
    (* default is ID *)
    val lookupassoc : string -> (bool * associativity) option
    (* (curried, assoc) - when INFIX... *)

    val declareIdPrefix : idclass -> string -> (string * idclass) list
    val declareIdClass : idclass -> string -> (string * idclass) option
    val symclass : string -> idclass
    exception Symclass_ of string
    val isnumber : string -> bool
    val isextensibleID : vid -> bool
    val symbolstring : symbol -> string
    val smlsymbolstring : symbol -> string
    (* aid for prettyprinters *)
    
    val metachar : string
    val mustseparate : string * string -> bool
    (* aid for parsers *)

    val scansymb : unit -> symbol
    val currsymb : unit -> symbol
    val currnovelsymb : unit -> symbol
    (* so that user can define new symbols, punctuation marks, whatever *)
    val canstartnovelsymb : symbol -> bool
    val peeksymb : unit -> symbol
    val putbacksymb : symbol -> unit
    (* for primitive backtracking *)

    type savedlex
    val pushlex : string -> IO.instream -> savedlex
    val poplex : savedlex -> unit
    val showInputError : (string list -> unit) -> string list -> unit
    val resetSymbols : unit -> unit
    val escapechar : string -> string
    val unescapechar : string -> string
    val commasymbol : symbol
    val appfix : int ref
    val substfix : int ref
    val substsense : bool ref
    (* aid for keyboardists *)
    val get_oplist : unit -> string list
    val set_oplist : string list -> unit
    (* aid for name inventors *)
    val autoVID : idclass -> vid -> vid
  end
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
 
module
  Symbol
  (AAA :
    sig
      module prestring : Prestring
      module idclass : Idclass
      module searchtree : Searchtree
      module symboltype : Symboltype
      module store : store
      module mappingfuns : Mappingfuns
      val andthenr : 'a option * ('a -> 'b option) -> 'b option
      val bracketedliststring : ('a -> string) -> string -> 'a list -> string
      val charpred : string -> (string -> bool) * (string * bool -> unit)
      val consolereport : string list -> unit
      val enQuote : string -> string
      val isdigit : string -> bool
      val isprefix : ('a * 'a -> bool) -> 'a list -> 'a list -> bool
      val opt2bool : 'a option -> bool
      val sortunique : ('a * 'a -> bool) -> 'a list -> 'a list
      val unSOME : 'a option -> 'a
      exception Catastrophe_ of string list
      exception ParseError_ of string list
      
    end)
  :
  Symbol =
  struct
    open AAA
    open idclass
    open searchtree
    open symboltype
    open prestring
    open store
    open IO
    (* from store.sig.sml *)
    
    
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
    
    let (isIDhead, updateIDhead) =
      charpred "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'"
    let (isIDtail, updateIDtail) =
      charpred
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'0123456789"
    let decIDheads : string list ref = ref []
    let decIDtails : string list ref = ref []
    let (isreserved, _) = charpred "\"_() \n\t"
    (* the chars we give special meaning to *)
      
      (* we can't use a list of the chars we allow as PUNCT because that kills 8-bit fonts *)
      (* recently deleted from isreserved: "$,[]" *)
    let metachar = "_"
    (* not a punct *)

    let rec ispunct c =
      not (((c = metachar || isdigit c) || isIDhead c) || isreserved c)
    let (reservedpunct, _) = charpred "(),[]"
    (* for fast-ish lookup of declared operators and identifiers, and for some error reporting *)
    let rec mkalt cts def =
      let A = Array.make 256 def in
      let rec lookup c = Array.get A (Char.code c) in
      List.iter (fun (c, t) -> Array.set A (Char.code c) t) cts; lookup
    let optree : (string, symbol) searchtree ref = ref (emptysearchtree mkalt)
    let idprefixtree : (string, idclass) searchtree ref =
      ref (emptysearchtree mkalt)
    let idfixedtree : (string, idclass) searchtree ref =
      ref (emptysearchtree mkalt)
    let streq : string * string -> bool = fun (x, y) -> x = y
    (* list of 'funny symbols' to be used in on-screen keyboards and the like *)
    let oplist : string list option ref = ref None
    let rec get_oplist () =
      match !oplist with
        Some ss -> ss
      | None ->
          let ops =
            map (fun ooo -> implode (sml__hash__1 ooo))
              (summarisetree !optree)
          in
          sortunique (fun (x, y) -> x < y : string * string -> bool)
            ((ops @ !decIDheads) @ !decIDtails)
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
          decIDhead c || bang ();
          map (fun c -> decIDtail c || bang ()) cs;
          (* may be a bad thing *)
          if !symboldebug then
            consolereport
              ["insertinIdtree "; idclassstring class__; " ";
               makestring isprefix; " "; s];
          tree :=
            addtotree (fun (x, y) -> x = y) !tree (c :: cs, class__, isprefix)
    let FriendlyLargeishPrime = 1231
    let symboltable = ref (store.new__ FriendlyLargeishPrime)
    let reversemapping : (symbol, string) mappingfuns.mapping ref =
      ref mappingfuns.empty
    let rec lookup string = store.at (!symboltable, string)
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
      | _ -> unSOME (mappingfuns.at (!reversemapping, symbol))
    let rec preclassopt =
      function
        Some c -> BQuote2 ["Some("; idclassstring c; ")"]
      | None -> BQuote2 ["None"]
    let rec Preassoc a =
      match a with
        LeftAssoc -> BQuote1 "LeftAssoc"
      | RightAssoc -> BQuote1 "RightAssoc"
      | AssocAssoc -> BQuote1 "AssocAssoc"
      | TupleAssoc -> BQuote1 "TupleAssoc"
      | CommAssocAssoc -> BQuote1 "CommAssocAssoc"
    let rec PreSYMBOL s =
      match s with
        ID (S'1, class__) ->
          BQuote3
            [BQuote1 "ID("; BQuote1 "\""; BQuote1S'1; BQuote1 "\""; Pre_comma;
             preclassopt class__; BQuote1 ")"]
      | UNKNOWN (S'1, class__) ->
          BQuote3
            [BQuote1 "UNKNOWN("; BQuote1 "\""; BQuote1S'1; BQuote1 "\"";
             Pre_comma; preclassopt class__; BQuote1 ")"]
      | NUM S'1 -> BQuote3 [BQuote1 "NUM "; Prestring S'1]
      | STRING S'1 -> BQuote3 [BQuote1 "STRING \""; BQuote1S'1; BQuote1 "\""]
      | BRA S'1 -> BQuote2 ["BRA \""; S'1; "\""]
      | SEP S'1 -> BQuote2 ["SEP \""; S'1; "\""]
      | KET S'1 -> BQuote2 ["KET \""; S'1; "\""]
      | SUBSTBRA -> BQuote2 ["SUBSTBRA \""; reverselookup SUBSTBRA; "\""]
      | SUBSTSEP -> BQuote2 ["SUBSTSEP \""; reverselookup SUBSTSEP; "\""]
      | SUBSTKET -> BQuote2 ["SUBSTKET \""; reverselookup SUBSTKET; "\""]
      | EOF -> BQuote1 "EOF"
      | PREFIX (S'1, S'2) ->
          BQuote3
            [BQuote1 "PREFIX ("; Preint S'1; Pre_comma; BQuote1 "\"";
             BQuote1S'2; BQuote1 "\")"]
      | POSTFIX (S'1, S'2) ->
          BQuote3
            [BQuote1 "POSTFIX ("; Preint S'1; Pre_comma; BQuote1 "\"";
             BQuote1S'2; BQuote1 "\")"]
      | INFIX (S'1, S'2, S'3) ->
          if S'3 = "," then Pre_comma
          else
            BQuote3
              [BQuote1 "INFIX("; Preint S'1; Pre_comma; Preassoc S'2;
               Pre_comma; BQuote1 "\""; BQuote1S'3; BQuote1 "\")"]
      | INFIXC (S'1, S'2, S'3) ->
          BQuote3
            [BQuote1 "INFIXC("; Preint S'1; Pre_comma; Preassoc S'2;
             Pre_comma; BQuote1 "\""; BQuote1S'3; BQuote1 "\")"]
      | LEFTFIX (S'1, S'2) ->
          BQuote3
            [BQuote1 "LEFTFIX("; Preint S'1; Pre_comma; BQuote1 "\"";
             BQuote1S'2; BQuote1 "\")"]
      | MIDFIX (S'1, S'2) ->
          BQuote3
            [BQuote1 "MIDFIX("; Preint S'1; Pre_comma; BQuote1 "\"";
             BQuote1S'2; BQuote1 "\")"]
      | RIGHTFIX (S'1, S'2) ->
          BQuote3
            [BQuote1 "RIGHTFIX("; Preint S'1; Pre_comma; BQuote1 "\"";
             BQuote1S'2; BQuote1 "\")"]
      | STILE S'1 -> BQuote2 ["STILE \""; S'1; "\""]
      | SHYID S'1 -> BQuote2 ["RESERVED-WORD "; S'1]
    let smlsymbolstring ooo = Preimplode (PreSYMBOL ooo)
    let symbolstring = reverselookup
    let rec register_op s sym =
      let rec bang () =
        raise (Catastrophe_ ["attempt to register_op \""; s; "\""])
      in
      if s = "" then bang ()
      else
        let cs = explode s in
        if List.exists (fun ooo -> not (ispunct ooo)) cs then bang ()
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
      optree :=
        deletefromtree (fun (x, y) -> x = y) !optree (explode s, sym, false)
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
          consolereport
            ["enter("; enQuote string; ","; smlsymbolstring symbol; ")"];
        begin try
          let oldstring = reverselookup symbol in
          if isop oldstring then deregister_op oldstring symbol;
          if hidden then store.delete (!symboltable, oldstring)
        with
          UnSOME_ -> ()
        end;
        store.update (!symboltable, string, symbol);
        if hidden then
          reversemapping :=
            mappingfuns.( ++ )
              (!reversemapping, mappingfuns.( |-> ) (symbol, string));
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
    let decVarPrefixes : (idclass, string) mappingfuns.mapping ref =
      ref mappingfuns.empty
    let rec declareIdPrefix class__ s =
      begin match mappingfuns.at (!decVarPrefixes, class__) with
        None ->
          decVarPrefixes :=
            mappingfuns.( ++ )
              (!decVarPrefixes, mappingfuns.( |-> ) (class__, s))
      | Some _ -> ()
      end;
      insertinIdtree "declareIdPrefix" true class__ idprefixtree s;
      (* no warnings about clashes any more
         case fsmpos (rootfsm idfixedtree) (explode s) of
           Some t => map ((fn t => s^implode t) o #1) (summarisetree t)
         | None   => []
      *)
      []
    let rec autoVID class__ prefix =
      match mappingfuns.at (!decVarPrefixes, class__) with
        Some s -> s
      | None ->
          (* we just add underscores to prefix till it isn't in the IdPrefix tree *)
          match fsmpos (rootfsm idprefixtree) (explode prefix) with
            None -> declareIdPrefix class__ prefix; prefix
          | Some _ -> autoVID class__ (prefix ^ "_")
    let rec declareIdClass class__ s =
      insertinIdtree "declareIdClass" false class__ idfixedtree s;
      enter (s, ID (s, Some class__));
      (* no warnings about clashes any more *)
      None
    let rec isnumber s =
      not (List.exists (fun ooo -> not (isdigit ooo)) (explode s))
    let rec isextensibleID s =
      lookup s = None && lookinIdtree idprefixtree (explode s) <> NoClass
    let commasymbol = INFIX (0, TupleAssoc, ",")
    (* comma is now an operator, and may we be lucky *)

    let rec resetSymbols () =
      let rec enterclass f s = enter (s, f s) in
      let debug = !symboldebug in
      symboldebug := false;
      symboltable := store.new__ FriendlyLargeishPrime;
      reversemapping := mappingfuns.empty;
      optree := emptysearchtree mkalt;
      oplist := None;
      List.iter (fun c -> updateIDhead (c, false)) !decIDheads;
      decIDheads := [];
      List.iter (fun c -> updateIDtail (c, false)) !decIDtails;
      decIDtails := [];
      idprefixtree := emptysearchtree mkalt;
      idfixedtree := emptysearchtree mkalt;
      decVarPrefixes := mappingfuns.empty;
      List.iter (enterclass SHYID)
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
    let rec escapechar c =
      match c with
        "n" -> "\n"
      | "r" -> Char.chr 13
      | "t" -> "\t"
      | "\"" -> "\""
      | "\\" ->(* this line not strictly necessary, but worth it for notification value *)
         "\\"
      | c ->(* ditto *)
         c
    let rec unescapechar c =
      match c with
        "\n" -> "\\n"
      | "\t" -> "\\t"
      | "\"" -> "\\\""
      | "\\" -> "\\\\"
      | c -> if c = Char.chr 13 then "\\r" else c
    (* I collected all these things into one place, and wrapped them in local, 
     * because these variables form all of the state of the lexer.  
     * Then you can properly set and reset the state, as in tryparse and 
     * other places, with pushlex and poplex.
     * RB 14/xii/94
     *)
    let lexin = ref stdin
    let lexinfile = ref ""
    let errout = ref stderr
    let linenum = ref 0
    let symb = ref EOF
    let peekedsymb : symbol list ref = ref []
    let rec showInputError f msg =
      let loc =
        if !lexinfile = "" then []
        else [!lexinfile; " (line "; makestring !linenum; "): "]
      in
      f (loc @ msg)
    let rec char () = lookahead !lexin
    let rec next () = input_char !lexin 1
    let rec scanwhile =
      fun P rcs con ->
        let c = char () in
        if c <> "" && P c then begin next (); scanwhile P (c :: rcs) con end
        else con (implode (rev rcs))
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
          else checkbadID (implode (rev rcs'))
    (* at this point we need backtracking ... *)
                      

    let rec scanid conf =
      let rec q rcs classopt =
        scanwhile isIDtail
          (if null rcs then :: (char (), [], before, next, ()) else rcs)
          (conf classopt)
      in
      match
        scanfsm (scanwatch scannext) (rootfsm idprefixtree) []
          (scanwatch char ())
      with
        Found (class__, rcs) -> q rcs (Some class__)
      | NotFound rcs -> q rcs None
    let rec scanreport s =
      if !symboldebug then begin consolereport [smlsymbolstring s]; s end
      else s
    let rec checkidclass con takeit class__ s =
      match lookup s with
        Some (ID (s, class')) -> con (s, class')
      | Some sy -> if takeit then sy else con (s, class__)
      | None -> con (s, class__)
    let rec scan () =
      match char () with
        "" -> scanreport EOF
      | " " -> next (); scan ()
      | "\t" -> next (); scan ()
      | "\n" -> inc linenum; next (); scan ()
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
              if isextensibleID s then checkidclass UNKNOWN false class__ s
              else raise (ParseError_ ["non-CLASS unknown "; metachar; s])
            in
            next ();
            if isIDhead (char ()) then
              scanreport (scanid (checkidclass UNKNOWN false))
            else raise (ParseError_ ["ID expected following "; metachar])
          else if isIDhead c then scanreport (scanid (checkidclass ID true))
          else if isdigit c then scanreport (scanwhile isdigit [] NUM)
          else if ispunct c then scanreport (scanop (rootfsm optree) [])
          else raise (Catastrophe_ ["scan can't see class of "; c])
    and scanComment (n, k) =
      match char () with
        "" ->
          raise
            (ParseError_
               ["End of file inside level "; makestring n;
                " comment starting on line "; makestring k])
      | "\n" -> inc linenum; next (); scanComment (n, k)
      | "*" ->
          next ();
          begin match char () with
            "/" -> next ()
          | "\n" -> inc linenum; next (); scanComment (n, k)
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
                          [", within a level "; makestring n;
                           " comment starting on line"; makestring k]))
              end;
              scanComment (n, k)
          | "\n" -> inc linenum; next (); scanComment (n, k)
          | _ -> scanComment (n, k)
          end
      | _ -> next (); scanComment (n, k)
    and scanString k rcs =
      match char () before next () with
        "" ->
          raise
            (ParseError_
               ["end of file inside string starting on line "; makestring k])
      | "\n" -> raise (ParseError_ ["end of line inside string"])
      | "\"" -> STRING (implode (rev rcs))
      | "\\" ->
          if char () = "\n" then
            begin
              inc linenum;
              next ();
              while
                match char () with
                  " " -> true
                | "\t" -> true
                | "\n" -> inc linenum; true
                | _ -> false
              do next ()
              done;
              if char () = "\\" then next ()
              else
                raise
                  (ParseError_
                     ["expected \\ to restart broken string starting on line ";
                      makestring k]);
              scanString k rcs
            end
          else scanString k (escapechar (char () before next ()) :: rcs)
      | c -> scanString k (c :: rcs)
    let showInputError = showInputError
    let rec scansymb () =
      match !peekedsymb with
        [] -> symb := scan (); !symb
      | sym :: more -> symb := sym; peekedsymb := more; !symb
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
             [] => (symb := scanwhile ispunct (rev cs) checkbadID; 
                    !symb
                   )
           | _    => raise Catastrophe_ ["peeksymb(); currnovelsymb()"]
        else !symb
        *)
        match !peekedsymb with
          [] ->
            symb :=
              scanwhile (fun ooo -> not (isreserved ooo)) (rev cs) checkbadID;
            !symb
        | _ -> raise (Catastrophe_ ["peeksymb(); currnovelsymb()"])
      else
        raise
          (ParseError_
             [symbolstring !symb;
              " can't start a new identifier or operator"])
    type savedlex = IO.instream * string * int * symbol * symbol list
    let rec pushlex name newstream =
      let state = !lexin, !lexinfile, !linenum, !symb, !peekedsymb in
      lexin := newstream;
      lexinfile := name;
      linenum := 1;
      peekedsymb := [];
      scansymb ();
      state
    let rec poplex (lxin, lxinf, lnum, sy, pksy) =
      close_in !lexin;
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
            opt2bool
              (andthenr
                 (fsmpos (rootfsm optree) (explode a1),
                  (fun t -> fsmpos t [c])))
          in
          let rec ms =
            function
              SPACE, _ -> false
            | _, SPACE -> false
            | PUNCT, _ -> msp (substring (a2, 0, 1))
            | _, PUNCT -> false
            | RESERVEDPUNCT, _ -> false
            | _, RESERVEDPUNCT -> false
            | _, _ -> true
          in
          ms (charclass (substring (a1, String.length a1 - 1, 1)),
              charclass (substring (a2, 0, 1)))
    let enter = carefullyEnter
    let lookup = checkbadID
    let _ = resetSymbols ()
  end

