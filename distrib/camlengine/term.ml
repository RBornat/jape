(* $Id$ *)

module type TM0 = 
  sig
	type vid = Symboltype.M.vid and idclass = Idclass.M.idclass
	  (* can't seem to avoid this definiteness here ... or can I? *)
  end

module TM0 : TM0 = 
  struct
	type vid = Symboltype.M.vid and idclass = Idclass.M.idclass
	  (* can't seem to avoid this definiteness here ... or can I? *)
  end

module type Termtype = 
  sig
	open TM0
	(* terms now contain hash information. RB 26/i/00 *)
	type term =
		Id of (int option * vid * idclass)
	  | Unknown of (int option * vid * idclass)
	  | App of (int option * term * term)
	  | Tup of (int option * string * term list)
	  | Literal of (int option * litcon)
	  | Fixapp of (int option * string list * term list)
	  | Subst of (int option * bool * term * (term * term) list)
	  | Binding of
		  (int option * (term list * term list * term list) *
			 (term * (int * int)) list * term)
	  | Collection of (int option * idclass * element list)
	and litcon = Number of int | String of string
	and element =
		Segvar of (int option * term list * term)
	  | Element of (int option * resnum * term)
	and resnum = Nonum | Resnum of int | ResUnknown of int
end
	
module Termtype =
  struct
    open TM0
	type term =
		Id of (int option * vid * idclass)
	  | Unknown of (int option * vid * idclass)
	  | App of (int option * term * term)
	  | Tup of (int option * string * term list)
	  | Literal of (int option * litcon)
	  | Fixapp of (int option * string list * term list)
	  | Subst of (int option * bool * term * (term * term) list)
	  | Binding of
		  (int option * (term list * term list * term list) *
			 (term * (int * int)) list * term)
	  | Collection of (int option * idclass * element list)
	and litcon = Number of int | String of string
	and element =
		Segvar of (int option * term list * term)
	  | Element of (int option * resnum * term)
	and resnum = Nonum | Resnum of int | ResUnknown of int
  end    	

module type Termstore =
  sig
	open TM0
	open Termtype
	val registerId : vid * idclass -> term
	val registerUnknown : vid * idclass -> term
	val registerApp : term * term -> term
	val registerTup : string * term list -> term
	val registerLiteral : litcon -> term
	val registerFixapp : string list * term list -> term
	val registerSubst : bool * term * (term * term) list -> term
	val registerBinding :
	  (term list * term list * term list) * 
	  (term * (int * int)) list *
	  term -> term
	val registerCollection : idclass * element list -> term
	val registerElement : resnum * term -> element
	val registerSegvar : term list * term -> element
	val resettermstore : unit -> unit
  end
	   
module type Term =
  sig
	open TM0
	open Termtype
	val isconstant : term -> bool
	val isId : term -> bool
	val isUnknown : term -> bool
	val isVariable : term -> bool
	val isleaf : term -> bool
	val isidentifier : term -> bool
	val ismetav : term -> bool
	val isextensibleId : term -> bool
	val isemptycollection : term -> bool
	val isCollection : term -> bool
	val isleafelement : element -> bool
	val issegvar : element -> bool
	val isselectionSubst : element -> bool
	val termkind : term -> int
	val termkindmax : int
	val emptycollection : idclass -> term
	val collection2term : term -> term option
	val element2term : element -> term option
	val int2term : int -> term
	val term2int : term -> int
	(* may raise AtoI_ or Catastrophe_ *)
	   
	val debracketapplications : bool ref
	val mapterm : (term -> term option) -> term -> term
	val option_mapterm : (term -> term option) -> term -> term option
	(* Some if rewritten *)
	val foldterm : (term * 'a -> 'a option) -> 'a -> term -> 'a
	val nj_foldterm : (term * 'a -> 'a option) -> term * 'a -> 'a
	val foldelements : (term * 'a -> 'a option) -> 'a -> element list -> 'a
	val findterm : (term -> 'a option) -> term -> 'a option
	val findhole : ((term -> term) -> term -> 'a option) -> term -> 'a option
	val searchterm : (term -> 'a option) -> 'a -> term -> 'a
	val existsterm : (term -> bool) -> term -> bool
	(* functions to give access to the innards of a term without giving away the whole type *)
	val decodeSubst : term -> (bool * term * (term * term) list) option
	(* gives r, p_, vts *)
	val decodeBinding : term -> (term list * term list * term list) option
	(* gives bs, ss, us *)
	val decodeBracketed : term -> term option
	val canonicalsubstmap : (term * term) list -> (term * term) list
	val termstring : term -> string
	val termstring_invisbracketed : term -> string
	val termstring_invischoose :
	  (term -> string) -> (term -> string) -> term -> string
	val vtsstring : (term * term) list -> string
	val argstring : term -> string
	(* bracketed for use as args in curried functions *)
	val collectionstring : string -> term -> string
	(* for those who don't want to see the details *)
	val termOrCollectionstring : string -> term -> string
	val smltermstring : term -> string
	val elementstring : element -> string
	(* what goes on the screen *)
	val elementstring_invisbracketed : element -> string
	val elementstring_invischoose :
	  (term -> string) -> (term -> string) -> element -> string
	val smlelementstring : (term -> string) -> element -> string
	val resnumstring : resnum -> string
	val termliststring : term list -> string
	val catelim_termstring : term -> string list -> string list
	val catelim_vtsstring : (term * term) list -> string list -> string list
	val catelim_argstring : term -> string list -> string list
	val catelim_collectionstring :
	  string -> term -> string list -> string list
	val catelim_termOrCollectionstring :
	  string -> term -> string list -> string list
	val catelim_smltermstring : term -> string list -> string list
	val catelim_elementstring : element -> string list -> string list
	val catelim_smlelementstring :
	  (term -> string list -> string list) -> element -> string list ->
		string list
	val catelim_resnumstring : resnum -> string list -> string list
	val catelim_termliststring : term list -> string list -> string list
	val bracketed : term -> bool
	val debracket : term -> term
	val enbracket : term -> term
	val eqterms : term * term -> bool
	(* ignoring bracketing *)
	val eqalphaterms : term * term -> bool
	(* ignoring bracketing and alpha-conversion *)
	val eqalphadebug : bool ref
	val termoccursin : term -> term -> bool
	val simterms : term * term -> bool
	val termvars : term -> term list
	val tmerge : term list -> term list -> term list
	val varbindings : term -> (term * term list list list) list
	val bmerge :
	  (term * term list list list) list -> (term * term list list list) list ->
		(term * term list list list) list
	val freevarsfrombindings :
	  (term * term list list list) list -> (term * term) list ->
		term list * (term * term list) list
	val varbindingsdebug : bool ref
	val earliervar : term -> term -> bool
	val mergevars : term list -> term list -> term list
	val termVIDs : term -> vid list
	val vartoVID : term -> vid
	val conVIDs : term list -> vid list
	val orderVIDs : vid list -> vid list
	val uniqueVID : idclass -> vid list -> vid list -> vid -> vid
	val mergeVIDs : vid list -> vid list -> vid list
	val idclass : term -> idclass
	val isSubstClass : term -> bool
	val specialisesto : idclass * idclass -> bool
	(* meant to be infix; A specialisesto B if a B-thing is a special kind of A-thing *)
	val canoccurfreein : idclass * idclass -> bool
	(* temporary additions to ease the passage to Collection use *)
	val explodeCollection : term -> element list
	val augmentCollection : term -> element list -> term option
	(* possibly permanent additions to ease passage to Collection use *)
	val elementnumbers : term -> resnum list
	val resnum2int : resnum -> int
	val isProperResnum : resnum -> bool
	val elementnumbered : term -> resnum -> term option
	val collectionkind : term -> idclass option
	val replaceelement : term -> element -> term -> element * term
	val eqelements : (term * term -> bool) -> element * element -> bool
	(* takes no notice of resource numbers *)
	val sameresource : element * element -> bool
	(* only looks at resource numbers *)
	val earlierresource : element * element -> bool
	(* only looks at resource numbers *)

	val explodeApp : bool -> term -> term * term list
	val implodeApp : bool -> term * term list -> term
	val explodebinapp : term -> (term * string * term) option
	val hashterm : term -> int option
	val hashelement : element -> int option
	val termhashing : bool ref
  end
  
(* $Id$ *)

module M :
  sig include TM0
      include Termtype
	  include Termstore
	  include Term 
  end with type vid = Symboltype.M.vid and type idclass = Idclass.M.idclass
=
  struct
    open Miscellaneous.M
    let invisbra = String.make 1 offbra
    and invisket = String.make 1 offket
    
    open Stringfuns.M
    open Optionfuns.M
    open Listfuns.M
    open Mappingfuns.M
    open Optionfuns.M
    open Symboltype.M
    open Symbol.M
    open Idclass.M
    open Idclassfuns.M
    open Answer.M
    
    open Simplecache.M
    
    type vid = Symboltype.M.vid
    type idclass = Idclass.M.idclass
    
    
    (* terms now contain hash information. RB 26/i/00 *)
    (* It's become an option so we don't cache terms which contain unknowns. RB 27/i/00 *)
    include Termtype

    let rec hashterm =
      function
        Id (h, _, _) -> h
      | Unknown (h, _, _) -> h
      | App (h, _, _) -> h
      | Tup (h, _, _) -> h
      | Literal (h, _) -> h
      | Fixapp (h, _, _) -> h
      | Subst (h, _, _, _) -> h
      | Binding (h, _, _, _) -> h
      | Collection (h, _, _) -> h
    let rec hashelement =
      function
        Segvar (h, _, _) -> h
      | Element (h, _, _) -> h
    (* --------------------- hack to help binding matches go faster --------------------- *)

    let rec termkind t =
      match t with
        Id _ -> 0
      | Unknown _ -> 1
      | App _ -> 2
      | Tup _ -> 3
      | Literal _ -> 4
      | Fixapp _ -> 5
      | Subst _ -> 6
      | Binding _ -> 7
      | Collection _ -> 8
    let termkindmax = 8
    (**** (possibly temporary) additions to ease transition to Collection use ****)

    let rec elementnumbers =
      function
        Collection (_, _, es) ->
          let rec f =
            function
              Element (_, r, _) -> Some r
            | _ -> None
          in
          optionfilter f es
      | _ -> []
    let rec elementnumbered a1 a2 =
      match a1, a2 with
        Collection (_, _, es), r ->
          findfirst
            (function
               Element (_, r', t) -> if r = r' then Some t else None
             | _ -> None)
            es
      | _, _ -> None
    let rec resnum2int =
      function
        Resnum n -> n
      | ResUnknown n -> n
      | Nonum -> 0
    let rec isProperResnum =
      function
        Resnum _ -> true
      | _ -> false
    let rec collectionkind =
      function
        Collection (_, c, _) -> Some c
      | _ -> None
    (************** printing out internal structure of term *************)
     
    let rec catelim_resnumstring r tail =
      match r with
        Nonum -> "Nonum" :: tail
      | Resnum r -> "Resnum " :: string_of_int r :: tail
      | ResUnknown r -> "ResUnknown " :: string_of_int r :: tail
    let resnumstring = catelim2stringfn catelim_resnumstring
    let rec dolist f = catelim_bracketedliststring f ","
    (* for those who need to know *exactly* what they have got *)
    let rec catelim_smltermstring t tail =
      match t with
        Id (h, v, c) ->
          "Id(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              (",\"" :: v :: "\"," :: idclassstring c :: ")" :: tail)
      | Unknown (h, v, c) ->
          "Unknown(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              (",\"" :: v :: "\"," :: idclassstring c :: ")" :: tail)
      | App (h, f, a) ->
          "App(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              ("," ::
                 catelim_smltermstring f
                   ("," :: catelim_smltermstring a (")" :: tail)))
      | Tup (h, s, ts) ->
          "Tup(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              ("," :: enQuote s :: "," ::
                 dolist catelim_smltermstring ts (")" :: tail))
      | Literal (h, Number k) ->
          "Literal(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              (",Number " :: string_of_int k :: ")" :: tail)
      | Literal (h, String k) ->
          "Literal(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              (",String \"" :: k :: "\")" :: tail)
      | Fixapp (h, ss, ts) ->
          "Fixapp(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              ("," ::
                 dolist (stringfn2catelim enQuote) ss
                   ("," :: dolist catelim_smltermstring ts (")" :: tail)))
      | Subst (h, r, p_, m) ->
          "Subst(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              ("," :: string_of_bool r :: "," ::
                 catelim_smltermstring p_
                   ("," :: catelim_smlsubstmapstring m (")" :: tail)))
      | Binding (h, (bs, ss, us), _, pat) ->
          "Binding(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              (",(" ::
                 dolist catelim_smltermstring bs
                   ("," ::
                      dolist catelim_smltermstring ss
                        ("," ::
                           dolist catelim_smltermstring us
                             (")," :: "...," ::
                                catelim_smltermstring pat (")" :: tail)))))
      | Collection (h, k, es) ->
          "Collection(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              ("," :: idclassstring k :: "," ::
                 dolist (catelim_smlelementstring catelim_smltermstring) es
                   (")" :: tail))
    and catelim_smlsubstmapstring vts =
      dolist
        (fun (v, t) tail ->
           "(" ::
             catelim_smltermstring v
               ("," :: catelim_smltermstring t (")" :: tail)))
        vts
    and catelim_smlelementstring f e tail =
      match e with
        Segvar (h, ps, v) ->
          "Segvar(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              ("," :: dolist f ps (f v (")" :: tail)))
      | Element (h, r, t) ->
          "Element(" ::
            catelim_optionstring (stringfn2catelim string_of_int) h
              ("," :: catelim_smlresnumstring r ("," :: f t (")" :: tail)))
    and catelim_smlresnumstring r = catelim_resnumstring r
    let smltermstring = catelim2stringfn catelim_smltermstring
    let rec smlelementstring f =
      catelim2stringfn (catelim_smlelementstring (stringfn2catelim f))
    (******** rebuilding bindings ******************)
    
    let rec remake mapterm (_, (bs, ss, us), env, pat as b) =
      try
        let emap = mkmap env in
        let rec f t =
          match at (emap, t) with
            Some (1, i) -> Some (List.nth (bs) (i))
          | Some (2, i) -> Some (List.nth (ss) (i))
          | Some (3, i) -> Some (List.nth (us) (i))
          | Some (k, i) ->
              raise
                (Catastrophe_
                   ["Some("; string_of_int k; ","; string_of_int i; ") in remake ";
                    smltermstring (Binding b)])
          | None -> None
        in
        mapterm f pat
      with
        Failure "nth" ->
          raise (Catastrophe_ ["Failure \"nth\" in remake "; smltermstring (Binding b)])
    (* We keep the user's bracket structure, so every time we match/unify/compare
       two terms, we must debracket them
     *)
    let rec bracketed =
      function
        Fixapp (_, ["("; ")"], [t]) -> true
      | t -> false
    let rec debracket =
      function
        Fixapp (_, ["("; ")"], [t]) -> debracket t
      | t -> t
    (* ------------------------------------------------------------------------------------- *)
    (* Bernard's pretty-printer in all its glory *)
         
    let debracketapplications = ref false
    exception Matchintermstring_
    (* spurious *)

        (* local versions of mapterm, mapelements, which don't use the termstore *)
        (* This is mildly more efficient, but really is because I want to use termstring
         * to monitor the termstore, and termstring needs to remake bindings, and remake needs
         * mapterm, and mapterm needs mapelements.
         *)
    let rec mapterm f t =
      match f t with
        Some t' -> t'
      | None ->
          let mtf = mapterm f in
          match t with
            App (_, f, a) -> App (None, mtf f, mtf a)
          | Tup (_, s, ts) -> Tup (None, s, _MAP (mtf, ts))
          | Fixapp (_, ss, ts) -> Fixapp (None, ss, _MAP (mtf, ts))
          | Binding (_, (bs, ss, us), env, pat) ->
              Binding
                (None, (_MAP (mtf, bs), _MAP (mtf, ss), _MAP (mtf, us)), env,
                 pat)
          | Subst (_, r, p_, vts) ->
              Subst (None, r, mtf p_, _MAP ((fun (v, t) -> mtf v, mtf t), vts))
          | Collection (_, k, es) -> Collection (None, k, mapelements f es)
          | _ -> t
    and mapelements f es =
      let rec g =
        function
          Segvar (_, ps, v) -> Segvar (None, ps, mapterm f v)
        | Element (_, r, t) ->(* not really satisfactory *)
           Element (None, r, mapterm f t)
      in
      (* yes, it should really be r *)
      List.map g es
    let rec stripelement =
      function
        Element (_, _, t) -> t
      | Segvar (_, [], v) -> v
      | Segvar (_, p :: ps, v) ->
          App (None, p, stripelement (Segvar (None, ps, v)))
    (* prettyprinter proper starts here *)
    
    let rec firstatom =
      function
        [] -> ""
      | "" :: ss -> firstatom ss
      | s :: ss -> if invisible s then firstatom ss else s
    (* now with priority exactly like :: *)
    
    (* triplecolon strips out blank strings, then puts a space _before and after_ things that 
     * must be separated. It's used for infix operators.
     *)
    (* ""   tricolon atoms = atoms -- never happens *)
    let rec tricolon a b =
      match a,b with
        atom, [] -> [atom]
      | atom, "" :: rest -> tricolon atom rest
      | atom, rest ->
          if mustseparate (atom, firstatom rest) then
            " " :: atom :: " " :: rest
          else atom :: rest
    (* quadruplecolon strips out blank strings, removes double spaces, puts spaces in between
     * atoms that must be separated.
     * Because _T is now properly cat-eliminated, we can't fold quadcolon into the result as we used to.
     * So we have to use it in place of quadcolon, almost everywhere.
     *)
    let rec quadcolon a b =
      match a, b with
        "", atoms -> atoms
      | atom, [] -> [atom]
      | " ", " " :: rest -> quadcolon " " rest
      | atom, rest ->
          (* a bit of overkill? *)
          if mustseparate (atom, firstatom rest) then atom :: insertspace rest
          else atom :: rest
    and insertspace =
      function
        r :: rs as rest ->
          if r = invisket then r :: insertspace rs else " " :: rest
      | [] -> [" "]
    let rec opname f =
      match f, !debracketapplications, debracket f with
        Id (_, name, _), _, _ -> Some name
      | _, true, Id (_, name, _) -> Some name
      | _ -> None
    (* this function will probably produce stupid results if the language includes
     * operators with identical priorities but differing associativity.
     *)
    (* I tried to write this without disfiguring it with parameters ivb & ivk, then I realised
     * that the compiler would put them in anyway.  So it's ugly, but not obviously more 
     * inefficient.
     * RB 25/6/2001
     *)
    let rec _T ivb ivk n a t s =
      (* a isn't associativity, it's mustbracket if equal priority *)
      let rec mustbracket b a m = if n > m || n = m && a then b else "" in
      let _OB = mustbracket "(" a in
      let _CB = mustbracket ")" a in
      let _OBprefix = mustbracket "(" false in
      let _CBprefix = mustbracket ")" false in
      let rec _TAP f arg s =
        quadcolon
          (_OB !appfix)
		  (_T ivb ivk !appfix false f
			  (_T ivb ivk !appfix true arg (quadcolon (_CB !appfix) s)))
      in
      let rec tip_ n' assoc arg1 f arg2 s =
        quadcolon
          (_OB n')
		  (let afterf =
			 _T ivb ivk n' (assoc <> RightAssoc) arg2
				(quadcolon (_CB n') s)
		   in
		   _T ivb ivk n' (assoc <> LeftAssoc) arg1
			  (tricolon f afterf))
      in
      let rec _TFA sy f t s =
        match sy with
          PREFIX (m, name) ->
            quadcolon
              (_OBprefix m)
              (quadcolon
                 name (_T ivb ivk m false t (quadcolon (_CBprefix m) s)))
        | POSTFIX (m, name) ->
            quadcolon
              (_OB m)
              (_T ivb ivk m false t
                  (quadcolon name (quadcolon (_CB m) s)))
        | _ -> _TAP f t s
      in
      let rec _TT n b sep ts s =
        let rec _TT' =
          function
            [] -> s
          | [t] -> _T ivb ivk n b t s
          | t :: ts -> _T ivb ivk n b t (tricolon sep (_TT' ts))
        in
        _TT' ts
      in
      match t with
        Id (_, v, _) ->
          begin match lookup v with
            INFIX _ ->
              ivb t ::
                quadcolon "(" (quadcolon v (quadcolon ")" (ivk t :: s)))
          | INFIXC _ ->
              ivb t ::
                quadcolon "(" (quadcolon v (quadcolon ")" (ivk t :: s)))
          | PREFIX _ ->
              ivb t ::
                quadcolon "(" (quadcolon v (quadcolon ")" (ivk t :: s)))
          | POSTFIX _ ->
              ivb t ::
                quadcolon "(" (quadcolon v (quadcolon ")" (ivk t :: s)))
          | _ -> ivb t :: quadcolon v (ivk t :: s)
          end
      | Unknown (_, v, _) -> ivb t :: quadcolon (metachar ^ v) (ivk t :: s)
      | App (_, (App (_, f, arg1) as l), arg2) ->
          begin match
            andthenr
              (opname f,
               (fun name ->
                  match lookup name with
                    INFIXC (m, a, _) ->
                      Some
                        (ivb t :: tip_ m a arg1 name arg2 (ivk t :: s))
                  | _ -> None))
          with
            Some r -> r
          | _ -> ivb t :: _TAP l arg2 (ivk t :: s)
          end
      | App (_, f, arg) ->
          begin match
            andthenr
              (opname f,
               (fun name ->
                  match lookup name with
                    INFIX (m, a, _) ->
                      begin match
                        arg, !debracketapplications, debracket arg
                      with
                        Tup (_, ",", [arg1; arg2]), _, _ ->
                          Some
                            (ivb t ::
                               tip_ m a arg1 name arg2 (ivk t :: s))
                      | _, true, Tup (_, ",", [arg1; arg2]) ->
                          Some
                            (ivb t ::
                               tip_ m a arg1 name arg2 (ivk t :: s))
                      | _ -> None
                      end
                  | sy -> Some (ivb t :: _TFA sy f arg (ivk t :: s))))
          with
            Some r -> r
          | None -> ivb t :: _TAP f arg (ivk t :: s)
          end
      | Tup (_, sep, ts) ->
          let n' =
            match lookup sep with
              INFIX (n', _, _) -> n'
            | _ -> 0
          in
          ivb t ::
            quadcolon
              (_OB n') (_TT n' true sep ts (quadcolon (_CB n') (ivk t :: s)))
      | Literal (_, Number k) ->(* take in the brackets as well ... *)
         ivb t :: quadcolon (string_of_int k) (ivk t :: s)
      | Literal (_, String k) ->
          ivb t :: quadcolon (("\"" ^ k) ^ "\"") (ivk t :: s)
      | Fixapp (_, ss, ts) ->
          begin match lookup (List.hd ss) with
            BRA _ ->
              ivb t ::
                quadcolon
                  (List.hd ss) (_TS1 ivb ivk 0 (List.tl ss) false ts (ivk t :: s))
          | LEFTFIX (m, _) ->
              ivb t ::
                quadcolon
                  (_OBprefix m)
                  (quadcolon
                     (List.hd ss)
                      (_TS2
                         ivb ivk m (List.tl ss) true ts
                         (quadcolon (_CBprefix m) (ivk t :: s))))
          | MIDFIX (m, _) ->
              ivb t ::
                quadcolon
                  (_OB m)
                  (_TS2 ivb ivk m ss true ts
                        (quadcolon (_CB m) (ivk t :: s)))
          | RIGHTFIX (m, _) ->
              ivb t ::
                quadcolon
                  (_OB m)
                  (_TS1 ivb ivk m ss true ts
                        (quadcolon (_CB m) (ivk t :: s)))
          | _ -> raise Matchintermstring_
          end
      | Subst (_, _, t, m) ->
          ivb t ::
            _T ivb ivk !substfix false t
               (quadcolon
                 (symbolstring SUBSTBRA)
                 (_TM ivb ivk m
                      (quadcolon (symbolstring SUBSTKET) (ivk t :: s))))
      | Binding stuff -> _T ivb ivk n a (remake mapterm stuff) s
      | Collection (_, c, es) ->
          ivb t ::
            quadcolon
              (_OB 0)
              (quadcolon
                 (unparseidclass c)
                 (quadcolon
                    " "
                    (_TT 0 true "," (List.map stripelement es)
                         (quadcolon (_CB 0) (ivk t :: s)))))
    and _TS ivb ivk ts r =
      match ts with
        [] -> r
      | [t] -> _T ivb ivk 0 true t r
      | t :: ts ->
          _T ivb ivk 0 true t (quadcolon "," (_TS ivb ivk ts r))
    and _TS1 ivb ivk m seps b ts s =
      match seps, ts with
        [ket], [] -> quadcolon ket s
      | [], [] ->(* special case for empty tuples *)
         s
      | sep :: seps, t :: ts ->
          (* normal case when as many seps as terms *)
          _T ivb ivk m b t
             (quadcolon sep (_TS1 ivb ivk m seps b ts s))
      | _ -> quadcolon "???_TS1???" s
    and _TS2 ivb ivk m seps b ts s =
      match seps, ts with
        [], [t] -> _T ivb ivk m b t s
      | sep :: seps, t :: ts ->
          _T ivb ivk m b t
             (quadcolon sep (_TS2 ivb ivk m seps b ts s))
      | _ -> quadcolon "???_TS2???" s
    and _TM ivb ivk vts s =
      let rec var (v, t) = v in
      let rec expr (v, t) = t in
      let (fst, snd) = if !substsense then var, expr else expr, var in
      _TS ivb ivk (List.map fst vts)
          (quadcolon (symbolstring SUBSTSEP) (_TS ivb ivk (List.map snd vts) s))
    let rec nobra _ = ""
    let catelim_termstring = _T nobra nobra 0 false
    let termstring = catelim2stringfn catelim_termstring
    let catelim_termstring_invisbracketed =
      _T (fun _ -> invisbra) (fun _ -> invisket) 0 false
    let termstring_invisbracketed =
      catelim2stringfn catelim_termstring_invisbracketed
    let rec catelim_termstring_invischoose ivb ivk = _T ivb ivk 0 false
    let rec termstring_invischoose ivb ivk =
      catelim2stringfn (catelim_termstring_invischoose ivb ivk)
    let rec catelim_vtsstring vts ss =
      quadcolon
        (symbolstring SUBSTBRA)
        (_TM nobra nobra vts (quadcolon (symbolstring SUBSTKET) ss))
    let vtsstring = catelim2stringfn catelim_vtsstring
    let rec catelim_argstring t ss =
      let rec mustbracket t =
        match t with
          Id _ -> false
        | Unknown _ -> false
        | Literal _ -> false
        | Fixapp (_, ss, _) ->
            begin match lookup (List.hd ss) with
              BRA _ -> false
            | _ -> true
            end
        | Binding stuff -> mustbracket (remake mapterm stuff)
        | Collection (_, c, _) -> true
        | Subst (_, _, p_, _) -> !substfix <= !appfix || mustbracket p_
        | _ -> true
      in
      if mustbracket t then
        quadcolon "(" (catelim_termstring t (quadcolon ")" ss))
      else catelim_termstring t ss
    let argstring = catelim2stringfn catelim_argstring
    let catelim_elementstring ooo = catelim_termstring (stripelement ooo)
    let elementstring = catelim2stringfn catelim_elementstring
    let catelim_elementstring_invisbracketed ooo =
      catelim_termstring_invisbracketed (stripelement ooo)
    let elementstring_invisbracketed =
      catelim2stringfn catelim_elementstring_invisbracketed
    let rec catelim_elementstring_invischoose ivb ivk ooo =
      catelim_termstring_invischoose ivb ivk (stripelement ooo)
    let rec elementstring_invischoose ivb ivk =
      catelim2stringfn (catelim_elementstring_invischoose ivb ivk)
    let rec catelim_collectionstring a1 a2 a3 =
      match a1, a2, a3 with
        sep, Collection (_, _, es), ss ->
          catelim_liststring catelim_elementstring sep es ss
      | sep, t, ss ->
          raise
            (Catastrophe_ ("collectionstring " :: catelim_termstring t []))
    let rec collectionstring sep =
      catelim2stringfn (catelim_collectionstring sep)
    let rec catelim_termOrCollectionstring sep t =
      match t with
        Collection _ -> catelim_collectionstring sep t
      | _ -> catelim_termstring t
    let rec termOrCollectionstring sep =
      catelim2stringfn (catelim_termOrCollectionstring sep)
    
    (* ------------------------------------------ the term store ------------------------------------------ *)
    
    (* this is an attempt to stop space explosions when reading in large proofs, and when rewriting.  It costs
     * in time, but we can't afford to use 64MB to read in 1.4MB of very repetitious tactic text.
     * RB 26/i/00
     *)
    (* After an initial experiment, light begins to dawn.  We shouldn't cache everything: it costs too much.
     * And we shouldn't cache stuff which includes unknowns, because it is ephemeral.  Ditto ResUnknowns.
     * So now I cache constant elements and constant collection and nothing else.  Computing the hash is
     * still not free, of course.
     * RB 27/i/00
     *)
    (* Initial impressions were wrong.  With large proofs, space is so important that we must cache everything.
     * RB 21/iii/00
     *)
    (* and of course, you idiot, what you should _really_ do is to make resources shared ... *)
    
    let termhashing = ref true
    let
      (hashcombine, hash_tepair, hash_telist, registerId, registerUnknown,
       registerApp, registerTup, registerLiteral, registerFixapp,
       registerSubst, registerBinding, registerCollection, registerElement,
       registerSegvar, resettermstore)
      =
	   (let module Secret =
		  struct
			let (cacheterm, resettermstore) =
			  simplecache "termstore"
				(fun (t, h, (t' : term)) ->
				   pairstring (string_of_int : int -> string) termstring ", " (h, t))
				127 (* why not? It can only grow :-> *)
				 (fun t -> t)
			let (cacheelement, resetelementstore) =
			  simplecache "elementstore"
				(fun (e, h, (e' : element)) ->
				   pairstring (string_of_int : int -> string) elementstring ", "
					 (h, e))
				127(* why not? It can only grow :-> *)
				 (fun e -> e)
		  end
		in
		let module Open =
		  struct
			open Secret 
			let rec hashcombine a1 a2 =
			  match a1, a2 with
				Some h1, Some h2 -> Some ((h1 lsl 1) lxor h2)
			  | _, _ -> None
			let rec hash_tepair f (a, b) = hashcombine (f a) (f b)
			let rec hash_telist f xs =
			  let rec hash a1 a2 =
				match a1, a2 with
				  [], h -> Some h
				| x :: xs, h -> andthenr (hashcombine (f x) (Some h), hash xs)
			  in
			  hash xs 0
			
			(* we only cache constant collections and elements. We may experiment, if this
			 * is a success, with caching Ids, since they are small, frequent and not very 
			 * diverse.
			 * RB 27/i/00
			 *)
			(* Profiling indicates that hashing constant collections and elements slows input down by 20%, 
			 * and hashing identifiers makes that 24%.  Hmmm.
			 * I won't make it a fixture till I profile some proof steps and proof reloads.
			 * RB 31/i/00
			 *)
			
			let registerId (s, c) =
			  if !termhashing then
				let h = hashstring s in cacheterm h (Id (Some h, s, c))
			  else Id (None, s, c)
			and registerUnknown (s, c) = Unknown (None, s, c)
			and registerApp (f, a as fa) =
			  match if !termhashing then hash_tepair hashterm fa else None with
				Some h as hash -> cacheterm h (App (hash, f, a))
			  | None -> App (None, f, a)
			and registerTup (s, ts) =
			  match
				if !termhashing then
				  hashcombine (Some (hashstring s)) (hash_telist hashterm ts)
				else None
			  with
				Some h as hash -> cacheterm h (Tup (hash, s, ts))
			  | None -> Tup (None, s, ts)
			and registerLiteral =
			  function
				String s as str ->
				  if !termhashing then
					let h = hashstring s in cacheterm h (Literal (Some h, str))
				  else Literal (None, str)
			  | Number n as num ->
				  if !termhashing then cacheterm n (Literal (Some n, num))
				  else Literal (None, num)
			and registerFixapp (ss, ts) =
			  match
				if !termhashing then
				  hashcombine (Some (hashlist hashstring ss))
					(hash_telist hashterm ts)
				else None
			  with
				Some h as hash -> cacheterm h (Fixapp (hash, ss, ts))
			  | None -> Fixapp (None, ss, ts)
			and registerSubst (r, t, vts) =
			  match
				if !termhashing then
				  hashcombine (hashterm t) (hash_telist (hash_tepair hashterm) vts)
				else None
			  with
				Some h as hash -> cacheterm h (Subst (hash, r, t, vts))
			  | None -> Subst (None, r, t, vts)
			and registerBinding ((bs, ss, us as inf), pat, body) =
			  match
				if !termhashing then
				  hashcombine (hashterm body)
					(hash_telist (hash_telist hashterm) [bs; ss; us])
				else None
			  with
				Some h as hash -> cacheterm h (Binding (hash, inf, pat, body))
			  | None -> Binding (None, inf, pat, body)
			and registerCollection (c, els) =
			  match if !termhashing then hash_telist hashelement els else None with
				Some h as hash -> cacheterm h (Collection (hash, c, els))
			  | None -> Collection (None, c, els)
			and registerElement (r, t) =
			  match
				if !termhashing then
				  match r with
					ResUnknown _ -> None
				  | _ -> hashcombine (Some (resnum2int r)) (hashterm t)
				else None
			  with
				Some h as hash -> cacheelement h (Element (hash, r, t))
			  | None -> Element (None, r, t)
			and registerSegvar (ms, v) =
			  match
				if !termhashing then hash_telist hashterm (v :: ms) else None
			  with
				Some h as hash -> cacheelement h (Segvar (hash, ms, v))
			  | None -> Segvar (None, ms, v)
			
			let resettermstore () = resettermstore (); resetelementstore () (* don't reset termhashing, please *)
		  end
		in
		Open.hashcombine, Open.hash_tepair, Open.hash_telist, Open.registerId, Open.registerUnknown,
		Open.registerApp, Open.registerTup, Open.registerLiteral, Open.registerFixapp,
		Open.registerSubst, Open.registerBinding, Open.registerCollection, Open.registerElement,
		Open.registerSegvar, Open.resettermstore
	  )
   
   (* ------------------------------ generic functions on terms and elements ------------------------------ *)

   (* These functions mop up after you have picked out the things of
    * interest with a special argument function (Some for caught, None for ignored).  
    * They go right through bindings and substitutions - unless the argument function catches that.
    * In fact they will even rewrite bindings, so watch out!
    *)
    let rec option_mapterm f t =
      ortryr
        (f t,
         (fun _ ->
            let mtff = option_mapterm f in
            let mtfl = option_rewritelist mtff in
            match t with
              Id _ -> None
            | Unknown _ -> None
            | App (_, f, a) ->
                andthenr
                  (option_rewrite2 mtff mtff (f, a),
                   (fun ooo -> Some (registerApp ooo)))
            | Tup (_, s, ts) ->
                andthenr (mtfl ts, (fun ts' -> Some (registerTup (s, ts'))))
            | Literal _ -> None
            | Fixapp (_, ss, ts) ->
                andthenr
                  (mtfl ts, (fun ts' -> Some (registerFixapp (ss, ts'))))
            | Subst (_, r, p_, vts) ->
                andthenr
                  (option_rewrite2 mtff
                     (option_rewritelist (option_rewrite2 mtff mtff))
                     (p_, vts),
                   (fun (p_', vts') -> Some (registerSubst (r, p_', vts'))))
            | Binding (_, bs_ss_us, env, pat) ->
                andthenr
                  (option_rewrite3 mtfl mtfl mtfl bs_ss_us,
                   (fun bs_ss_us' ->
                      Some (registerBinding (bs_ss_us', env, pat))))
            | Collection (_, k, es) ->
                andthenr
                  (option_mapelements f es,
                   (fun es' -> Some (registerCollection (k, es'))))))
    and option_mapelement a1 a2 =
      match a1, a2 with
        f, Segvar (_, ps, v) ->
          andthenr
            (option_mapterm f v, (fun v' -> Some (registerSegvar (ps, v'))))
      | f, Element (_, r, t) ->
          andthenr
            (option_mapterm f t, (fun t' -> Some (registerElement (r, t'))))
    (* yes, it should really be r *)

    and option_mapelements f = option_rewritelist (option_mapelement f)
    let rec mapterm f = anyway (option_mapterm f)
    let rec mapelements f = anyway (option_mapelements f)
    let rec foldterm f z t =
      match f (t, z) with
        Some v -> v
      | None ->
          let rec ff z ts = nj_fold (nj_foldterm f) ts z in
          match t with
            App (_, f, a) -> ff z [f; a]
          | Tup (_, _, ts) -> ff z ts
          | Fixapp (_, _, ts) -> ff z ts
          | Subst (_, _, p_, vts) ->
              ff (ff z (_MAP ((fun(_,hash2)->hash2), vts)))
                 (p_ :: _MAP ((fun(hash1,_)->hash1), vts))
          | Binding (_, (bs, ss, us), _, _) -> ff (ff (ff z us) ss) bs
          | Collection (_, k, es) -> foldelements f z es
          | _ -> z
    and nj_foldterm f (t, z) = foldterm f z t
    and foldelements f z es =
      let rec fe z e =
        match e with
          Element (_, _, t) -> foldterm f z t
        | Segvar (_, ms, v) -> nj_fold (nj_foldterm f) (v :: ms) z
      in
      (* not sure that's right ... *)
      nj_fold (fun (e, z) -> fe z e) es z
    let rec findterm g t =
      match g t with
        None ->
          let fx = findfirst (findterm g) in
          begin match t with
            App (_, f, a) -> fx [f; a]
          | Tup (_, s, ts) -> fx ts
          | Fixapp (_, ss, ts) -> fx ts
          | Binding (_, (bs, ss, us), _, _) ->
              ortryr (ortryr (fx bs, (fun _ -> fx ss)), (fun _ -> fx us))
          | Subst (_, r, p_, vts) ->
              ortryr
                (findterm g p_,
                 (fun _ -> findfirst (fun (v, t) -> fx [v; t]) vts))
          | Collection (_, k, es) ->
              let rec fe =
                function
                  Element (_, _, t) -> findterm g t
                | Segvar (_, ms, v) -> findfirst (findterm g) (v :: ms)
              in
              (* not sure that's right *)
              findfirst fe es
          | _ -> None
          end
      | v -> v
    (* what on earth does findhole do?  Some very clever person must have written this ... *)
    let rec findhole g t =
      let rec fh h t =
        let rec fhs sel build h xs =
          let rec fhx a1 a2 =
            match a1, a2 with
              h, [] -> None
            | h, x :: xs ->
                ortryr
                  (fh (fun ooo -> h ((fun t -> build x t :: xs) ooo)) (sel x),
                   (fun _ -> fhx (fun ooo -> h ((fun xs -> x :: xs) ooo)) xs))
          in
          fhx h xs
        in
        let rec selt t = t in
        let rec buildt _ t = t in
        match g h t with
          None ->
            begin match t with
              App (_, f, a) ->
                ortryr
                  (fh (fun ooo -> h ((fun f -> registerApp (f, a)) ooo)) f,
                   (fun _ ->
                      fh (fun ooo -> h ((fun a -> registerApp (f, a)) ooo))
                         a))
            | Tup (_, s, ts) ->
                fhs selt buildt
                  (fun ooo -> h ((fun ts -> registerTup (s, ts)) ooo)) ts
            | Fixapp (_, ss, ts) ->
                fhs selt buildt
                  (fun ooo -> h ((fun ts -> registerFixapp (ss, ts)) ooo)) ts
            | Binding (_, (bs, ss, us), env, pat) ->
                ortryr
                  (fhs selt buildt
                     (fun ooo ->
                        h ((fun ss ->
                              registerBinding ((bs, ss, us), env, pat))
                             ooo))
                     ss,
                   (fun _ ->
                      fhs selt buildt
                        (fun ooo ->
                           h ((fun us ->
                                 registerBinding ((bs, ss, us), env, pat))
                                ooo))
                        us))
            | Subst (_, r, p_, vts) ->
                ortryr
                  (fh (fun ooo ->
                         h ((fun p_ -> registerSubst (r, p_, vts)) ooo))
                      p_,
                   (fun _ ->
                      fhs (fun (v, t) -> t) (fun (v, _) t -> v, t)
                        (fun ooo ->
                           h ((fun vts -> registerSubst (r, p_, vts)) ooo))
                        vts))
            | Collection (_, k, es) ->
                let rec sele =
                  function
                    Element (_, _, t) -> t
                  | Segvar (_, ms, v) -> v
                in
                (* not really satisfactory *)
                let rec builde a1 a2 =
                  match a1, a2 with
                    Element (_, r, _), t -> registerElement (r, t)
                  | Segvar (_, ps, _), v -> registerSegvar (ps, v)
                in
                fhs sele builde
                  (fun ooo -> h ((fun es -> registerCollection (k, es)) ooo))
                  es
            | _ -> None
            end
        | res -> res
      in
      fh (fun t -> t) t
    let rec searchterm g z t =
      match findterm g t with
        Some v -> v
      | None -> z
    let rec existsterm g t =
      opt2bool (findterm (fun t -> if g t then Some true else None) t)
    (* at present we have a very specialised type hierarchy *)
    (* c1 specialisesto c2 if something of class c1 can, by process of unification
     * and/or instantiation, be made into a thing of class c1.  
     * If you like, it tests whether a c2-thing is a kind of c1-thing.
     *)
    
    let rec specialisesto (c1, c2) =
      c1 = c2 ||
      (match c1, c2 with
         FormulaClass, VariableClass -> true
       | FormulaClass, ConstantClass -> true
       | FormulaClass, NumberClass -> true
       | ConstantClass, NumberClass -> true
       | FormulaClass, StringClass -> true
       | ConstantClass, StringClass -> true
       | FormulaClass, OperatorClass -> true
       | ConstantClass, OperatorClass -> true
       | BagClass c1, BagClass c2 ->(* the lines above are essential at present, to allow (=), for example, to 
          * be unified with a FormulaClass unknown.  But I'm vaguely unhappy.  I 
          * think we need to be a bit more careful with connectives, and I guess I 
          * think that (=) should be ConstantClass or something, while (logand) 
          * can be a connective, and not at all a function.
          * RB May 95
          *)
          specialisesto (c1, c2)
       | ListClass c1, ListClass c2 -> specialisesto (c1, c2)
       | SubstClass, _ ->(* I think we need these too ... *)
          specialisesto (FormulaClass, c2)
       | _, SubstClass -> specialisesto (c1, FormulaClass)
       | _ -> false)
    (* specialises to is not adequate to catch canoccurfreein, because of Collection
     * formulae.  In future it will be even worse, when we have typed formulae ...
     *)
    
    let rec canoccurfreein (c1, c2) =
      specialisesto (c2, c1) ||
      (* this is wrong, actually: all they need is a common ancestor *)
      (match c2 with
         BagClass c -> canoccurfreein (c1, c)
       | ListClass c -> canoccurfreein (c1, c)
       | _ -> false)
    let rec idclass t =
      match debracket t with
        Id (_, _, c) -> c
      | Unknown (_, _, c) -> c
      | App _ -> FormulaClass
      | Tup _ -> FormulaClass
      | Literal (_, Number _) -> NumberClass
      | Literal (_, String _) -> StringClass
      | Fixapp _ -> FormulaClass
      | Subst (_, _, t, vts) ->
          begin match idclass t with
            VariableClass ->
              (* ohmygod *) 
              if _All
                   (fun t -> idclass t = VariableClass)
                   (List.map (fun(_,hash2)->hash2) vts)
              then
                VariableClass
              else SubstClass
          | c ->
              match debracket t with
                Unknown _ ->
                  if specialisesto (c, VariableClass) then SubstClass else c
              | _ -> c
          end
      | Binding _ -> FormulaClass
      | Collection (_, k, _) -> k
    let rec isSubstClass t = idclass t = SubstClass
    (* parse that, you bastards *)
    let termliststring = bracketedliststring termstring ","
    let catelim_termliststring =
      catelim_bracketedliststring catelim_termstring ","
    let rec replaceelement a1 a2 a3 =
      match a1, a2, a3 with
        Collection (_, c, es), (Element (_, r, _) as el), t ->
          let newel = registerElement (r, t) in
          let rec rep =
            function
              (Element (_, r', _) as el) :: es ->
                if r = r' then newel :: es else el :: rep es
            | el :: es -> el :: rep es
            | [] ->
                raise
                  (Catastrophe_
                     ["replaceelement: collection ";
                      bracketedliststring (smlelementstring termstring) ","
                        es;
                      " doesn't contain "; smlelementstring termstring el])
          in
          newel, registerCollection (c, rep es)
      | c, el, t ->
          raise
            (Catastrophe_
               ["replaceelement ("; smltermstring c; ") (";
                smlelementstring termstring el; ") ("; termstring t; ")"])
    let rec uniqueVID class__ sortedVIDs extraVIDs vid =
      let rec rootext vid =
        let vid' = String.sub (vid) (0) (String.length vid - 1) in
        let n = String.sub (vid) (String.length vid - 1) (1) in
        if ((isdigit n && vid' <> "") && isextensibleID vid') &&
           symclass vid' = class__
        then
          let (vid'', n') = rootext vid' in vid'', n' ^ n
        else vid, ""
      in
      let (rootVID, extVID) = rootext vid in
      let rec nextVID n = rootVID ^ string_of_int (n + 1) in
      let rec e_ vid n =
        if member (vid, extraVIDs) || symclass vid <> class__ then
          u_ (nextVID n) (n + 1) sortedVIDs
        else vid
      and u_ a1 a2 a3 =
        match a1, a2, a3 with
          vid, n, [] -> e_ vid n
        | vid, n, vid1 :: vids ->
            if vid < vid1 then e_ vid n
            else if vid = vid1 then
              let vid' = nextVID n in
              if vid1 < vid' then u_ vid' (n + 1) vids
              else u_ vid' (n + 1) sortedVIDs
            else u_ vid n vids
      in
      if isextensibleID vid then
        if symclass vid <> class__ then
          raise
            (Catastrophe_
               ["uniqueVID "; idclassstring class__; " ... "; vid;
                " (which is "; idclassstring (symclass vid); ")"])
        else u_ vid (if extVID = "" then 0 else atoi extVID) sortedVIDs
      else
        raise
          (Catastrophe_ ["uniqueVID "; idclassstring class__; " ... "; vid])
    let mergeVIDs = sortedmerge (<)
    let rec earliervar t1 t2 =
      let rec ordinal =
        function
          Id _ -> 0
        | Unknown _ -> 1
        | _ -> 2
      in
      match debracket t1, debracket t2 with
        Id (_, v1, _), Id (_, v2, _) -> v1 < v2
      | Unknown (_, v1, _), Unknown (_, v2, _) -> v1 < v2
      | t1, t2 -> ordinal t1 < ordinal t2
    
    let rec mergevars xs ys = sortedmerge earliervar xs ys

    (* In order to be able to compare or unify two maps,
       we must have a canonical order (sigh!).
     *)

    let rec canonicalsubstmap vts =
      let rec earliervt (v, _) (v', _) = earliervar v v' in
      sort earliervt vts
    let rec isconstantID (_, _, c) =
      match c with
        ConstantClass -> true
      | NumberClass -> true
      | StringClass -> true
      | OperatorClass -> true
      | _ -> false
    (* used (now only in proviso) to tell if we have a 'variable' or a 'constant' *)
    let rec isconstant t =
      match debracket t with
        Id s -> isconstantID s
      | Unknown s -> isconstantID s
      | _ -> false
    (* This is a replacement for uses of isconstant, isoperator and the like which
       attempted to tell the difference between 'made up' names like x, y, z and 
       'constants' like map, fold, (+).
       
       A 'meta variable' is an implicitly (meta-) quantified
       variable which occurs in a rule or theorem. We look for them
       in (at least) the following situations:
       
         when matching terms (for example in the UNFOLD/FOLD search)
         when doing LET bindings
         when (in EVALUATE) finding the fresh unknowns and object variables
         in a sequent (things which appear in the consequent but not in
         the hypothesis).
       
       An operator or an identifier which has been declared to be a constant
       can not be a meta variable.
       
       An identifier which has been declared to be of
       some identifier CLASS is a meta variable.
       
       The implementation of Symbol distinguishes
       between CONSTANT foo and CLASS CONSTANT foo only by
       making the former answer false to isextensibleID whereas
       the latter answers true. 
       
       BAS September 5th 1996 (slightly edited RB 10/ix/96).
     *)

    let rec ismetav t =
      match debracket t with
        Id (_, s, c) -> isextensibleID s
      | Unknown _ -> true
      | _ -> false
    let rec isextensibleId t =
      match debracket t with
        Id (_, s, c) -> isextensibleID s
      | _ -> false
    let rec isId t =
      match debracket t with
        Id _ -> true
      | _ -> false
    let rec isUnknown t =
      match debracket t with
        Unknown _ -> true
      | _ -> false
    let rec isVariable t =
      match debracket t with
        Id (_, _, VariableClass) -> true
      | Unknown (_, _, VariableClass) -> true
      | _ -> false
    let rec isleaf t =
      match t with
        Id _ -> true
      | Unknown _ -> true
      | Literal _ -> true
      | _ -> false
    let rec isleafelement e =
      match e with
        Segvar (_, [], v) -> isleaf v
      | Element (_, _, t) -> isleaf t
      | _ -> false
    let rec isidentifier t =
      match t with
        Id _ -> true
      | Unknown _ -> true
      | _ -> false
    let rec issegvar e =
      match e with
        Segvar _ -> true
      | _ -> false
    let rec isCollection =
      function
        Collection _ -> true
      | _ -> false
    let rec isemptycollection =
      function
        Collection (_, _, []) -> true
      | _ -> false
    let rec isselectionSubst e =
      match e with
        Element (_, _, Subst (_, false, _, _)) -> true
      | _ -> false
    let rec emptycollection k = registerCollection (k, [])
    let rec element2term =
      function
        Element (_, _, t) -> Some t
      | _ -> None
    let rec collection2term =
      function
        Collection (_, _, [el]) -> element2term el
      | _ -> None
    (* This function would be simple equality, were it not for the debracketing.
     * Now perhaps the right thing would be a stripbracket function ... but no.
     * It might be more economically expressed if it used |||, but this way 
     * (perhaps) is faster.
     *)
   
    let rec eqterms (t1, t2) =
      let rec fEQs =
        function
          t1 :: t1s, t2 :: t2s -> eqterms (t1, t2) && fEQs (t1s, t2s)
        | [], [] -> true
        | _ -> false
      in
      match debracket t1, debracket t2 with
        Id (_, s1, c1), Id (_, s2, c2) ->(* first the ones which alter the interpretation *)
         s1 = s2 && c1 = c2
      | Unknown (_, s1, c1), Unknown (_, s2, c2) -> s1 = s2 && c1 = c2
      | App (_, f1, a1), App (_, f2, a2) ->
          eqterms (f1, f2) && eqterms (a1, a2)
      | Literal (_, k1), Literal (_, k2) -> k1 = k2
      | Tup (_, s1, t1s), Tup (_, s2, t2s) -> s1 = s2 && fEQs (t1s, t2s)
      | Fixapp (_, ss1, t1s), Fixapp (_, ss2, t2s) ->
          ss1 = ss2 && fEQs (t1s, t2s)
      | Subst (_, _, p1, vts1), Subst (_, _, p2, vts2) ->
          let vts1 = canonicalsubstmap vts1 in
          let vts2 = canonicalsubstmap vts2 in
          let rec fEQvts =
            function
              (v1, t1) :: vts1, (v2, t2) :: vts2 ->
                (eqterms (v1, v2) && eqterms (t1, t2)) && fEQvts (vts1, vts2)
            | [], [] -> true
            | _ -> false
          in
          eqterms (p1, p2) && fEQvts (vts1, vts2)
      | Binding (_, (bs, ss, us), _, pat),
        Binding (_, (bs', ss', us'), _, pat') ->
          ((pat = pat' && fEQs (sort earliervar bs, sort earliervar bs')) &&
           fEQs (ss, ss')) &&
          fEQs (us, us')
      | Collection (_, BagClass k1, es1), Collection (_, BagClass k2, es2) ->
          k1 = k2 && eqbags (eqelements eqterms) (es1, es2)
      | Collection (_, ListClass k1, es1),
        Collection (_, ListClass k2, es2) ->
          k1 = k2 && eqlists (eqelements eqterms) (es1, es2)
      | _ -> false
    and eqelements eq (e1, e2) =
      match e1, e2 with
        Segvar (_, p1s, v1), Segvar (_, p2s, v2) ->
          eqlists eqterms (p1s, p2s) && eq (v1, v2)
      | Element (_, r1, t1), Element (_, r2, t2) -> eq (t1, t2)
      | _ ->(* ignore resource numbers *)
         false
    let rec sameresource (e1, e2) =
      match e1, e2 with
        Element (_, r1, _), Element (_, r2, _) -> r1 = r2
      | _ -> e1 = e2
    (* we don't analyse Segvars *)
      
    let rec earlierresource (e1, e2) =
      match e1, e2 with
        Element (_, r1, _), Element (_, r2, _) ->
          begin match r1, r2 with
            Resnum i1, Resnum i2 -> i1 < i2
          | ResUnknown i1, ResUnknown i2 -> i1 < i2
          | Resnum _, Nonum -> true
          | Resnum _, ResUnknown _ -> true
          | ResUnknown _, Nonum -> true
          | _ -> false
          end
      | _ -> false
    (* who cares ? *)
    (* this is an alpha-conversion-capable version of eqterms.
       tbs is a binding (a mapping from vars to numbers).
       Because substitutions are a kind of binding, an earlier version tried
       to simplify them on the fly: it wasn't a success.  This version will
       work provided that all the substitutions with which it is presented are 
       maximally reduced.
       
       It occurs to me that we could, now this function only considers 
       alpha-conversion, modify it so that it is independent of the order in
       which variables are declared in a binding.  But one step at a time ...
     *)
    let eqalphadebug = ref false
    let rec eqalphaterms (t1, t2) =
      let count = ref 0 in
      let rec nxb _ = incr count; !count in
      (* infix at confuses OCaml *)
      let rec at (tb, v) =
        match tb with
          (v', n) :: tb -> if v = v' then Some n else at (tb, v)
        | [] -> None
      in
      let rec eq t1bs t2bs (t1, t2) =
        let fEQ = eq t1bs t2bs in
        let rec fEQs =
          function
            t1 :: t1s, t2 :: t2s -> fEQ (t1, t2) && fEQs (t1s, t2s)
          | [], [] -> true
          | _ -> false
        in
        let rec doublev () =
          match at (t1bs, t1), at (t2bs, t2) with
            Some n1, Some n2 -> n1 = n2
          | None, None -> t1 = t2
          | _ -> false
        in
        match debracket t1, debracket t2 with
          Id _, _ ->(* first the ones which alter the interpretation *)
           doublev ()
        | _, Id _ -> doublev ()
        | Unknown _, _ -> doublev ()
        | _, Unknown _ -> doublev ()
        | App (_, f1, a1), App (_, f2, a2) -> fEQ (f1, f2) && fEQ (a1, a2)
        | Literal (_, k1), Literal (_, k2) -> k1 = k2
        | Tup (_, s1, t1s), Tup (_, s2, t2s) -> s1 = s2 && fEQs (t1s, t2s)
        | Fixapp (_, ss1, t1s), Fixapp (_, ss2, t2s) ->
            ss1 = ss2 && fEQs (t1s, t2s)
        | Subst (_, _, p1, vts1), Subst (_, _, p2, vts2) ->
            let vts1 = canonicalsubstmap vts1 in
            let vts2 = canonicalsubstmap vts2 in
            let rec fEQvts =
              function
                (v1, t1) :: vts1, (v2, t2) :: vts2 ->
                  (fEQ (v1, v2) && fEQ (t1, t2)) && fEQvts (vts1, vts2)
              | [], [] -> true
              | _ -> false
            in
            fEQ (p1, p2) && fEQvts (vts1, vts2)
        | Binding (_, (bs, ss, us), _, pat),
          Binding (_, (bs', ss', us'), _, pat') ->
            let ns = _MAP (nxb, bs) in
            begin try
              (pat = pat' && fEQs (us, us')) &&
              _All
                (eq (( ||| ) (bs, ns) @ t1bs) (( ||| ) (bs', ns) @ t2bs))
                (( ||| ) (ss, ss'))
            with
              Zip -> false
            end
        | Collection (_, BagClass k1, es1),
          Collection (_, BagClass k2, es2) ->
            (* handling may not be necessary for the moment ...*)
            k1 = k2 && eqbags (eqelements fEQ) (es1, es2)
        | Collection (_, ListClass k1, es1),
          Collection (_, ListClass k2, es2) ->
            k1 = k2 && eqlists (eqelements fEQ) (es1, es2)
        | _ -> false
      in
      let r = t1 = t2 || eq [] [] (t1, t2) in(* if !eqalphadebug then
          consolereport["eqalphaterms (", termstring t1, ",",
                        termstring t2, ") => ", string_of_int r
                       ]
         else ();
       *)
       r
    (* we are only interested in a definite answer here, in order to turn a Maybe 
     * into a Yes or a No.
     * This function doesn't use binding structure: it is looking for an occurrence 
     * of a sub-tree, for use in unification and equality testing.  It tells you 
     * whether in *every* unification/instantiation, t occurs in P.
     * RB 20/i/93
     *)
    let rec termoccursin t = fun p_ -> existsterm (curry2 eqterms t) p_
    (* termvars takes no note of bindings, and makes no interpretation of maps. 
     * No longer cat-eliminated, produces a sorted list of names.
     *)
    
    let tmerge = sortedmerge earliervar
    let rec termvars t =
      sortunique earliervar
        (foldterm
           (function
              (Id _ as v), vs -> Some (v :: vs)
            | (Unknown _ as v), vs -> Some (v :: vs)
            | _ -> None)
           [] t)
    (*-------------------------- stuff about bindings ------------------------------ *)

    (* The functions which follow are intended to help in minimising the need for 
     * NOTIN provisos.  To begin with we consider what binds what in varbindings. 
     * RB 15/iv/96
     *)

    let varbindingsdebug = ref false
    let bcstring = bracketedliststring termliststring ","
    let bcliststring = bracketedliststring bcstring ","
    let varinfstring = pairstring termstring bcliststring ","
    let varbindingsresstring = bracketedliststring varinfstring ","
    let bvsorder = earlierlist earliervar
    let bcorder = earlierlist bvsorder
    let rec relorder (v1, _) (v2, _) = earliervar v1 v2
    let rec combinebindings (x, bcs) (_, bcs') =
      x, sortedmerge bcorder bcs bcs'
    let bmerge = sortedmergeandcombine relorder combinebindings
    (* This computes a list of variables in t, each with various binding
     * contexts. A binding context is a list of lists of binders, so each
     * variable is paired with a list of lists of lists of binders.
     * RB 19/xi/97
     *)
    let rec varbindings t =
      let rec doit outers inners t = foldterm (f outers inners) [] t
      and f outers inners (t, ps) =
        let binders =
          if null outers then if null inners then [] else [inners]
          else inners :: outers
        in
        let dothem = List.map (doit outers inners) in
        let rec v () =
          if canoccurfreein (VariableClass, idclass t) then
            Some (bmerge [t, [binders]] ps)
          else Some ps
        in
        let rec doit2 bs = doit binders (sort earliervar bs) in
        let r =
          match t with
            Id _ -> v ()
          | Unknown _ -> v ()
          | Subst (_, _, p_, vts) ->
              Some
                (nj_fold (uncurry2 bmerge)
                   (doit2 (_MAP ((fun(hash1,_)->hash1), vts)) p_ ::
                      dothem (_MAP ((fun(_,hash2)->hash2), vts)))
                   ps)
          | Binding (_, (bs, ss, us), _, _) ->
              Some
                (nj_fold (uncurry2 bmerge) (_MAP (doit2 bs, ss))
                   (nj_fold (uncurry2 bmerge) (dothem us) ps))
          | _ -> None
        in
        if !varbindingsdebug then
          begin match r with
            Some _ ->
              consolereport
                ["(varbindings) f "; termliststring inners; " ";
                 bracketedliststring termliststring "," outers; " ";
                 pairstring termstring varbindingsresstring "," (t, ps);
                 " => "; optionstring varbindingsresstring r]
          | None -> ()
          end;
        r
      in
      doit [] [] t
    let combinemappings (x, vs) (_, vs') = x, tmerge vs vs'
    let mmerge = sortedmergeandcombine relorder combinemappings
    let rec fmerge (fvs, m) (fvs', m') = tmerge fvs fvs', mmerge m m'
    (* this function finds identifiers that occur free, those of which we have
     * 'lost control', and what binds what otherwise.
     * In the case of bindings which are distinct only because of the order of
     * binding (e.g. Ax.Ay.P and Ay.Ax.P) it is necessary to find which variable
     * 'dominates' which in each list, and to add binding pairs accordingly.
     * Phew.  RB 18/iv/96
     *)

     (* This thing needed a rewrite, because it was obscure and uncommented and didn't
      * work.  So it got one.  Here is what it does, and there are comments about how it
      * does it. (Please note that the treatment here was developed before the
      * introduction of 'hidden provisos' generated from the interpretation of binding
      * structures and predicate notation. That later treatment means that some of the
      * data gathered here is over-cautious, but that doesn't matter, because when we
      * have a proviso saying that two names are independent, we believe it no matter
      * what the data structure prepared here says.  However, steps 3 and 4 below are 
      * far too conservative when we are interpreting predicates, and lead to far too 
      * many binders being treated as 'bad'.  So we have added a parameter which 
      * details those NOTIN provisos which are in force.)
      * 
      * The data structure built here is added to the context by rewritecxt (see
      * rewrite.sml).  It's interpreted in exterioreqvarsq (see facts.sml).
      * 
      * We get (indirectly from varbindings) a list of names, each paired with a list of
      * binding contexts; each binding context is a list of lists of names.  Each element
      * of a binding context is a 'parallel binding' - what you get from something like
      * Ax,y.P.  So if you have Ax,y.Eu,v.P what is recorded for P is the binding context
      * [[u,v],[x,y]].
      * 
      * Now the question that we want to solve is this: are there names in the 'exterior'
      * of a theorem/derived rule -- that is, in the base sequent plus any givens --
      * which can be treated as independent, because if we get an instance of the theorem
      * or rule in which they happen to be the same, we can alpha-convert them apart?
      * 
      * We solve this question by constructing a data structure which tells us the names
      * that are not necessarily independent.  Unfortunately the question applies not
      * only to variables like x, y, z but also to formulas like P, Q, R.  That
      * complicates the issue: we need to know not only when x and z can be considered
      * distinct, but also when x can be considered not to occur free in P.  Luckily
      * these questions are closely related, but it does mean that you have to pay rather
      * close attention to some details.
      * 
      * Step 1. Clearly, names which appear free are not independendent. So we should
      *     accumulate names which appear free.  We can add to the free names those
      *     formula names which appear bound: if we have free z and also we have Ax.P,
      *     then we can't say that z doesn't appear free in P.
      * 
      * Step 2. If we have Ax.P, then clearly P may contain occurrences of x: we record a
      *     relation between binders like x and the names which they 'dominate' like P.
      * 
      * Step 3. If we have Ax.Ay.P(x,y) and also Ay.Ax.P(x,y) then there is a problem. We
      *     can envisage an instance of the theorem/rule in which x and y happen to be
      *     the same, but then we won't be able to alpha-convert them apart, because in
      *     one case the ys are captured and in the other the xs are captured.  So if
      *     there is one binding in which x dominates y and another in which y dominates
      *     x, both pairs are added to the mapping of step 2.
      * 
      * Step 4. If we have Ax.P and Ay.P then y can occur free (case 1) and so can x
      *     (case 2).  This doesn't apply, clearly, to Ax.z and Ay.z, so we restrict it
      *     to cases in which what is dominated isn't a variable.  Anyway, if we find it
      *     we add x and y to the list of 'free' variables from step 1.
      * 
      * The result is a list of 'free' variables -- if two names are in that list then
      * they can't be judged independent -- plus a 'dominates' relation -- if x dominates
      * P then x NOTIN P can't be ignored.  But if x and y are two variables which aren't
      * both in the 'free' variable list and neither dominates the other, then they can
      * be treated as independent and distinct.  And if x and P aren't both in the free
      * variable list and x doesn't dominate P, then x NOTIN P can be considered
      * satisfied.  And that's really the point: to allow simplification of substitutions
      * involving schematic but different names like x and y, and to eliminate provisos
      * ditto.
      * 
      * RB 20/xi/97
      *)

    let rec freevarsfrombindings inf notins =
      (* we assume that inf is sorted by variable and that each bcs is sorted by bcorder *)
      let rec vvorder (a, b) (c, d) =
        earliervar a c || a = c && earliervar b d
      in
      let rec closed v bc = List.exists (fun bvs -> member (v, bvs)) bc in
      (* step 1 - find all free and semi-free names *)
      let rec freev (v, bcs) = List.exists (fun ooo -> not (closed v ooo)) bcs in
      let freevs = _MAP ((fun(hash1,_)->hash1), ( <| ) (freev, inf)) in
      (* step 2 - find the dominates relation from all the bindings *)
      let rec domf (v, bcs) =
        let rec truncate bc =
          takewhile (fun bvs -> not (member (v, bvs))) bc
        in
        let bcs = _MAP (truncate, bcs) in
        let dominators = nj_fold (fun (bc, ds) -> nj_fold (uncurry2 tmerge) bc ds) bcs [] in
        _MAP ((fun bv -> bv, [v]), dominators)
      in
      let domrel = nj_fold (uncurry2 mmerge) (_MAP (domf, inf)) [] in
      (* step 3 - find all the pairs of names which occur in bindings
       * both ways round, add to domrel
       *)
      let allbindings =
        nj_fold (uncurry2 (sortedmerge bcorder)) (_MAP ((fun(_,hash2)->hash2), inf)) []
      in
      let rec allbpairs bc =
        let vpss =
          List.map (fun (us, vs) -> sort vvorder (( >< ) us vs)) (allpairs bc)
        in
        List.filter (fun (x, y) -> x <> y) (nj_fold (uncurry2 (sortedmerge vvorder)) vpss [])
      in
      let vps =
        nj_fold (uncurry2 (sortedmerge vvorder)) (List.map allbpairs allbindings) []
      in
      let rvps = sort vvorder (_MAP ((fun (x, y) -> y, x), vps)) in
      let commonpairs =
        ( <| )
          ((fun (x, y) ->
              not (member ((x, y), notins) || member ((y, x), notins))),
           sortedsame vvorder vps rvps)
      in
      let domrel =
        nj_fold (uncurry2 mmerge) (_MAP ((fun (x, y) -> [x, [y]; y, [x]]), commonpairs))
          domrel
      in
      (* step 4 - find pairs of bindings which aren't the same, add their 
       * differences to domrel.
       *
       * This could be simplified by using VariableClass - we aren't interested in
       * bindings where the dominated is simply a variable - but the code here
       * will be more robust when the notion of idclass gets more intricate
       *)
      let rec badbindings (v, bcs) =
        let bcs = ( <| ) ((fun ooo -> not (closed v ooo)), bcs) in
        let vclass = idclass v in
        let rec allbvs bc =
          let rec filterbv bv =
            let bvclass = idclass bv in
            bvclass <> vclass && canoccurfreein (bvclass, vclass)
          in
          nj_fold (uncurry2 tmerge) (_MAP ((fun bvs -> ( <| ) (filterbv, bvs)), bc)) []
        in
        let bvss = _MAP (allbvs, bcs) in
        let rec escapers bvs =
          ( <| ) ((fun bv -> not (member ((bv, v), notins))), bvs)
        in
        let rec b2 ((bv1s, bv2s), bads) =
          nj_fold (uncurry2 tmerge)
            [escapers (sorteddiff earliervar bv1s bv2s);
             escapers (sorteddiff earliervar bv2s bv1s)]
            bads
        in
        nj_fold b2 (allpairs bvss) []
      in
      let freevs = nj_fold (uncurry2 tmerge) (_MAP (badbindings, inf)) freevs in
      (* and there we have it *)
      let r = freevs, domrel in
      (* stuff to persuade ourselves we have it right :-) *)
      let showin = varinfstring in
      let showout =
        pairstring termliststring
          (fun ooo -> mappingstring termstring termliststring (mkmap ooo)) ","
      in
      if !varbindingsdebug then
        consolereport
          ["freevarsfrom bindings "; bracketedliststring showin ", " inf; " ";
           bracketedliststring (pairstring termstring termstring ",") ","
             notins;
           " => "; showout r];
      r
    let orderVIDs = sortunique (<)
    (* should be identifiertoVID *)
    let rec vartoVID =
      function
        Id (_, v, _) -> v
      | Unknown (_, v, _) -> v
      | t -> raise (Catastrophe_ ["vartoVID "; argstring t])
    let rec termVIDs t = orderVIDs (_MAP (vartoVID, termvars t))
    let rec conVIDs ts =
      nj_fold
        (function
           Id (_, v, c), vs -> v :: vs
         | _, vs -> vs)
        ts []
    (* --------------------------------------------------------------------- *)

       (* could t1 be changed by unification/substitution so that it becomes the same term as t2, 
        * and/or vice-versa? 
        *)
    let rec simterms (t1, t2) =
      let rec similar t1subst t1 t2subst t2 =
        let (t1, t2) = debracket t1, debracket t2 in
        let rec sim t1 t2 = similar t1subst t1 t2subst t2 in
        let rec sims t1s t2s =
          try _All (uncurry2 sim) (( ||| ) (t1s, t2s)) with
            Zip -> false
        in
        let rec reverse () = similar t2subst t2 t1subst t1 in
        let rec lsub () = t1subst && idclass t1 = VariableClass in
        let rec rsub () = t2subst && idclass t2 = VariableClass in
        let rec luni () =
          specialisesto (idclass t1, idclass t2) ||
          t1subst && specialisesto (idclass t1, VariableClass)
        in
        let rec runi () =
          specialisesto (idclass t2, idclass t1) ||
          t2subst && specialisesto (idclass t2, VariableClass)
        in
        if t1 = t2 then true
        else
          match t1, t2 with
            Id _, Id _ -> lsub () || rsub ()
          | Id _, Unknown _ -> lsub () || runi ()
          | Id _, Subst (_, _, p2, _) -> lsub () || similar t1subst t1 true p2
          | Id _, _ -> lsub ()
          | Unknown _, Id _ -> reverse ()
          | Unknown _, Unknown _ -> true
          | Unknown _, Subst (_, _, p2, _) ->
              (* need a notion of common subclass to go farther, don't have it *)
              (luni () || lsub ()) || similar t1subst t1 true p2
          | Unknown _, _ -> luni ()
          | Subst _, Id _ -> reverse ()
          | Subst _, Unknown _ -> reverse ()
          | Subst (_, _, p1, _), Subst (_, _, p2, _) ->
              similar true p1 true p2
          | Subst (_, _, p1, _), _ -> similar true p1 t2subst t2
          | _, Id _ -> reverse ()
          | _, Unknown _ -> reverse ()
          | _, Subst _ -> reverse ()
          | App (_, f1, a1), App (_, f2, a2) -> sim f1 f2 && sim a1 a2
          | Tup (_, s1, t1s), Tup (_, s2, t2s) -> s1 = s2 && sims t1s t2s
          | Literal (_, l1), Literal (_, l2) -> l1 = l2
          | Fixapp (_, ss1, t1s), Fixapp (_, ss2, t2s) ->
              (* false by now, really, but who cares? *)
              ss1 = ss2 && sims t1s t2s
          | Binding stuff, Binding stuff' ->
              sim (remake mapterm stuff) (remake mapterm stuff')
          | Collection (_, k, _), Collection (_, k', _) -> k = k'
          | _ ->(* otherwise too hard *)
             false
      in
      similar false t1 false t2
    (*------------------------------------------------------------------------------*)
    
    (* find the 'function' and 'arguments' in a curried or uncurried 'application'; 
     * strip brackets from all; give back a pair of function and argument list. 
     * Even works for null-argument 'applications'. It's sketchy, and doesn't protect
     * against silly arguments.
     * Now augmented with its converse.
     *)
    
    let rec explodeApp curry t =
      let rec unApp t rs =
        match t with
          App (_, l, r) -> unApp (debracket l) (debracket r :: rs)
        | _ -> t, rs
      in
      match curry, unApp (debracket t) [] with
        true, (f, [Tup (_, ",", rs)]) -> f, _MAP (debracket, rs)
      | _, res -> res
    let rec implodeApp curry (t, args) =
      if curry then registerApp (t, registerTup (",", args))
      else nj_revfold (fun (r, l) -> registerApp (l, r)) args t
    (* find the various ways in which a term can be a binary operation (sigh) *)
    let rec explodebinapp t =
      match debracket t with
        App (_, Id (_, s, OperatorClass), Tup (_, ",", [e; f])) ->
          Some (e, s, f)
      | App (_, App (_, Id (_, s, OperatorClass), e), f) -> Some (e, s, f)
      | Tup (_, ",", _) -> None
      | Tup (_, s, [e; f]) -> Some (e, s, f)
      | _ -> None
    (* ---------- for export ------------ *)
    
    let rec int2term (i : int) = registerLiteral (Number i)
    let rec term2int t =
      try
        match debracket t with
          Literal (_, Number n) -> n
        | App (_, Id (_, "~", NoClass), t') -> - term2int t'
        | _ ->(* can happen, and NoClass is important ... *)
           raise (Catastrophe_ [])
      with
        _ -> raise (Catastrophe_ ["term2int "; smltermstring t])
    let rec enbracket t = registerFixapp (["("; ")"], [t])
    let rec explodeCollection =
      function
        Collection (_, _, es) -> es
      | t -> [registerElement (Nonum, t)]
    let rec augmentCollection a1 a2 =
      match a1, a2 with
        Collection (_, kind, es), els ->
          Some (registerCollection (kind, es @ els))
      | _, _ -> None
    let rec decodeSubst =
      function
        Subst (_, r, p_, vts) -> Some (r, p_, vts)
      | _ -> None
    let rec decodeBinding =
      function
        Binding (_, info, _, _) -> Some info
      | _ -> None
    let rec decodeBracketed =
      function
        Fixapp (_, ["("; ")"], [t]) -> Some t
      | _ -> None
  end

(* module type Termtype = sig type vid=M.vid and idclass=M.idclass
						   include Termtype
					   end *)