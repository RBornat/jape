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

open Termtype

open Stringfuns
open Sml
open Listfuns
open Miscellaneous
open Symbol
open Symboltype
open Optionfuns
open Idclass
open Mappingfuns
open Idclassfuns

let invisbra = String.make 1 offbra
and invisket = String.make 1 offket

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
		  (",\"" :: string_of_vid v :: "\"," :: idclassstring c :: ")" :: tail)
  | Unknown (h, v, c) ->
	  "Unknown(" ::
		catelim_optionstring (stringfn2catelim string_of_int) h
		  (",\"" :: string_of_vid v :: "\"," :: idclassstring c :: ")" :: tail)
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
	  match (emap <@> t) with
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
	  | Tup (_, s, ts) -> Tup (None, s, (mtf <* ts))
	  | Fixapp (_, ss, ts) -> Fixapp (None, ss, (mtf <* ts))
	  | Binding (_, (bs, ss, us), env, pat) ->
		  Binding
			(None, ((mtf <* bs), (mtf <* ss), (mtf <* us)), env,
			 pat)
	  | Subst (_, r, p_, vts) ->
		  Subst (None, r, mtf p_, (fun (v, t) -> mtf v, mtf t) <* vts)
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
	Id (_, v, _), _, _ -> Some (string_of_vid v)
  | _, true, Id (_, v, _) -> Some (string_of_vid v)
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
	  let sv = string_of_vid v in
	  (match lookup sv with
		INFIX _ ->
		  ivb t ::
			quadcolon "(" (quadcolon sv (quadcolon ")" (ivk t :: s)))
	  | INFIXC _ ->
		  ivb t ::
			quadcolon "(" (quadcolon sv (quadcolon ")" (ivk t :: s)))
	  | PREFIX _ ->
		  ivb t ::
			quadcolon "(" (quadcolon sv (quadcolon ")" (ivk t :: s)))
	  | POSTFIX _ ->
		  ivb t ::
			quadcolon "(" (quadcolon sv (quadcolon ")" (ivk t :: s)))
	  | _ -> ivb t :: quadcolon sv (ivk t :: s))
  | Unknown (_, v, _) -> ivb t :: quadcolon (metachar ^ (string_of_vid v)) (ivk t :: s)
  | App (_, (App (_, f, arg1) as l), arg2) ->
	  begin match
		(opname f &~~
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
		(opname f &~~
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

let nobra   _ = ""
let noket   _ = ""
let showbra _ = invisbra
let showket _ = invisket

let catelim_termstring_invisbracketed b = 
	(if b then _T showbra showket else _T nobra noket) 0 false
let termstring_invisbracketed =
  catelim2stringfn <.> catelim_termstring_invisbracketed

let catelim_termstring = catelim_termstring_invisbracketed false
let termstring = catelim2stringfn catelim_termstring

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

let catelim_elementstring_invisbracketed b =
  catelim_termstring_invisbracketed b <.> stripelement
let elementstring_invisbracketed =
  catelim2stringfn <.> catelim_elementstring_invisbracketed

let catelim_elementstring = catelim_elementstring_invisbracketed false
let elementstring = catelim2stringfn catelim_elementstring

let rec catelim_elementstring_invischoose ivb ivk =
  catelim_termstring_invischoose ivb ivk <.> stripelement
let rec elementstring_invischoose ivb ivk =
  catelim2stringfn (catelim_elementstring_invischoose ivb ivk)

let catelim_collectionstring_invisbracketed b sep t =
  match t with
	Collection (_, _, es) -> 
	  catelim_liststring (catelim_elementstring_invisbracketed b) sep es
  | _ -> raise (Catastrophe_ ("collectionstring " :: catelim_termstring t []))
let collectionstring_invisbracketed b sep = 
  catelim2stringfn (catelim_collectionstring_invisbracketed b sep)

let catelim_collectionstring = catelim_collectionstring_invisbracketed false
let collectionstring sep = catelim2stringfn (catelim_collectionstring sep)

let rec catelim_termOrCollectionstring_invisbracketed b sep t =
  match t with
	Collection _ -> catelim_collectionstring_invisbracketed b sep t
  | _ -> catelim_termstring_invisbracketed b t
let rec termOrCollectionstring_invisbracketed b sep =
  catelim2stringfn (catelim_collectionstring_invisbracketed b sep)

let catelim_termOrCollectionstring = catelim_termOrCollectionstring_invisbracketed false
let termOrCollectionstring sep = catelim2stringfn (catelim_collectionstring sep)

let termliststring = bracketedliststring termstring ","
let catelim_termliststring = catelim_bracketedliststring catelim_termstring ","
