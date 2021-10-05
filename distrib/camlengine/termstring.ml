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

open Termtype

open Stringfuns
open Sml
open Listfuns
open Miscellaneous
open Symbol
open Symboltype
open Optionfuns
open Idclass
open Invisibles
open Mappingfuns
open Idclassfuns
open UTF

let invisbra = offbra_as_string
and invisket = offket_as_string

let termstringdebug = ref false

let utpecua = ref false (* un train peut en cacher un autre *)

(************** printing out internal structure of term *************)
 
let rec catelim_string_of_resnum r tail =
  match r with
  | Nonum        -> "Nonum" :: tail
  | Resnum     r -> "Resnum " :: string_of_int r :: tail
  | ResUnknown r -> "ResUnknown " :: string_of_int r :: tail

let string_of_resnum = stringfn_of_catelim catelim_string_of_resnum

let rec dolist f = catelim_bracketedstring_of_list f ";"

(* for those who need to know *exactly* what they have got *)
let rec catelim_debugstring_of_term t tail =
  match t with
    Id (h, v, c) ->
      "Id(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          (",\"" :: string_of_vid v :: "\"," :: string_of_idclass c :: ")" :: tail)
  | Unknown (h, v, c) ->
      "Unknown(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          (",\"" :: string_of_vid v :: "\"," :: string_of_idclass c :: ")" :: tail)
  | App (h, f, a) ->
      "App(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          ("," ::
             catelim_debugstring_of_term f
               ("," :: catelim_debugstring_of_term a (")" :: tail)))
  | Tup (h, s, ts) ->
      "Tup(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          ("," :: enQuote s :: "," ::
             dolist catelim_debugstring_of_term ts (")" :: tail))
  | Literal (h, Number k) ->
      "Literal(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          (",Number " :: string_of_int k :: ")" :: tail)
  | Literal (h, String k) ->
      "Literal(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          (",String \"" :: k :: "\")" :: tail)
  | Fixapp (h, ss, ts) ->
      "Fixapp(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          ("," ::
             dolist (catelim_of_stringfn enQuote) ss
               ("," :: dolist catelim_debugstring_of_term ts (")" :: tail)))
  | Subst (h, r, p_, m) ->
      "Subst(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          ("," :: string_of_bool r :: "," ::
             catelim_debugstring_of_term p_
               ("," :: catelim_debugstring_of_substmap m (")" :: tail)))
  | Binding (h, (bs, ss, us), _, pat) ->
      "Binding(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          (",(" ::
             dolist catelim_debugstring_of_term bs
               ("," ::
                  dolist catelim_debugstring_of_term ss
                    ("," ::
                       dolist catelim_debugstring_of_term us
                         (")," :: "...," ::
                            catelim_debugstring_of_term pat (")" :: tail)))))
  | Collection (h, k, es) ->
      "Collection(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          ("," :: string_of_idclass k :: "," ::
             dolist (catelim_debugstring_of_element catelim_debugstring_of_term) es
               (")" :: tail))

and catelim_debugstring_of_substmap vts =
  dolist
    (fun (v, t) tail ->
       "(" ::
         catelim_debugstring_of_term v
           ("," :: catelim_debugstring_of_term t (")" :: tail)))
    vts

and catelim_debugstring_of_element f e tail =
  match e with
    Segvar (h, ps, v) ->
      "Segvar(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          ("," :: dolist f ps (f v (")" :: tail)))
  | Element (h, r, t) ->
      "Element(" ::
        catelim_string_of_option (catelim_of_stringfn string_of_int) h
          ("," :: catelim_debugstring_of_resnum r ("," :: f t (")" :: tail)))

and catelim_debugstring_of_resnum r = catelim_string_of_resnum r

let debugstring_of_term = stringfn_of_catelim catelim_debugstring_of_term

let rec debugstring_of_element f =
  stringfn_of_catelim (catelim_debugstring_of_element (catelim_of_stringfn f))

(******** rebuilding bindings ******************)

let rec remake mapterm (_, (bs, ss, us), env, pat as b) =
  try
    let emap = mkmap env in
    let rec f t =
      match (emap <@> t) with
        Some (1, i) -> Some (Listfuns.guardednth bs i)
      | Some (2, i) -> Some (Listfuns.guardednth ss i)
      | Some (3, i) -> Some (Listfuns.guardednth us i)
      | Some (k, i) ->
          raise
            (Catastrophe_
               ["Some("; string_of_int k; ","; string_of_int i; ") in remake ";
                debugstring_of_term (Binding b)])
      | None -> None
    in
    mapterm f pat
  with
    Listfuns.Bad_nth ->
      raise (Catastrophe_ ["Bad_nth in remake "; 
                                debugstring_of_term (Binding b)])

(* ------------------------------------------------------------------------------------- *)
(* Bernard's pretty-printer in all its glory *)
     
let debracketapplications = ref false

exception Matchintermstring_     (* spurious *)

    (* local versions of mapterm, mapelements, which don't use the termstore *)
    (* This is mildly more efficient, but really is because I want to use string_of_term
     * to monitor the termstore, and string_of_term needs to remake bindings, and remake needs
     * mapterm, and mapterm needs mapelements.
     *)
let rec mapterm f t =
  match f t with
  | Some t' -> t'
  | None ->
      let mtf = mapterm f in
      match t with
      | App         (_, f, a) -> App (None, mtf f, mtf a)
      | Tup        (_, s, ts) -> Tup (None, s, (mtf <* ts))
      | Fixapp    (_, ss, ts) -> Fixapp (None, ss, (mtf <* ts))
      | Binding (_, (bs, ss, us), env, pat) ->
          Binding
            (None, ((mtf <* bs), (mtf <* ss), (mtf <* us)), env,
             pat)
      | Subst (_, r, p_, vts) ->
          Subst (None, r, mtf p_, (fun (v, t) -> mtf v, mtf t) <* vts)
      | Collection (_, k, es) -> Collection (None, k, mapelements f es)
      | _                     -> t

and mapelements f es =
  let rec g =
    function
    | Segvar (_, ps, v) -> Segvar (None, ps, mapterm f v)
    | Element (_, r, t) -> (* not really satisfactory *)
                           Element (None, r, mapterm f t)
  in
  (* yes, it should really be r *)
  List.map g es

let rec stripelement =
  function
  | Element (_, _, t)      -> t
  | Segvar (_, [], v)      -> v
  | Segvar (_, p :: ps, v) ->
      App (None, p, stripelement (Segvar (None, ps, v)))

(* *********************** prettyprinter proper starts here ************************* *)

let rec firstatom =
  function
  | []       -> ""
  | "" :: ss -> firstatom ss
  | s  :: ss -> if isInvisibleString s then firstatom ss else s

(* OCaml gave ::: and :::: priority lower than :: (SML let us give them priorities) 
   and left-associative (not like ::). So they have to be named functions. Sorry.
 *)

(* tricolon strips out blank strings, then puts a space _before and after_ things that 
 * must be separated. It's used for infix operators.
 *)
(* "" ::: atoms = atoms -- never happens *)

let rec tricolon a b =
  match a,b with
    atom, [] -> [atom]
  | atom, "" :: rest -> tricolon atom rest
  | atom, rest ->
      if mustseparate (atom, firstatom rest) then
        " " :: atom :: " " :: rest
      else atom :: rest

let (<:::>) = tricolon (* use with care for readability *)

(* quadcolon strips out blank strings, removes double spaces, puts spaces in between
 * atoms that must be separated.
 * Because _T is now properly cat-eliminated, we can't fold quadcolon into the result as we used to.
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

let (<::::>) = quadcolon (* use with care for readability *)

let opname f =
  match f, !debracketapplications, debracket f with
    Id (_, v, _), _   , _            -> Some (string_of_vid v)
  | _           , true, Id (_, v, _) -> Some (string_of_vid v)
  | _                                -> None

(* test if a formula will be printed as a juxtaposition *)
let isJuxtapos t =
  let opsymb = opname &~ (_Some <.> lookup) in
  match t with
  | App (_, (App (_, f', _) as f), a) ->
      (match opsymb f' with
         Some (INFIXC  _) -> None
       | _                -> Some(f,a))
  | App (_, f, a) ->
      (match opsymb f with
         Some (INFIX   _) -> 
           (match a, !debracketapplications, debracket a with 
              Tup (_, ",", [_; _]), _   , _                    -> None
            | _                   , true, Tup (_, ",", [_; _]) -> None
            | _                                                -> Some(f,a))
       | Some (PREFIX  _) -> None
       | Some (POSTFIX _) -> None
       | _                -> Some(f,a))
  | _ -> None

(* test if a formula will be printed as infix *)
let isInfixApp t =
  let opsymb = opname &~ (fun name -> Some(name,lookup name)) in
  match t with
  | App (_, (App (_, f, a1)), a2) ->
      (match opsymb f with
         Some (name, (INFIXC _ as sy)) -> Some (name, prio sy, assoc sy, a1, a2)
       | _                             -> None)
  | App (_, f, arg) ->
      (match opsymb f with
         Some (name, (INFIX _ as sy)) -> 
           (match arg, !debracketapplications, debracket arg with 
              Tup (_, ",", [a1; a2]), _, _ ->
                Some(name, prio sy, assoc sy, a1, a2)
            | _, true, Tup (_, ",", [a1; a2]) ->
                Some(name, prio sy, assoc sy, a1, a2)
            | _, _, _ -> None)
       | _ -> None)
  | _ -> None
  
(* this function will probably produce stupid results if the language includes
 * operators with identical priorities but differing associativity.
 * (But in that case we have ambiguity anyway, so who cares? RB 14/xi/2008)
 *)
(* I tried to write this without disfiguring it with parameters ivb & ivk, then I realised
 * that the compiler would put them in anyway.  So it's ugly, but not obviously more 
 * inefficient.
 * RB 25/6/2001
 *)
(* Given low-priority LEFTFIX and/or RIGHTFIX, we put in too many brackets. This is an attempt
   to fix that. RB 01/06/2020
 *)
(* ivb and ivk put in invisible brackets;
   nl and nr are the priority of symbols to left and right (needed for LEFTFIX, RIGHTFIX);
   a isn't associativity, it's mustbracket if equal priority;
   t is the term;
   s is the strings that follow (should be ss, n'est ce pas?)
 *)
let rec _T ivb ivk nl nr a t s = 
  let mbn a m n = n > m || n = m && a in
  let mb a m =  mbn a m nl || mbn a m nr in (* the normal case *)
  let mustbracket b a m = if mb a m then b else "" in
  let _OB = mustbracket "(" a in
  let _CB = mustbracket ")" a in
  let _Bns m = if mb a m then 0, 0 else nl, nr in
  let _OBprefix m = if mbn a m nr then "(" else "" in
  let _CBprefix m = if mbn a m nr then ")" else "" in
  let _Bprefixns m = if mbn a m nr then 0, 0 else nl, nr in
  let _OBpostfix m = if mbn a m nl then "(" else "" in
  let _CBpostfix m = if mbn a m nl then ")" else "" in
  let _Bpostfixns m = if mbn a m nl then 0, 0 else nl, nr in
  let rec _TAP f arg s = (* must be function application, surely *)
     let nl, nr = _Bns !appfix in
    _OB !appfix <::::> _T ivb ivk nl !appfix false f (_T ivb ivk !appfix nr true arg (_CB !appfix <::::> s))
  in
  let rec tip_ m assoc arg1 f arg2 s = (* I think this is doing infix *)
    let nl, nr = _Bns m in
    let afterf = _T ivb ivk m nr (assoc <> RightAssoc) arg2 (_CB m <::::> s) in
    _OB m <::::> _T ivb ivk nl m (assoc <> LeftAssoc) arg1 (f <:::> afterf)
  in
  let rec _TFA sy f t s =
    match sy with
    | PREFIX  name  ->
        (* we need to bracket PREFIX only according to nr *)
        let m = prio sy in
        let nl, nr = _Bprefixns m in
        _OBprefix m <::::> (name <::::> _T ivb ivk m nr false t (_CBprefix m <::::> s))
    | POSTFIX name ->
        (* we need to bracket POSTFIX only according to nl *)
        let m = prio sy in
        let nl, nr = _Bpostfixns m in
        _OBpostfix m <::::> _T ivb ivk nl m false t (name <::::> (_CBpostfix m <::::> s))
    | _            -> _TAP f t s
  in
  let rec _TT nl nsep nr b sep ts s = (* doing tuples, surely *)
    match ts with
    | []      -> s
    | [t]     -> _T ivb ivk nl nr b t s
    | t :: ts -> _T ivb ivk nl nsep b t (sep  <::::> _TT nsep nsep nr b sep ts s)
  in
  match t with
  | Id (_, v, _) ->
      let sv = string_of_vid v in
      (match lookup sv with
       | INFIX   _ 
       | INFIXC  _ 
       | PREFIX  _ 
       | POSTFIX _ -> ivb t :: ("(" <::::> (sv <::::> (")" <::::> (ivk t :: s))))
       | _         -> ivb t :: (sv <::::> (ivk t :: s))
      )
  | Unknown (_, v, _) -> ivb t :: (metachar_as_string ^ (string_of_vid v) <::::> (ivk t :: s))
  | App (_, (App (_, f, arg1) as l), arg2) ->
      (match opname f &~~ (fun name -> match lookup name with
                                       | INFIXC _ as sy' ->
                                           Some (ivb t :: tip_ (prio sy') (assoc sy') arg1 name arg2 (ivk t :: s))
                                       | _               -> None
                          )
       with
       | Some r -> r
       | _      -> ivb t :: _TAP l arg2 (ivk t :: s)
      )
  | App (_, f, arg) ->
      (match opname f &~~ (fun name -> match lookup name with
                                       | INFIX _ as sy' ->
                                           let m = prio sy' in
                                           let a = assoc sy' in 
                                           (match arg, !debracketapplications, debracket arg with
                                            | Tup (_, ",", [arg1; arg2]), _, _ ->
                                                Some (ivb t :: tip_ m a arg1 name arg2 (ivk t :: s))
                                            | _, true, Tup (_, ",", [arg1; arg2]) ->
                                                Some (ivb t :: tip_ m a arg1 name arg2 (ivk t :: s))
                                            | _ -> None
                                           )
                                       | sy -> Some (ivb t :: _TFA sy f arg (ivk t :: s))
                          )
       with
       | Some r -> r
       | None   -> ivb t :: _TAP f arg (ivk t :: s)
      )
  | Tup (_, sep, ts) ->
      let m = match lookup sep with
              | INFIX _ as sy' -> prio sy'
              | _              -> 0
      in
      let nl, nr = _Bns m in 
      ivb t :: (_OB m <::::> _TT nl m nr true sep ts (quadcolon (_CB m) (ivk t :: s)))
  | Literal (_, Number k) ->(* take in the brackets as well ... *)
     ivb t :: (string_of_int k <::::> (ivk t :: s))
  | Literal (_, String k) ->
      ivb t :: ("\"" ^ k ^ "\"" <::::> (ivk t :: s))
  | Fixapp (_, ss, ts) ->
      (match lookup (List.hd ss) with
       | BRA _ ->
           ivb t :: (List.hd ss <::::> _TS1 ivb ivk 0 0 0 (List.tl ss) false ts (ivk t :: s))
       | LEFTFIX _ as sy' ->
           let m = prio sy' in
           (* we need to bracket LEFTFIX only according to nr *)
           let nl, nr = _Bprefixns m in
           ivb t ::
             (_OBprefix m <::::>
               (List.hd ss <::::> _TS2 ivb ivk nl m nr (List.tl ss) true ts (_CBprefix m <::::> (ivk t :: s))))
       | MIDFIX _ as sy' ->
           let m = prio sy' in
           let nl, nr = _Bns m in
           ivb t :: (_OB m <::::> _TS2 ivb ivk nl m nr ss true ts (_CB m <::::> (ivk t :: s)))
       | RIGHTFIX _ as sy' ->
           let m = prio sy' in
           (* we need to bracket RIGHTFIX only according to nl *)
           let nl, nr = _Bpostfixns m in
           ivb t :: (_OB m <::::> _TS1 ivb ivk nl m 0 ss true ts (_CB m <::::> (ivk t :: s)))
       | sy'               -> raise (Catastrophe_ ["Matchintermstring_ "; debugstring_of_symbol sy'])
      )
  | Subst (_, _, t, m) ->
      ivb t ::
        _T ivb ivk nl !substfix false t
           (string_of_symbol SUBSTBRA <::::> _TM ivb ivk m (string_of_symbol SUBSTKET <::::> (ivk t :: s)))
  | Binding stuff -> _T ivb ivk nl nr a (remake mapterm stuff) s
  | Collection (_, c, es) ->
      let nl, nr = if mb a 0 then 0, 0 else nl, nr in 
      ivb t :: 
        (_OB 0 <::::> 
           (unparseidclass c <::::> (" " <::::> _TT nl 0 nr true "," (List.map stripelement es) (_CB 0 <::::> (ivk t :: s)))))

and _TS1 ivb ivk nl m nr seps b ts s = (* this does a list of terms with separators, priority m, nl to the left, nr to the right *)
  match seps, ts with
  | [ket]      , []  when nr<>0    
                         -> (Printf.sprintf "???_TS1 %d???" nr) <::::> (ket <:::> s)
  | [ket]      , []      -> ket <:::> s
  | []         , []      -> s 
  | sep :: seps, t :: ts -> _T ivb ivk nl m b t (sep <::::> _TS1 ivb ivk m m nr seps b ts s)
  | _                    -> "???_TS1???" <::::> s

and _TS2 ivb ivk nl m nr seps b ts s =
  match seps, ts with
    []         , [t]     -> _T ivb ivk nl nr b t s
  | sep :: seps, t :: ts ->  _T ivb ivk nl m b t (sep <::::> _TS2 ivb ivk m m nr seps b ts s)
  | _                    -> quadcolon "???_TS2???" s

and _TS ivb ivk ts r = (* this thing does the stuff inside SUBSTBRA, SUBSTKET *)
  match ts with
    []      -> r
  | [t]     -> _T ivb ivk 0 0 true t r
  | t :: ts -> _T ivb ivk 0 0 true t (","  <::::> _TS ivb ivk ts r)

and _TM ivb ivk vts s =
  let rec var (v, t) = v in
  let rec expr (v, t) = t in
  let (fst, snd) = if !substsense then var, expr else expr, var in
  _TS ivb ivk (List.map fst vts) (string_of_symbol SUBSTSEP <::::> _TS ivb ivk (List.map snd vts) s)

let nobra   _ = ""
let noket   _ = ""
let showbra _ = invisbra
let showket _ = invisket

let catelim_invisbracketedstring_of_prioterm b n = 
  if b then _T showbra showket n n else _T nobra noket n n
 
let catelim_invisbracketedstring_of_term b = 
  catelim_invisbracketedstring_of_prioterm b 0 false
let invisbracketedstring_of_term =
  stringfn_of_catelim <.> catelim_invisbracketedstring_of_term

let catelim_string_of_term = catelim_invisbracketedstring_of_term false
let rec string_of_term = stringfn_of_catelim catelim_string_of_term
let rec diag_string_of_term t = (if !termstringdebug then debugstring_of_term 
                                                     else string_of_term) t

let rec catelim_chooseinvisbracketedstring_of_term ivb ivk = _T ivb ivk 0 0 false
let rec chooseinvisbracketedstring_of_term ivb ivk = stringfn_of_catelim (catelim_chooseinvisbracketedstring_of_term ivb ivk)

let rec catelim_string_of_vts vts ss =
  string_of_symbol SUBSTBRA <::::> _TM nobra nobra vts (string_of_symbol SUBSTKET <::::> ss)
let string_of_vts = stringfn_of_catelim catelim_string_of_vts

let catelim_invisbracketedstring_of_termfun b =
  catelim_invisbracketedstring_of_prioterm b !appfix false (* see _TAP *)
  
let catelim_invisbracketedstring_of_termarg b =
  catelim_invisbracketedstring_of_prioterm b !appfix true
let invisbracketedstring_of_termarg =
  stringfn_of_catelim <.> catelim_invisbracketedstring_of_termarg
  
let rec catelim_string_of_termarg = catelim_invisbracketedstring_of_termarg false
let string_of_termarg = stringfn_of_catelim catelim_string_of_termarg

let catelim_invisbracketedstring_of_element b =
  catelim_invisbracketedstring_of_term b <.> stripelement
let invisbracketedstring_of_element =
  stringfn_of_catelim <.> catelim_invisbracketedstring_of_element

let catelim_string_of_element = catelim_invisbracketedstring_of_element false
let string_of_element = stringfn_of_catelim catelim_string_of_element

let rec catelim_chooseinvisbracketedstring_of_element ivb ivk =
  catelim_chooseinvisbracketedstring_of_term ivb ivk <.> stripelement
let rec chooseinvisbracketedstring_of_element ivb ivk =
  stringfn_of_catelim (catelim_chooseinvisbracketedstring_of_element ivb ivk)

let catelim_invisbracketedstring_of_collection b sep t =
  match t with
  | Collection (_, _, es) -> 
      catelim_string_of_list (catelim_invisbracketedstring_of_element b) sep es
  | _ -> raise (Catastrophe_ ("string_of_collection " :: catelim_string_of_term t []))
let invisbracketedstring_of_collection b sep = 
  stringfn_of_catelim (catelim_invisbracketedstring_of_collection b sep)

let catelim_string_of_collection = catelim_invisbracketedstring_of_collection false
let string_of_collection sep = stringfn_of_catelim (catelim_string_of_collection sep)

let rec catelim_invisbracketedstring_of_termOrCollection b sep t =
  match t with
  | Collection _ -> catelim_invisbracketedstring_of_collection b sep t
  | _ -> catelim_invisbracketedstring_of_term b t
let rec invisbracketedstring_of_termOrCollection b sep =
  stringfn_of_catelim (catelim_invisbracketedstring_of_collection b sep)

let catelim_string_of_termOrCollection = catelim_invisbracketedstring_of_termOrCollection false
let string_of_termOrCollection sep = stringfn_of_catelim (catelim_string_of_collection sep)

let string_of_termlist = bracketed_string_of_list string_of_term ";"
let catelim_string_of_termlist = catelim_bracketedstring_of_list catelim_string_of_term ";"
