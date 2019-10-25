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

open Sexpr
open UTF

type sexpr = Sexpr of sexpr list | Atom of string

(* convert a term to an Sexpr *)

let rec _S t =
   let _SIN left operator right ss = Sexpr [Atom "Infix"; _S left; operator; _S right] in
   let _SAP f a ss = Sexpr [Atom "App"; _S f; _S a] in
   match t with
     Id(_,v,_) ->
       let sv = string_of_vid v in
       let id_as_op () = Sexpr [Atom "Op"; Atom sv] in
       ( match lookup sv with
            INFIX   _ -> id_as_op ()
          | INFIXC  _ -> id_as_op ()
          | PREFIX  _ -> id_as_op ()
          | POSTFIX _ -> id_as_op ()
          |         _ -> Sexpr [Atom "Id "; Atom sv] )
  | Unknown (_, v, _) -> Sexpr [Atom "Unknown "; Atom (string_of_vid v)]
  | App (_, (App (_, f, arg1) as left), arg2) ->
      ( match
          opname f &~~
          (fun name ->
             match lookup name with
               INFIXC _ ->
                 Some (_SIN arg1 name arg2 ss)
                 | _           -> None)
        with
          Some ss -> ss
        | _       -> _SAP left arg2 ss )
  | App (_, f, arg) ->
      ( match
          opname f &~~
          (fun name ->
             match lookup name with
               INFIX _ ->
                 ( match arg, !debracketapplications, debracket arg with
                     Tup (_, ",", [arg1; arg2]), _, _ -> 
                       Some (_SIN arg1 name arg2 ss)
                   | _, true, Tup (_, ",", [arg1; arg2]) ->
                       Some (_SIN arg1 name arg2 ss)
                   | _ -> None )
             | PREFIX _ ->
                 Some (Sexpr [Atom "Prefix"; _S arg])
             | POSTFIX _ ->
                 Some (Sexpr [Atom "Postfix"; _S arg])
             | _ -> None)
        with
          Some ss -> ss
        | None    -> _SAP f arg ss )
  | Tup (_, sep, ts) ->
      Sexpr (Atom "Tuple" :: Atom sep :: List.map _S ts)
  | Literal (_, Number k) ->
      Sexpr [Atom "Number"; Atom (string_of_int k)]
  | Literal (_, String k) ->
      Sexpr [Atom "String"; Atom k]
  | Fixapp (_, seps, ts) ->
      let rec _SSt seps ts =
        match seps, ts with
          [], []    -> []
        | _ , []    -> _SSs seps ts
        | _ , t::ts -> _S t :: _SSs seps ts
      and _SSs seps ts =
        match seps, ts with
          []       , []  -> []
        | []       , _   -> _SSt seps ts
        | sep::seps, _ -> Atom sep :: _SSt seps ts
			in
      ( match lookup (List.hd ss) with
          BRA _ ->
            Sexpr ("Outfix" :: _SSs seps ts) 
        | LEFTFIX _  ->
            Sexpr ("Leftfix" :: _SSs seps ts) 
        | MIDFIX _ ->
            Sexpr ("Midfix" :: _SSt seps ts) 
        | RIGHTFIX _ ->
            Sexpr ("Outfix" :: _SSt seps ts) 
        | sy' -> raise (Catastrophe_ ["Matchintermstring_ "; debugstring_of_symbol sy']) )
  | Subst (_, _, t, ms) ->
      let _SM (v,t) vts = _S v :: _S t :: vts in
      Sexpr (Atom "Substitution"; _S t; List.fold_right _SM ms [])
  | Binding stuff -> _S (remake mapterm stuff)
  | Collection (_, c, es) ->
      Sexpr (Atom "Collection" :: Atom (string_of_idclass c) :: List.fold_right (_S <.> stripelement) es [])
			
(* An S-expr printer, for those, such as oracles, who want to know the structure of a formula 
 * but don't care about its original form nor the Japeish internal form *)
 
 let rec _S t ss =
   let _SIN left operator right ss = "(Infix " :: _S left (" " :: enQuote operator :: " " :: _S right (")" :: ss)) in
   let _SAP f a ss = "(App " :: _S f (" " :: _S a (")" :: ss)) in
   match t with
     Id(_,v,_) ->
       let sv = string_of_vid v in
       let id_as_op () = "(Op " :: enQuote sv :: ")" :: ss in
       ( match lookup sv with
            INFIX   _ -> id_as_op ()
          | INFIXC  _ -> id_as_op ()
          | PREFIX  _ -> id_as_op ()
          | POSTFIX _ -> id_as_op ()
          |         _ -> "(Id " :: enQuote sv :: ")" :: ss )
  | Unknown (_, v, _) -> "(Unknown " :: enQuote (string_of_vid v) :: ")" :: ss
  | App (_, (App (_, f, arg1) as left), arg2) ->
      ( match
          opname f &~~
          (fun name ->
             match lookup name with
               INFIXC _ ->
                 Some (_SIN arg1 name arg2 ss)
                 | _           -> None)
        with
          Some ss -> ss
        | _       -> _SAP left arg2 ss )
  | App (_, f, arg) ->
      ( match
          opname f &~~
          (fun name ->
             match lookup name with
               INFIX _ ->
                 ( match arg, !debracketapplications, debracket arg with
                     Tup (_, ",", [arg1; arg2]), _, _ -> 
                       Some (_SIN arg1 name arg2 ss)
                   | _, true, Tup (_, ",", [arg1; arg2]) ->
                       Some (_SIN arg1 name arg2 ss)
                   | _ -> None )
             | PREFIX _ ->
                 Some ("(Prefix " :: enQuote name :: _S arg (")" :: ss))
             | POSTFIX _ ->
                 Some ("(Postfix " :: _S arg (" " :: enQuote name :: ")" :: ss))
             | _ -> None)
        with
          Some ss -> ss
        | None    -> _SAP f arg ss )
  | Tup (_, sep, ts) ->
      "(Tuple " :: enQuote sep :: List.fold_right _S ts (")" :: ss)
  | Literal (_, Number k) ->
      "(Number " :: enQuote (string_of_int k) :: ")" :: ss
  | Literal (_, String k) ->
      "(String " :: enQuote k :: ")" :: ss
  | Fixapp (_, seps, ts) ->
      let rec _SS seps ts ss =
        match seps, ts with
          []       , []    -> ss
        | [sep]    , []    -> enQuote sep :: " " :: ss
        | []       , [t]   -> _S t ss
        | sep::seps, t::ts -> enQuote sep :: " " :: _S t (_SS seps ts ss)
				| _        , _     -> raise (Catastrophe_ ["_SS "; string_of_int (List.length seps); " "; string_of_int (List.length ts)])
			in
      ( match lookup (List.hd ss) with
          BRA _ ->
            "(Outfix " :: _SS seps ts (")" :: ss) 
        | LEFTFIX _  ->
            "(Leftfix " :: _SS seps ts (")" :: ss) 
        | MIDFIX _ ->
            "(Midfix " :: _S (List.hd ts) (" " :: _SS seps (List.tl ts) (")" :: ss)) 
        | RIGHTFIX _ ->
            "(Rightfix " :: _S (List.hd ts) (" " :: _SS seps (List.tl ts) (")" :: ss)) 
        | sy' -> raise (Catastrophe_ ["Matchintermstring_ "; debugstring_of_symbol sy']) )
  | Subst (_, _, t, ms) ->
      let _SM (v,t) ss = " (" :: _S v (" " :: _S t (")" :: ss)) in
      "(Substitution " :: _S t (List.fold_right _SM ms (")" :: ss))
  | Binding stuff -> _S (remake mapterm stuff) ss
  | Collection (_, c, es) ->
      "(Collection " :: List.fold_right (_S <.> stripelement) es (")" :: ss)

let catelim_sexprstring_of_term = _S
let sexprstring_of_term = stringfn_of_catelim _S

let term_of_sexprstring s = 
	
	
