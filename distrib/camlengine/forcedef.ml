(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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

open Listfuns
open Miscellaneous
open Optionfuns 
open Sml
open Sequent
open Stringfuns
open Symbol
open Symboltype
open Termfuns
open Termstring
open Termparse

type term = Termtype.term
    
type forcedef = ForceAlways
              | ForceNever
              | ForcePrim of term
              | ForceBoth of (forcedef * forcedef)
              | ForceEither of (forcedef * forcedef)
              | ForceIf of (forcedef * forcedef)
              | ForceEverywhere of forcedef
              | ForceNowhere of forcedef
              | ForceAll  of (term * term list * forcedef)
              | ForceSome of (term * term list * forcedef)
                            (* pat   vars        body: a binder *)

let term_of_forcedef fd =
  match fd with
  | ForcePrim t -> Some t
  | _           -> None
  
let rec catelim_string_of_forcedef f ss =
  match f with
    ForceAlways          -> "ALWAYS" :: ss
  | ForceNever           -> "NEVER" :: ss
  | ForcePrim t          -> "FORCE " :: catelim_string_of_termarg t ss
  | ForceBoth (f1, f2)   -> "BOTH (" :: catelim_string_of_forcedef f1 (") (" :: catelim_string_of_forcedef f2 (")"::ss))
  | ForceEither (f1, f2) -> "EITHER (" :: catelim_string_of_forcedef f1 (") (" :: catelim_string_of_forcedef f2 (")"::ss))
  | ForceIf (f1, f2)     -> "IF (" :: catelim_string_of_forcedef f1 (") (" :: catelim_string_of_forcedef f2 (")"::ss))
  | ForceEverywhere f    -> "EVERYWHERE (" :: catelim_string_of_forcedef f (")"::ss)
  | ForceNowhere f       -> "NOWHERE (" :: catelim_string_of_forcedef f (")"::ss)
  | ForceAll (t, vs, f)  -> "ALL (" :: catelim_string_of_term t (") (" :: catelim_string_of_forcedef f (")"::ss))
  | ForceSome (t, vs, f) -> "SOME (" :: catelim_string_of_term t (") (" :: catelim_string_of_forcedef f (")"::ss))

let rec string_of_forcedef f = implode (catelim_string_of_forcedef f [])

let rec option_mapforcedef f fd =
  let omff = option_mapforcedef f in
  let ompair = option_rewrite2 omff omff in
  let omtvsfd = option_rewrite3 (fun v -> None) (fun v -> None) omff in
  match f fd with
  | Some _ as result -> result
  | None             ->
      match fd with
      | ForceAlways         
      | ForceNever          
      | ForcePrim         _ -> None
      | ForceBoth      pair -> ompair pair &~~ (_Some <.> (fun v->ForceBoth v))
      | ForceEither    pair -> ompair pair &~~ (_Some <.> (fun v->ForceEither v))
      | ForceIf        pair -> ompair pair &~~ (_Some <.> (fun v->ForceIf v))
      | ForceEverywhere  fd -> omff fd &~~ (_Some <.> (fun v->ForceEverywhere v))
      | ForceNowhere     fd -> omff fd &~~ (_Some <.> (fun v->ForceNowhere v))
      | ForceAll      tvsfd -> omtvsfd tvsfd &~~ (_Some <.> (fun v->ForceAll v))
      | ForceSome     tvsfd -> omtvsfd tvsfd &~~ (_Some <.> (fun v->ForceSome v))

let mapforcedef f = anyway (option_mapforcedef f)

let rec option_mapforcedefterms f fd =
  let omt = option_mapterm f in
  let omff = option_mapforcedefterms f in
  let omtvsfd = option_rewrite3 omt (option_rewritelist omt) omff in
  let fdf fd = 
    match fd with
    | ForcePrim         t -> omt t &~~ (_Some <.> (fun v->ForcePrim v))
    | ForceAll      tvsfd -> omtvsfd tvsfd &~~ (_Some <.> (fun v->ForceAll v))
    | ForceSome     tvsfd -> omtvsfd tvsfd &~~ (_Some <.> (fun v->ForceSome v))
    | _                   -> None
  in
  option_mapforcedef fdf fd

let rec mapforcedefterms f = anyway (option_mapforcedefterms f)

let rec findinforcedef f fd =
  let rec findinpair (fd1, fd2) =
    (findinforcedef f fd1 |~~ (fun _ -> findinforcedef f fd2))
  in
  match fd with
    ForceAlways         -> None
  | ForceNever          -> None
  | ForcePrim         t -> findterm f t
  | ForceBoth      pair -> findinpair pair
  | ForceEither    pair -> findinpair pair
  | ForceIf        pair -> findinpair pair
  | ForceEverywhere  fd -> findinforcedef f fd
  | ForceNowhere     fd -> findinforcedef f fd
  | ForceAll (t, _, fd) ->
      (findterm f t |~~ (fun _ -> findinforcedef f fd))
  | ForceSome (t, _, fd) ->
      (findterm f t |~~ (fun _ -> findinforcedef f fd))

let rec existsinforcedef f =
  bool_of_opt <.> findinforcedef (fun t -> if f t then Some true else None)

(* this ran into trouble when it emerged that it was using ALL as a reserved word, which had
   been previously used in LAYOUT tactics to mean 'display all subtrees'.  On the principle
   that reserved words ought not to be easily confused, this was a problem.  Also, it conflicted
   with the principle that we ought to be able to load old proofs.  
   
   First I tried to fix it by making LAYOUT use another word (ALLL instead of ALL) but ugh!.
   
   Second I tried to fix it by parsing it as a term and then analysing.  This worked up to a 
   point, but it fell apart when it saw FORCE P(i), which parses as (but obviously doesn't mean)
   (FORCE P) i.  
   
   But I stuck with it, demanding only that you bracket the argument of FORCE. 
 *)
(* I should have liked BOTH and EITHER to work as infix, but it makes life too 
   complicated ... so Lisp-style FORCEDEFs rule.  Anyway, it avoids stupidities
   about priorities!
 *)
let rec parseForceDef () =
  (* there is no operator priority in forcedefs ... *)
  let decodeApp t =
    match explodeApp false t with
      (f, args) -> (string_of_term f, args)
  in
  let rec tranForceDef t =
    match decodeApp t with
      ("ALWAYS"      , [ ]) -> ForceAlways
    | ("NEVER"       , [ ]) -> ForceNever
    | ("FORCE"       , [t]) -> ForcePrim (tranPrim t)
    | ("EVERYWHERE"  , [f]) -> ForceEverywhere (tranForceDef f)
    | ("NOWHERE"     , [f]) -> ForceNowhere (tranForceDef f)
    | ("ALL"    , [pat; f]) -> ForceAll (tranForceDefBinder pat f)
    | ("SOME"   , [pat; f]) -> ForceSome (tranForceDefBinder pat f)
    | ("BOTH"   , [f1; f2]) -> ForceBoth(tranForceDef f1, tranForceDef f2)
    | ("EITHER" , [f1; f2]) -> ForceEither(tranForceDef f1, tranForceDef f2)
    | ("IF"     , [f1; f2]) -> ForceIf(tranForceDef f1, tranForceDef f2)
    | _ -> raise (ParseError_ 
           ["FORCE t, EVERYWHERE f, NOWHERE f, ALL pat f, SOME pat f, BOTH f f, EITHER f f "; 
            "or IF f f expected in FORCEDEF; found "; string_of_term t
           ])
  
  and tranPrim t =
    try checkTacticTerm t; debracket t 
    with Tacastrophe_ ss -> 
           raise (ParseError_ ("FORCE " :: string_of_term t :: " contains " :: ss))
  
  and tranForceDefBinder pat f = 
    let vs = isVariable <| termvars pat in
    if List.exists (not <.> isextensibleId) vs then
      raise (ParseError_ ["ALL and SOME must use CLASS VARIABLE identifiers to describe individuals"])
    else (pat,vs,tranForceDef f)
  in
      tranForceDef (asTactic parseTerm EOF)

(* now also includes the disproof universe bit of shared proofs *)

type coordinate = Coord of (int * int)

type world = World of (coordinate * coordinate list * term list)

type model = Model of world list

let rec parsemodel () =
  let rec parseCoord () =
    match currsymb () with
      BRA "(" ->
        scansymb ();
        let rec parseInt () =
          match currsymb () with
            NUM n -> (scansymb ();  atoi n)
          | sy ->
              match string_of_symbol sy with
                "-" -> (scansymb ();  - parseUnsignedInt "-")
              | "~" -> (scansymb ();  - parseUnsignedInt "~")
              | s -> bang [s]
        and bang ss =
          raise
            (ParseError_ ("number expected in coordinate; found " :: ss))
        and parseUnsignedInt s1 =
          match currsymb () with
            NUM n -> (scansymb (); atoi n)
          | s2 -> bang [s1; " followed by "; string_of_symbol s2]
        in
        let x = parseInt () in
        let y =
          if currsymb () = commasymbol then
            (scansymb ();  parseInt ())
          else
            raise
              (ParseError_
                 ["comma expected after x value in world coordinate"])
        in
        begin match currsymb () with
          KET ")" -> (scansymb ();  Coord (x, y))
        | sy ->
            raise
              (ParseError_
                 ["right paren expected after coordinate; found ";
                  string_of_symbol sy])
        end
    | sy ->
        raise
          (ParseError_
             ["coordinate expected, starting with left paren; found ";
              string_of_symbol sy])
  in
  let rec parseWorlds () =
    match currsymb () with
      SHYID "WORLD" ->
        scansymb (); 
        let c = parseCoord () in
        let chs =
          match currsymb () with
            SHYID "CHILDREN" ->
              (scansymb (); 
               parseList
                 (function BRA "(" -> true | _ -> false)
                 (fun _ -> parseCoord ()) commasymbol)
          | _ -> []
        in
        let ts =
          match currsymb () with
            SHYID "LABELS" ->
              (scansymb ();  parseList canstartTerm parseTerm commasymbol)
          | _ -> []
        in
        World (c, chs, ts) :: parseWorlds ()
    | _ -> []
  in
  match currsymb () with
    SHYID "SEMANTICS" ->
      (scansymb (); 
       let seq = parseSeq () in
       (match parseWorlds () with
         [] -> raise (ParseError_ ["empty disproof description"])
       | worlds -> Some (seq, Model worlds)))
  | _ -> None

let rec catelim_string_of_model a1 a2 =
  match a1, a2 with
    None, ss -> ss
  | Some (seq, Model worlds), ss ->
      let sep = "\n" in
      let rec catelim_string_of_int i ss =
        (string_of_int : int -> string) i :: ss
      in
      let rec catelim_string_of_coord =
        fun (Coord c) ->
          catelim_string_of_pair catelim_string_of_int catelim_string_of_int "," c
      in
      let rec catelim_string_of_world =
        fun (World (c, chs, ts)) ss ->
          let sep2 = sep ^ "  " in
          let rec catelim_string_of_children chs ss =
            match chs with
              [] -> ss
            | _ ->
                sep2 :: "CHILDREN" :: " " ::
                  catelim_string_of_list catelim_string_of_coord ", " chs ss
          in
          let rec catelim_string_of_labels ts ss =
            match ts with
              [] -> ss
            | _ ->
                sep2 :: "LABELS" :: " " ::
                  catelim_string_of_list catelim_string_of_term ", " ts ss
          in
          "WORLD" :: " " ::
            catelim_string_of_coord c
              (catelim_string_of_children chs (catelim_string_of_labels ts ss))
      in
      "SEMANTICS" :: sep ::
        catelim_string_of_seq seq
          (sep ::
             catelim_string_of_list catelim_string_of_world sep worlds
               ("\n" :: ss))
