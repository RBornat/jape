(*
    $Id$

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
open Sequent.Funs
open Stringfuns
open Symbol
open Symboltype
open Termfuns
open Termstring
open Termparse
    
type forcedef =
    ForcePrim of term
  | ForceBoth of (forcedef * forcedef)
  | ForceEither of (forcedef * forcedef)
  | ForceIf of (forcedef * forcedef)
  | ForceEverywhere of forcedef
  | ForceNowhere of forcedef
  | ForceAll of (term * term list * forcedef)
  | ForceSome of (term * term list * forcedef)
                 (* pat, vars, body: a binder *)

let rec catelim_forcedefstring f ss =
  match f with
    ForcePrim t          -> "FORCE " :: catelim_argstring t ss
  | ForceBoth (f1, f2)   -> "BOTH (" :: catelim_forcedefstring f1 (") (" :: catelim_forcedefstring f2 (")"::ss))
  | ForceEither (f1, f2) -> "EITHER (" :: catelim_forcedefstring f1 (") (" :: catelim_forcedefstring f2 (")"::ss))
  | ForceIf (f1, f2)     -> "IF (" :: catelim_forcedefstring f1 (") (" :: catelim_forcedefstring f2 (")"::ss))
  | ForceEverywhere f    -> "EVERYWHERE " :: catelim_forcedefstring f ss
  | ForceNowhere f       -> "NOWHERE " :: catelim_forcedefstring f ss
  | ForceAll (t, vs, f)  -> "ALL " :: catelim_termstring t (" " :: catelim_forcedefstring f ss)
  | ForceSome (t, vs, f) -> "SOME " :: catelim_termstring t (" " :: catelim_forcedefstring f ss)

let rec forcedefstring f = implode (catelim_forcedefstring f [])

(* for some reason this went exponential when the body was a function.  
   Don't understand. RB vii/01 
 *)
let rec option_mapforcedefterms f fd =
  let omt = option_mapterm f in
  let omff = option_mapforcedefterms f in
  let ompair = option_rewrite2 omff omff in
  let omtvsfd = option_rewrite3 omt (option_rewritelist omt) omff in
  (* val _ = consolereport ["option_mapforcedefterms ", forcedefstring fd] *)
  let res =
    match fd with
      ForcePrim t -> (omt t &~~ (fSome <.> (fun v->ForcePrim v)))
    | ForceBoth pair ->
        (ompair pair &~~ (fSome <.> (fun v->ForceBoth v)))
    | ForceEither pair ->
        (ompair pair &~~ (fSome <.> (fun v->ForceEither v)))
    | ForceIf pair ->
        (ompair pair &~~ (fSome <.> (fun v->ForceIf v)))
    | ForceEverywhere fd' ->
        (omff fd' &~~ (fSome <.> (fun v->ForceEverywhere v)))
    | ForceNowhere fd' ->
        (omff fd' &~~ (fSome <.> (fun v->ForceNowhere v)))
    | ForceAll tvsfd ->
        (omtvsfd tvsfd &~~ (fSome <.> (fun v->ForceAll v)))
    | ForceSome tvsfd ->
        (omtvsfd tvsfd &~~ (fSome <.> (fun v->ForceSome v)))
  in
  (* consolereport ["option_mapforcedefterms ", forcedefstring fd, " => ", optionstring forcedefstring res]; *)
  res

let rec mapforcedefterms f = anyway (option_mapforcedefterms f)

let rec findinforcedef f fd =
  let rec findinpair (fd1, fd2) =
    (findinforcedef f fd1 |~~ (fun _ -> findinforcedef f fd2))
  in
  match fd with
    ForcePrim t -> findterm f t
  | ForceBoth pair -> findinpair pair
  | ForceEither pair -> findinpair pair
  | ForceIf pair -> findinpair pair
  | ForceEverywhere fd -> findinforcedef f fd
  | ForceNowhere fd -> findinforcedef f fd
  | ForceAll (t, _, fd) ->
      (findterm f t |~~ (fun _ -> findinforcedef f fd))
  | ForceSome (t, _, fd) ->
      (findterm f t |~~ (fun _ -> findinforcedef f fd))

let rec existsinforcedef f =
  opt2bool <.> findinforcedef (fun t -> if f t then Some true else None)

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
      (f, args) -> (termstring f, args)
  in
  let rec tranForceDef t =
    match decodeApp t with
      ("FORCE"       , [t]) -> ForcePrim (tranPrim t)
    | ("EVERYWHERE"  , [f]) -> ForceEverywhere (tranForceDef f)
    | ("NOWHERE"     , [f]) -> ForceNowhere (tranForceDef f)
    | ("ALL"    , [pat; f]) -> ForceAll (tranForceDefBinder pat f)
    | ("SOME"   , [pat; f]) -> ForceSome (tranForceDefBinder pat f)
    | ("BOTH"   , [f1; f2]) -> ForceBoth(tranForceDef f1, tranForceDef f2)
    | ("EITHER" , [f1; f2]) -> ForceEither(tranForceDef f1, tranForceDef f2)
    | ("IF"     , [f1; f2]) -> ForceIf(tranForceDef f1, tranForceDef f2)
    | _ -> raise (ParseError_ 
           ["FORCE t, EVERYWHERE f, NOWHERE f, ALL pat f, SOME pat f, BOTH f f, EITHER f f "; 
            "or IF f f expected in FORCEDEF; found "; termstring t
           ])
  
  and tranPrim t =
    try checkTacticTerm t; debracket t 
    with Tacastrophe_ ss -> 
           raise (ParseError_ ("FORCE " :: termstring t :: " contains " :: ss))
  
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
        let _ = scansymb () in 
        let rec parseInt () =
          match currsymb () with
            NUM n -> let _ = scansymb () in  atoi n
          | sy ->
              match symbolstring sy with
                "-" -> let _ = scansymb () in  - parseUnsignedInt "-"
              | "~" -> let _ = scansymb () in  - parseUnsignedInt "~"
              | s -> bang [s]
        and bang ss =
          raise
            (ParseError_ ("number expected in coordinate; found " :: ss))
        and parseUnsignedInt s1 =
          match currsymb () with
            NUM n -> let _ = scansymb () in  atoi n
          | s2 -> bang [s1; " followed by "; symbolstring s2]
        in
        let x = parseInt () in
        let y =
          if currsymb () = commasymbol then
            begin let _ = scansymb () in  parseInt () end
          else
            raise
              (ParseError_
                 ["comma expected after x value in world coordinate"])
        in
        begin match currsymb () with
          KET ")" -> let _ = scansymb () in  Coord (x, y)
        | sy ->
            raise
              (ParseError_
                 ["right paren expected after coordinate; found ";
                  symbolstring sy])
        end
    | sy ->
        raise
          (ParseError_
             ["coordinate expected, starting with left paren; found ";
              symbolstring sy])
  in
  let rec parseWorlds () =
    match currsymb () with
      SHYID "WORLD" ->
        let _ = scansymb () in 
        let c = parseCoord () in
        let chs =
          match currsymb () with
            SHYID "CHILDREN" ->
              let _ = scansymb () in 
              parseList
                (function
                   BRA "(" -> true
                 | _ -> false)
                (fun _ -> parseCoord ()) commasymbol
          | _ -> []
        in
        let ts =
          match currsymb () with
            SHYID "LABELS" ->
              let _ = scansymb () in  parseList canstartTerm parseTerm commasymbol
          | _ -> []
        in
        World (c, chs, ts) :: parseWorlds ()
    | _ -> []
  in
  match currsymb () with
    SHYID "SEMANTICS" ->
      let _ = scansymb () in 
      let seq = parseSeq () in
      begin match parseWorlds () with
        [] -> raise (ParseError_ ["empty disproof description"])
      | worlds -> Some (seq, Model worlds)
      end
  | _ -> None

let rec catelim_modelstring a1 a2 =
  match a1, a2 with
    None, ss -> ss
  | Some (seq, Model worlds), ss ->
      let sep = "\n" in
      let rec catelim_intstring i ss =
        (string_of_int : int -> string) i :: ss
      in
      let rec catelim_coordstring =
        fun (Coord c) ->
          catelim_pairstring catelim_intstring catelim_intstring "," c
      in
      let rec catelim_worldstring =
        fun (World (c, chs, ts)) ss ->
          let sep2 = sep ^ "  " in
          let rec catelim_childrenstring chs ss =
            match chs with
              [] -> ss
            | _ ->
                sep2 :: "CHILDREN" :: " " ::
                  catelim_liststring catelim_coordstring ", " chs ss
          in
          let rec catelim_labelsstring ts ss =
            match ts with
              [] -> ss
            | _ ->
                sep2 :: "LABELS" :: " " ::
                  catelim_liststring catelim_termstring ", " ts ss
          in
          "WORLD" :: " " ::
            catelim_coordstring c
              (catelim_childrenstring chs (catelim_labelsstring ts ss))
      in
      "SEMANTICS" :: sep ::
        catelim_seqstring seq
          (sep ::
             catelim_liststring catelim_worldstring sep worlds
               ("\n" :: ss))
