(* $Id$ *)

open Listfuns
open Miscellaneous
open Optionfuns 
open Sml
open Sequent.Funs
open Stringfuns
open Symbol
open Symboltype
open Term.Funs
open Term.Termstring
open Termparse
    
type forcedef =
    ForcePrim of term
  | ForceBracket of forcedef
  | ForceAnd of (forcedef * forcedef)
  | ForceOr of (forcedef * forcedef)
  | ForceImplies of (forcedef * forcedef)
  | ForceEverywhere of forcedef
  | ForceNowhere of forcedef
  | ForceAll of (term * term list * forcedef)
  | ForceSome of (term * term list * forcedef)
                 (* pat, vars, body: a binder *)

let rec catelim_forcedefstring f ss =
  match f with
    ForcePrim t -> "FORCE " :: catelim_termstring t ss
  | ForceBracket f -> "(" :: catelim_forcedefstring f (")" :: ss)
  | ForceAnd (f1, f2) ->
      catelim_forcedefstring f1 (" AND " :: catelim_forcedefstring f2 ss)
  | ForceOr (f1, f2) ->
      catelim_forcedefstring f1 (" OR " :: catelim_forcedefstring f2 ss)
  | ForceImplies (f1, f2) ->
      catelim_forcedefstring f1
        (" IMPLIES " :: catelim_forcedefstring f2 ss)
  | ForceEverywhere f -> "EVERYWHERE " :: catelim_forcedefstring f ss
  | ForceNowhere f -> "NOWHERE " :: catelim_forcedefstring f ss
  | ForceAll (t, vs, f) ->
      "ALL " :: catelim_termstring t (" " :: catelim_forcedefstring f ss)
  | ForceSome (t, vs, f) ->
      "Some " :: catelim_termstring t (" " :: catelim_forcedefstring f ss)
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
      ForcePrim t -> (omt t &~~ (fSome <*> (fun v->ForcePrim v)))
    | ForceBracket fd' ->
        (omff fd' &~~ (fSome <*> (fun v->ForceBracket v)))
    | ForceAnd pair ->
        (ompair pair &~~ (fSome <*> (fun v->ForceAnd v)))
    | ForceOr pair ->
        (ompair pair &~~ (fSome <*> (fun v->ForceOr v)))
    | ForceImplies pair ->
        (ompair pair &~~ (fSome <*> (fun v->ForceImplies v)))
    | ForceEverywhere fd' ->
        (omff fd' &~~ (fSome <*> (fun v->ForceEverywhere v)))
    | ForceNowhere fd' ->
        (omff fd' &~~ (fSome <*> (fun v->ForceNowhere v)))
    | ForceAll tvsfd ->
        (omtvsfd tvsfd &~~ (fSome <*> (fun v->ForceAll v)))
    | ForceSome tvsfd ->
        (omtvsfd tvsfd &~~ (fSome <*> (fun v->ForceSome v)))
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
  | ForceBracket fd -> findinforcedef f fd
  | ForceAnd pair -> findinpair pair
  | ForceOr pair -> findinpair pair
  | ForceImplies pair -> findinpair pair
  | ForceEverywhere fd -> findinforcedef f fd
  | ForceNowhere fd -> findinforcedef f fd
  | ForceAll (t, _, fd) ->
      (findterm f t |~~ (fun _ -> findinforcedef f fd))
  | ForceSome (t, _, fd) ->
      (findterm f t |~~ (fun _ -> findinforcedef f fd))
let rec existsinforcedef f =
  opt2bool <*> findinforcedef (fun t -> if f t then Some true else None)
let rec parseForceDef () =
  (* there is no operator priority in forcedefs ... *)
  let f =
    match currsymb () with
      SHYID "FORCE" -> let _ = scansymb () in ForcePrim (parseTerm EOF)
    | SHYID "EVERYWHERE" ->
        let _ = scansymb () in  ForceEverywhere (parseForceDef ())
    | SHYID "NOWHERE" -> let _ = scansymb () in  ForceNowhere (parseForceDef ())
    | SHYID "ALL" -> let _ = scansymb () in  ForceAll (parseForceDefBinder ())
    | SHYID "Some" -> let _ = scansymb () in  ForceSome (parseForceDefBinder ())
    | BRA "(" ->
        let _ = scansymb () in 
        ForceBracket
          (let r = parseForceDef () in
           (if currsymb () = KET ")" then let _ = scansymb () in ()
            else
              raise
                (ParseError_
                   ["closing bracket expected after FORCE definition; found ";
                    symbolstring (currsymb ())])); r)
    | sy ->
        raise
          (ParseError_
             ["FORCE, EVERYWHERE, NOWHERE, ALL, Some or bracket expected in ";
              "FORCE definition; found "; symbolstring sy])
  in
  match currsymb () with
    SHYID "AND" -> let _ = scansymb () in  ForceAnd (f, parseForceDef ())
  | SHYID "OR" -> let _ = scansymb () in  ForceOr (f, parseForceDef ())
  | SHYID "IMPLIES" -> let _ = scansymb () in  ForceImplies (f, parseForceDef ())
  | _ -> f
and parseForceDefBinder () =
  let t = parseTerm EOF in
  let vs = isVariable <| termvars t in
  let f = parseForceDef () in
  if List.exists (not <*> isextensibleId) vs then
    raise
      (ParseError_
         ["ALL and Some must use CLASS VARIABLE identifiers to describe individuals"])
  else t, vs, f
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
