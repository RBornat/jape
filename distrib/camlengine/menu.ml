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
open Mappingfuns 
open Miscellaneous
open Name
open Optionfuns 
open Panelkind
open Sml
open Stringfuns

type panelkind = Panelkind.panelkind
 and panelbuttoninsert = Panelkind.panelbuttoninsert
 and name = Name.name
 

let systemmenus = ["File"; "Edit"] (* filth; introduced by RB *)

exception Menuconfusion_ of string list

let menudebug = ref false
type menudata =
    Mseparator
  | Mentry       of (name * string option * string)
  | Mcheckbox    of (name * name * (string * string) * string option)
  | Mradiobutton of (name * (name * string) list * string option)
                (* variable  label  cmd            default cmd *)

type paneldata =
    Pentry       of (name * string)
  | Pbutton      of (name * panelbuttoninsert list)
  | Pcheckbox    of (name * name * (string * string) * string option)
  | Pradiobutton of (name * (name * string) list * string option)
                (* variable  label  cmd            default cmd *)

type pentry = string * string

let str x = x

let rec checkboxstring c =
  quadruplestring namestring namestring (pairstring str str ",")
    (optionstring str) "," c

let rec radiobuttonstring r =
  triplestring namestring
    (bracketedliststring (pairstring namestring str ",") ",")
    (optionstring str) "," r

let rec menudatastring =
  function
    Mseparator      -> "Mseparator"
  | Mentry me       -> "Mentry " ^ triplestring namestring (optionstring str) str "," me
  | Mcheckbox mc    -> "Mcheckbox " ^ checkboxstring mc
  | Mradiobutton mr -> "Mradiobutton " ^ radiobuttonstring mr

let rec panelbuttoninsertstring =
  function
    StringInsert s -> enQuote s
  | LabelInsert    -> "LABEL"
  | CommandInsert  -> "COMMAND"

let rec paneldatastring =
  function
    Pentry pe -> "Pentry " ^ pairstring namestring str "," pe
  | Pbutton pb ->
      "Pbutton " ^
        pairstring namestring
          (bracketedliststring panelbuttoninsertstring ",") "," pb
  | Pcheckbox pc -> "Pcheckbox " ^ checkboxstring pc
  | Pradiobutton pr -> "Pradiobutton " ^ radiobuttonstring pr
(* Order of insertion no longer exploits the details of mapping implementation.
 * There is a lot of silliness in what follows, which attempts to keep things
 * in the same order no matter how often the same data is thrown at addmenudata
 * or addpaneldata
 *)


let menus : (name, menudata list ref) mapping ref = ref empty

let panels
  :
  (name, (panelkind * (name, string ref) mapping * paneldata list) ref)
   mapping ref =
  ref empty

let rec addtodata lf vf e es =
  let lsopt = lf e in
  let vopt = vf e in
  let rec conflicts e' =
    (match lsopt, lf e' with
       Some ls, Some ls' ->
         List.exists (fun l' -> List.exists (fun l -> l = l') ls) ls'
     | _ -> false) ||
    (match vopt, vf e' with
       Some v, Some v' -> v = v'
     | _ -> false)
  in
  let rec delete ps = (not <*> conflicts) <| ps in
  let rec insert =
    function
      [] -> []
    | e' :: es' ->
        (* won't happen, but who cares? *)
        if conflicts e' then e :: delete es' else e' :: insert es'
  in
  match lsopt, vopt with
    None, None -> e :: es
  | _ ->(* I think this reduces churn *)
     if List.exists conflicts es then insert es else e :: es

let rec addmenu m =
  match at (!menus, m) with
    None -> menus := ( ++ ) (!menus, ( |-> ) (m, ref []))
  | Some _ -> ()

let rec addtomenu e es =
  let rec lf e =
    match e with
      Mseparator                 -> None
    | Mentry (label, _, _)       -> Some [label]
    | Mcheckbox (label, _, _, _) -> Some [label]
    | Mradiobutton (_, lcs, _)   -> Some ((fst <* lcs))
  in
  let rec vf e =
    match e with
      Mseparator                 -> None
    | Mentry (_, _, _)           -> None
    | Mcheckbox (_, var, _, _)   -> Some var
    | Mradiobutton (var, lcs, _) -> Some var
  in
  if !menudebug then consolereport ["adding "; menudatastring e];
  match e with
    Mseparator -> e :: es
  | _          -> addtodata lf vf e es

let rec addmenudata m es =
  if !menudebug then consolereport ["adding to "; namestring m];
  match at (!menus, m) with
    Some contents -> List.iter (fun e -> contents := addtomenu e !contents) es
  | None ->
      raise
        (Menuconfusion_
           ["no menu called "; namestring m; " (addmenudata)"])

let rec clearmenudata m =
  match at (!menus, m) with
    Some contents -> contents := []
  | None -> ()

let rec getmenus () = dom !menus

let rec getmenudata m =
  let rec doseps =
    function
      Mseparator :: Mseparator :: es -> doseps (Mseparator :: es)
    | [Mseparator] -> []
    | Mseparator :: Mradiobutton r :: es ->
        Mseparator :: Mradiobutton r :: doseps (Mseparator :: es)
    | Mradiobutton r :: es ->
        Mseparator :: Mradiobutton r :: doseps (Mseparator :: es)
    | e :: es -> e :: doseps es
    | [] -> []
  in
  let rec tidy es =
    (* system menus can start with a separator, because they
     * (e.g. File, Edit) have entries pre-defined by the interface
     * server; no other menu can start with a separator
     *)
    match doseps es with
      Mseparator :: es' ->
        if member (namestring m, systemmenus) then Mseparator :: es'
        else es'
    | es' -> es'
  in
    at (!menus, m) &~~
    (fSome <*> tidy <*> List.rev <*> (!))

let rec addpanel k p =
  match at (!panels, p) with
    None -> panels := ( ++ ) (!panels, ( |-> ) (p, ref (k, empty, [])))
  | Some _ -> ()

let rec addtopanel e (k, em, bs as stuff) =
  let rec lf e =
    match e with
      Pentry (label, _) -> Some [label]
    | Pbutton (label, _) -> Some [label]
    | Pcheckbox (label, _, _, _) -> Some [label]
    | Pradiobutton (_, lcs, _) -> Some ((fst <* lcs))
  in
  let rec vf e =
    match e with
      Pentry (_, _) -> None
    | Pbutton (_, _) -> None
    | Pcheckbox (_, var, _, _) -> Some var
    | Pradiobutton (var, _, _) -> Some var
  in
  match e with
    Pentry (label, cmd) ->
      begin match at (em, label) with
        Some cref -> cref := cmd; stuff
      | None -> k, ( ++ ) (em, ( |-> ) (label, ref cmd)), bs
      end
  | b -> k, em, addtodata lf vf b bs

let rec addpaneldata p es =
  match at (!panels, p) with
    Some contents -> List.iter (fun e -> contents := addtopanel e !contents) es
  | None ->
      raise
        (Menuconfusion_
           ["no panel called "; namestring p; " (addpaneldata)"])

let rec clearpaneldata p =
  match at (!panels, p) with
    Some ({contents = k, _, _} as contents) -> contents := k, empty, []
  | None -> ()

let rec getpanels () =
   (fun (p, {contents = k, _, _}) -> p, k) <* aslist !panels

let rec getpanelkind p =
  at (!panels, p) &~~ (fSome <*> (fun (a,b,c) -> a) <*> (!))

let rec getpaneldata p =
    (at (!panels, p) &~~
     (let applyname = namefrom "Apply" in
        fSome <*> 
          ((fun {contents = k, em, bs} ->
              nj_fold (fun ((l, {contents = c}), es) -> Pentry (l, c) :: es)
                (aslist em)
                (match bs, k with
                   [], ConjecturePanelkind ->
                     [Pbutton
                        (applyname,
                         [StringInsert "apply"; CommandInsert])]
                 | [], GivenPanelkind ->
                     [Pbutton
                        (applyname,
                         [StringInsert "applygiven"; CommandInsert])]
                 | _ -> List.rev bs))
             )))

let rec clearmenusandpanels () = menus := empty; panels := empty
(***** temporary, for backwards compatibility *****)


let rec assignvarval var vval =
  (("assign " ^ parseablenamestring var) ^ " ") ^ vval

let rec menuiter f = List.iter f (getmenus ())

let rec paneliter f = List.iter f (getpanels ())

let rec menuitemiter m ef cbf rbf sf =
  let rec tran e =
    match e with
      Mseparator                              -> sf ()
    | Mentry (label, shortcut, cmd)           -> ef (label, shortcut, cmd)
    | Mcheckbox (var, label, (val1, val2), _) -> cbf (label, assignvarval var val1)
    | Mradiobutton (var, lcs, _)              ->
        (* this will be reset *)
        rbf (List.map (fun (label, vval) -> label, assignvarval var vval) lcs)
  in
  if !menudebug then consolereport ["reporting on "; namestring m];
  match getmenudata m with
    Some es -> List.iter tran es
  | None -> ()

let rec panelitemiter p ef bf cbf rbf =
  let rec tran e =
    match e with
      Pentry pe -> ef pe
    | Pbutton be -> bf be
    | Pcheckbox (var, label, (val1, val2), _) ->
        cbf (label, assignvarval var val1)
    | Pradiobutton (var, lcs, _) ->
        rbf
          ((fun (label, vval) -> label, assignvarval var vval) <* lcs)
  in
  match getpaneldata p with
    Some es -> List.iter tran es
  | None -> ()
