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
 
let consolereport = Miscellaneous.consolereport

let systemmenus = ["File"; "Edit"] (* filth; introduced by RB *)

exception Menuconfusion_ of string list

let menudebug = ref false
type menudata =
    Mseparator
  | Mentry       of (name * string option * string)
  | Mcheckbox    of (name * name * (string * string) * string option)
  | Mradiobutton of (name * (name * string) list * string option)
                (* variable  label  cmd            default cmd *)

type menucommand =
    MCdata   of menudata
  | MCbefore of (name * menudata) 
  | MCrename of (name * name) 

type paneldata =
    Pentry  of (name * string)
  | Pbutton of (name * panelbuttoninsert list)
  (* will have before, rename one day *)

type pentry = string * string

let str x = x

let rec string_of_checkbox c =
  string_of_quadruple string_of_name string_of_name (string_of_pair str str ",")
    (string_of_option str) "," c

let rec string_of_radiobutton r =
  string_of_triple string_of_name
    (bracketedstring_of_list (string_of_pair string_of_name str ",") ",")
    (string_of_option str) "," r

let rec string_of_menudata =
  function
    Mseparator      -> "Mseparator"
  | Mentry me       -> "Mentry " ^ string_of_triple string_of_name (string_of_option str) str "," me
  | Mcheckbox mc    -> "Mcheckbox " ^ string_of_checkbox mc
  | Mradiobutton mr -> "Mradiobutton " ^ string_of_radiobutton mr
  
let string_of_menucommand = 
  function
    MCdata   md -> "MCdata (" ^ string_of_menudata md ^ ")"
  | MCbefore mb -> "MCbefore " ^ string_of_pair string_of_name string_of_menudata "," mb
  | MCrename mr -> "MCrename " ^ string_of_pair string_of_name string_of_name "," mr

let rec string_of_panelbuttoninsert =
  function
    StringInsert s -> enQuote s
  | LabelInsert    -> "LABEL"
  | CommandInsert  -> "COMMAND"

let rec string_of_paneldata =
  function
    Pentry pe -> "Pentry " ^ string_of_pair string_of_name str "," pe
  | Pbutton pb ->
      "Pbutton " ^
        string_of_pair string_of_name
          (bracketedstring_of_list string_of_panelbuttoninsert ",") "," pb

(* Order of insertion no longer exploits the details of mapping implementation.
 * There is a lot of silliness in what follows, which attempts to keep things
 * in the same order no matter how often the same data is thrown at addmenudata
 * or addpaneldata
 *)

let menus : (name, (bool*menudata list ref)) mapping ref = ref empty

let panels :
  (name, (panelkind * (name, string ref) mapping * paneldata list) ref)
   mapping ref =
  ref empty

let rec addtodata beforeopt lf vf e es =
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
  let rec delete ps = (not <.> conflicts) <| ps in
  let rec insert =
    function
      []        -> [e]
    | e' :: es' ->
        if conflicts e' then 
          e :: delete es' 
        else
        if bool_of_opt beforeopt && beforeopt=lf e' then
          e' :: e :: delete es'
        else
          e' :: insert es'
  in
  if List.exists conflicts es || bool_of_opt beforeopt then insert es else e::es

let rec addmenu proofsonly m =
  match (!menus <@> m) with
    None   -> menus := (!menus ++ (m |-> (proofsonly, ref [])))
  | Some _ -> ()

let rec addtomenu beforeopt e es =
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
  if !menudebug then consolereport ["adding "; string_of_menudata e];
  addtodata beforeopt lf vf e es

let renamemenu name name' es =
  let lf e =
    match e with 
      Mseparator                 -> None
    | Mentry (label, x, y)       -> 
        if name=label then Some (Mentry (name', x, y)) else None
    | Mcheckbox (label, x, y, z) -> None (* too complicated to rename these -- see paragraphfuns.ml *)
        (* if name=label then Some (Mcheckbox (name', x, y, z)) else None *)
    | Mradiobutton (x, lcs, y)   -> None (* ditto -- but if I moved the tick etc. responsibility to the GUI ...  *)
        (* optionmap (fun (label,z) -> if name=label then Some (name',z) else None) lcs &~~
           (fun lcs' -> Some (Mradiobutton (x, lcs', y))) *)
  in
    match option_rewritelist lf es with
      Some es' -> es'
    | None     -> es
      
let rec addmenudata m es =
  let doit cs =
    function
      MCdata md              -> addtomenu None md cs
    | MCbefore (name, md)    -> addtomenu (Some [name]) md cs
    | MCrename (name, name') -> renamemenu name name' cs
  in
  if !menudebug then consolereport ["adding to "; string_of_name m];
  match (!menus <@> m) with
    Some (_, contents) -> List.iter (fun e -> contents := doit !contents e) es
  | None               -> raise (Menuconfusion_ ["no menu called "; string_of_name m; " (addmenudata)"])

let rec clearmenudata m =
  match (!menus <@> m) with
    Some (_, contents) -> contents := []
  | None               -> ()

let rec getmenus () = 
  let names = dom !menus in
  List.map (fun m -> fst(_The(!menus <@> m)),m) names

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
        if member (string_of_name m, systemmenus) then Mseparator :: es'
        else es'
    | es' -> es'
  in
    (!menus <@> m) &~~
    (fun (proofsonly, es) -> Some (proofsonly, tidy(List.rev !es)))

let rec addpanel k p =
  match (!panels <@> p) with
    None -> panels := (!panels ++ (p |-> ref (k, empty, [])))
  | Some _ -> ()

let rec addtopanel beforeopt e (k, em, bs as stuff) =
  let rec lf e =
    match e with
      Pentry  (label, _) -> Some [label]
    | Pbutton (label, _) -> Some [label]
  in
  let rec vf e =
    match e with
      Pentry (_, _)  -> None
    | Pbutton (_, _) -> None
  in
  match e with
    Pentry (label, cmd) ->
      begin match (em <@> label) with
        Some cref -> cref := cmd; stuff
      | None -> k, (em ++ (label |-> ref cmd)), bs
      end
  | Pbutton _ as b -> k, em, addtodata beforeopt lf vf b bs

let rec addpaneldata p es =
  match (!panels <@> p) with
    Some contents -> List.iter (fun e -> contents := addtopanel None e !contents) es
  | None ->
      raise (Menuconfusion_ ["no panel called "; string_of_name p; " (addpaneldata)"])

let rec clearpaneldata p =
  match (!panels <@> p) with
    Some ({contents = k, _, _} as contents) -> contents := k, empty, []
  | None -> ()

let rec getpanels () =
   (fun (p, {contents = k, _, _}) -> p, k) <* aslist !panels

let rec getpanelkind p =
  (!panels <@> p) &~~ (fSome <.> (fun (a,b,c) -> a) <.> (!))

let rec getpaneldata p =
    ((!panels <@> p) &~~
     (let applyname = name_of_string "Apply" in
        fSome <.> 
        (fun {contents = k, em, bs} ->
            nj_fold (fun ((l, {contents = c}), es) -> Pentry (l, c) :: es)
              (aslist em)
              (match bs, k with
                 [], ConjecturePanelkind ->
                   [Pbutton (applyname, [StringInsert "applyconjecture"; CommandInsert])]
               | [], GivenPanelkind ->
                   [Pbutton (applyname, [StringInsert "applygiven"; CommandInsert])]
               | _ -> List.rev bs))))

let rec clearmenusandpanels () = menus := empty; panels := empty
(***** temporary, for backwards compatibility *****)


let rec assignvarval var vval =
  (("assign " ^ parseablestring_of_name var) ^ " ") ^ vval

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
  if !menudebug then consolereport ["reporting on "; string_of_name m];
  match getmenudata m with
    Some (_, es) -> List.iter tran es
  | None -> ()

let rec panelitemiter p ef bf (* cbf rbf *) =
  let rec tran e =
    match e with
      Pentry  pe -> ef pe
    | Pbutton be -> bf be
    (* | Pcheckbox (var, label, (val1, val2), _) ->
           cbf (label, assignvarval var val1)
       | Pradiobutton (var, lcs, _) ->
           rbf ((fun (label, vval) -> label, assignvarval var vval) <* lcs)
     *)
  in
  match getpaneldata p with
    Some es -> List.iter tran es
  | None -> ()
