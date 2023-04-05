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

(*
        tcl/tk implementation of the Jape server
        Bernard Sufrin,
        Oxford July 1993 et.seq, up to January 2002 

*)

open Array
open Box
open Displayfont
open Displayclass
open Invisibles
open Listfuns
open Miscellaneous
open Sml
open UTF

type box = Box.box
 and displayclass = Displayclass.displayclass
 and element = Termtype.element
 and font = Displayfont.displayfont
 and pane = Displayfont.pane
 and panelbuttoninsert = Panelkind.panelbuttoninsert
 and panelkind = Panelkind.panelkind
 and pos = Box.pos
 and size = Box.size
 and textsize = Box.textsize

let consolereport = Miscellaneous.consolereport
let enQuote = Stringfuns.enQuote
let string_of_element = Termstring.string_of_element

(* Fit Richard's box adt back to into his earlier concrete form *)
let rec explodeBox box = bPos box, bSize box
let rec explodePos pos = posX pos, posY pos
let rec explodeSize s = sW s, sH s
let rec explodeTextSize s = tsW s, tsA s, tsD s (* width, ascent, descent *)

let infromGUI = stdin 
let outtoGUI = stdout

let _GUIresponded = ref false

exception DeadGUI_

let deadGUI () =
  consolereport ["the GUI is not responding"; 
                 if !_GUIresponded then "" 
                 else " (and has never responded)"];
  raise DeadGUI_
  
(* if the server is dead, this will definitely cause a problem *)
let flush s =
  try flush outtoGUI 
  with Sys_error _ -> deadGUI()
  |    exn -> consolereport [Printexc.to_string exn; " in Japeserver.flush "; s];
              deadGUI()

let out s = 
  (* I can imagine that stuffing loads of stuff down the pipe, if the server
     has crashed, could generate a broken pipe signal
   *)
  try output_string outtoGUI s 
  with Sys_error _ -> deadGUI()
  |    exn -> consolereport [Printexc.to_string exn; " in Japeserver.out "];
              deadGUI()

let write s = out s; out "\n"; flush s

let rec visible s = implode (List.map vis (utf8_explode s))
and vis c = if isInvisibleUcode c then "\\" ^ string_of_int c else utf8_of_ucode c
    
(* if the server has crashed, input_line may give an exception *)
let readline s = 
  flush s; 
  let r = (try input_line infromGUI
                 with End_of_file -> deadGUI()
                 |    exn -> consolereport [Printexc.to_string exn; " in Japeserver.readline "; s];
                             deadGUI())
  in
  _GUIresponded := true; r

let rec readlines s =
  function
    0 -> []
  | n -> readline s :: readlines s (n-1)
;;

(* this is a strange bit of code.  I _think_ it converts a space-separated sequence of numbers
   into a list of numbers.
 *)
let rec nn_ =
  function
    dash :: ds when dash=Char.code '-' -> - (dd_ 0 ds)
  | ds                                 -> dd_ 0 ds

and dd_ a1 a2 =
  match a1, a2 with
    r, []      -> r
  | r, d :: ds -> dd_ (r * 10 + d - Char.code '0') ds

let rec ints_of_reply line =
  let rec ff_ a1 a2 a3 =
    match a1, a2, a3 with
      s, r, []         -> nn_ s :: r
    | s, r, 0x20 :: xs -> ff_ [] (nn_ s :: r) xs
    | s, r, x :: xs    -> ff_ (x :: s) r xs
  in
  ff_ [] [] (List.rev (utf8_explode line))

let atoi s = nn_ (utf8_explode s)
let atob =
  function 
    "0" -> false
  | "1" -> true
  | s   -> raise (Catastrophe_ ["Japeserver.atob "; Stringfuns.enQuote s])
  
let rec strings_of_reply s =
  let rec ff_ a1 a2 a3 =
    match a1, a2, a3 with
      w, r, []         -> utf8_implode w :: r
    | w, r, 0x91 :: cs -> ff_ [] (utf8_implode w :: r) cs
    | w, r, c    :: cs -> ff_ (c :: w) r cs
  in
  ff_ [] [] (List.rev (utf8_explode s))

type _ITEM = Bool of bool | Int of int | Str of string

let string_of__ITEM =
  function
    Bool b -> "Bool "^string_of_bool b
  | Int  i -> "Int "^string_of_int i
  | Str  s -> "Str "^enQuote s

let _Bool v = Bool v
let _Int v  = Int  v
let _Str v  = Str  v

(* 0x25 = '%' *)
let rec writef orig_s orig_is =
  let signedstring_of_int i = if i < 0 then "-" ^ string_of_int (- i) else string_of_int i in
  let bad message =
    raise (Catastrophe_ ["Japeserver.writef "; message; " "; enQuote orig_s; " ";
                         bracketed_string_of_list string_of__ITEM ";" orig_is])
  in
  let rec ww_ cs is =
    match cs, is with
    | []                , []           -> ()
    | []                , _            -> bad "too many items"
    | 0x25 :: 0x25 :: cs, is           -> out (utf8_of_ucode 0x25); ww_ cs is
    | 0x25 :: cs,         Bool b :: is -> out (if b then "T" else "F"); ww_ cs is
    | 0x25 :: cs,         Int i  :: is -> out (signedstring_of_int i); ww_ cs is
    | 0x25 :: cs,         Str s  :: is -> outs s; ww_ cs is
    | 0x25 :: cs,                   [] -> bad "too few items"
    | c    :: cs,                   is -> out (utf8_of_ucode c); ww_ cs is
  in
  ww_ (utf8_explode orig_s) orig_is

and outs s =
  List.iter
    (function
     | sp    when    sp=Char.code ' '  -> out8 sp
     | nl    when    nl=Char.code '\n' -> out "\\n"
     | dq    when    dq=Char.code '\"' -> out "\\\""
     | slosh when slosh=Char.code '\\' -> out "\\\\"
     | c -> if c < 32 then out8 c else out (utf8_of_ucode c)
    )
    (utf8_explode s)

and out8 c =
  let r =
    implode
      ["\\"; string_of_int (c / 64); string_of_int (c mod 64 / 8); string_of_int (c mod 8)]
  in
  (*consolereport ["mks8 ", string_of_int c, " => ", r];*)
  out r

let intlist_of_ask s is = writef s is; ints_of_reply (readline s)
let string_of_ask  s is = writef s is; readline s

let listen () = writef "GET\n" []; readline "GET"
let terminateGUI () = writef "TERMINATE\n" []
let dropdead () = writef "ENGINECRASHED\n" []; exit(2)

(*  Local to the interface *)

(* client stuff *)

(* this bit is dead, but not forgotten, and may one day be resurrected *)
let idlsignature = ""
let rec getSignature () = ""

(* By default all drawing takes place in the front window.
 * When fonts change, background windows have to be refreshed.
 * The following allow the engine temporarily to redirect
 * drawing to a background window.  If canbackgroundfocus is false,
 * it won't do such a thing.
 *)
 
let canbackgroundfocus = true
let rec setbackgroundfocus _ = ()
let rec setforegroundfocus _ = ()

let rec sendVersion v = writef "VERSION %\n" [Str v]

let rec sendOperators strings =
  writef "OPERATORSBEGIN\n" [];
  List.iter (fun s -> writef "OPERATOR %\n" [Str s]) strings;
  writef "OPERATORSEND\n" []
(* fonts *)

let rec setFonts stringfromtheory =
  writef "SETFONTS %\n" [Str stringfromtheory]

exception Fontinfo_

let rec fontinfo fontn =
    match intlist_of_ask "FONTINFO %\n" [Int (int_of_displayfont fontn)] with 
      [asc; desc; lead]  -> asc, desc, lead
    | _ -> raise Fontinfo_

let rec getPointSize n =
  (* actually it's font height, and it isn't used *)
  match intlist_of_ask "POINTSIZE %\n" [Int n] with
    [n] -> n
  | _   -> output_string stderr "[POINTSIZE FAILS]\n"; 0

let invischars : string list ref = ref []

let rec printable s =
  utf8_implode ((not <.> isInvisibleUcode) <| utf8_explode s)

let fontnames : string array ref = ref (Array.make 0 "")

let resetfontnames () = fontnames := Array.make 0 ""

let setFontNames fs =
   if List.length fs = 0 then 
     raise (Catastrophe_ ["Japeserver.setFontNames: empty font name list"])
   else 
     fontnames := Array.of_list fs

let rec getfontname n =
  let nfonts = Array.length !fontnames in
  if n >= 0 && n < nfonts
  then Array.get !fontnames n
  else 
  if nfonts = 0 then (* we never initialised it *) (
    writef "FONTNAMES\n" [];
    let fs = strings_of_reply (readline "FONTNAMES") in 
    setFontNames fs; getfontname n 
  )
  else
    raise (Catastrophe_ ["Japeserver.getfontname can't decode fontnumber "; string_of_int n])

(* let rec getfontname n =
     try Array.get !fontnames n 
     with Invalid_argument _ -> 
           (if length !fontnames = 0 then (* we never initialised it *) (
              writef "FONTNAMES\n" [];
              let fs = strings_of_reply (readline "FONTNAMES") in 
              setFontNames fs; getfontname n )
            else
              raise (Catastrophe_ ["Japeserver.getfontname can't decode fontnumber "; 
                                   string_of_int n])
          )
 *)  
open Hashtbl

let stringSizeCache : (string*string, int*int*int) Hashtbl.t = 
  Hashtbl.create 251 (* usual comment *)

exception Measurestring_ of int * string * int list

let rec measurestring font string =
  let fontnum = int_of_displayfont font in
  let fontname = getfontname fontnum in
  try Hashtbl.find stringSizeCache (fontname,string) 
  with Not_found -> 
    (let wad = 
       match intlist_of_ask "STRINGSIZE % %\n" [Int fontnum; Str (printable string)] with
       | [width; asc; desc] -> width, asc, desc
       | ns                 -> raise (Measurestring_ (fontnum, printable string, ns))
     in
     Hashtbl.add stringSizeCache (fontname,string) wad;
     wad
   )

let rec loadFont (fontn, name) =
  writef "LOADFONT % %\n" [Int fontn; Str name]

(***************)

let linethickness = ref 1

let rec setinvischars
  (onbra, onket) (offbra, offket) (outbra, outket) (lockbra, lockket) =
  invischars :=
    [onbra; onket; offbra; offket; outbra; outket; lockbra; lockket];
  writef "SETINVISCHARS % % % % % % % %\n"
    (List.map (fun s -> Str s) !invischars)

(* these are idleness formalised. See comment in dialogue.ml *)

let rec settextselectionmode m = 
  writef "SETTEXTSELECTIONMODE %\n" 
           [Int (match m with
                   "subformula" -> 0
                 | "token"      -> 1
                 | _ -> raise (Catastrophe_ ["Japeserver.settextselectionmode "; 
                                             Stringfuns.enQuote m]))]

let rec setmultihypsel m = 
  writef "SETMULTIHYPSEL %\n" 
           [Bool (match m with
                  | "true"  -> true
                  | "false" -> false
                  | _ -> raise (Catastrophe_ ["Japeserver.multihypsel "; Stringfuns.enQuote m])
                 )
           ]

let rec drawLine pos1 pos2 =
  let (x1, y1) = explodePos pos1 in
  let (x2, y2) = explodePos pos2 in
  writef "DRAWLINE % % % %\n" (List.map _Int [x1; y1; x2; y2])

let rec drawRect box s =
  let (pos, size) = explodeBox box in
  let (x, y) = explodePos pos in
  let (w, h) = explodeSize size in
  writef "DRAWRECT % % % % %\n"
         [Int x; Int y; Int w; Int h; Str s]

let rec drawinpane pane = writef "DRAWINPANE %\n" [Int (int_of_pane pane)]

let rec drawstring (font, class__, s, pos) =
  let (x, y) = explodePos pos in
  writef "DRAWSTRING % % % % %\n"
         [Int x; Int y; Int font; Int class__; Str s]

let rec showAlert s = writef "SETALERT %\n" [Str s]

let rec drawmeasuredtext class__ lines pos =
  (* : displayclass -> (pos*font*string) list -> pos -> unit *)
  (* consolereport ["drawmeasuredtext ";
                 string_of_displayclass class__;
                 " ";
                 bracketed_string_of_list 
                    (Stringfuns.string_of_triple 
                        string_of_pos string_of_displayfont Stringfuns.enQuote ",") 
                    ";\n" lines;
                 " ";
                 string_of_pos pos]; *)
  let classn = int_of_displayclass class__ in
  match lines with
    [pos', font, string] ->
      let fontn = int_of_displayfont font in
      drawstring (fontn, classn, string, Box.(+->+) pos pos')
  | [] -> ()
  | _  ->
      let dostring (pos, font, s) =
        let (x, y) = explodePos pos in
        writef "DRAWMT % % % %\n" [Int x; Int y; Int (int_of_displayfont font); Str s]
      in
      let (x, y) = explodePos pos in
      writef "DRAWMEASUREDTEXT % % % %\n" [Int x; Int y; Int classn; Int (List.length lines)]; 
      List.iter dostring lines;
      writef "DRAWMEASUREDTEXTEND\n" []

(* just like string size, we can't use a procrustean Cache, dammit! *)
let procrusteanCache : (int*string*string*string, string) Hashtbl.t = 
  Hashtbl.create 251 (* usual comment *)

let procrustes width ellipsis font text =
  if fst_of_3 (measurestring font text) <= width then text 
  else
    (let fontnum = int_of_displayfont font in
     let fontname = getfontname fontnum in
     let arg = (width,text,ellipsis,fontname) in
     try Hashtbl.find procrusteanCache arg with
     Not_found -> (
       let trunc = 
         writef "PROCRUSTES % % % %\n" [Int width; Str text; Str ellipsis; Int fontnum];
         readline "PROCRUSTES"
       in
       Hashtbl.add procrusteanCache arg trunc;
       trunc
     ))

let howToText s = 
  match intlist_of_ask "HOWTOTEXT %\n" [Str s] with
    [n] ->
      let rec get n =
        let s = readline "multiLine" in
        if n=0 then s else s ^ "\n" ^ get (n-1)
      in
      get n
  | _ -> raise (Catastrophe_ ["multiLine protocol failure (HOWTOTEXT)"])

(***************)

let commentSet = ref false

let rec setComment s =
  if s = "" && not !commentSet then ()
  else begin commentSet := true; writef "SETCOMMENT %\n" [Str s] end

let rec ask_unpatched severity message buttons default =
  writef "ASKSTART\n" [];
  List.iter (fun but -> writef "ASKBUTTON %\n" [Str but]) buttons;
  match
    intlist_of_ask "ASKNOW % % %\n" [Int severity; Str message; Int default]
  with
    [n] -> n (* warning: can be -1 if the interface doesn't stop Windoze users closing alerts *)
  | _   -> raise (Catastrophe_ ["ask protocol failure"])

let rec askDangerously_unpatched message doit dont =
  match
    intlist_of_ask "ASKDANGEROUSLY % % %\n" [Str message; Str doit; Str dont]
  with
    [0] -> None
  | [n] -> if n<0 then None else Some (n - 1)
  | ns  -> raise (Catastrophe_ ["askDangerously protocol failure ";
                                bracketed_string_of_list string_of_int ";" ns])

let rec askCancel_unpatched severity message buttons default =
  let n = ask_unpatched severity message (buttons @ ["Cancel"]) default in
  if n<0 || n = List.length buttons then None else Some n

let rec echo s = s

let menus = ref []

let menusVisible = ref false

let existsmenu name = name="File" || name="Edit" || List.exists (fun m -> m=name) !menus

let emptymenusandpanels lemmasAllowed =
  writef "EMPTYMENUSANDPANELS %\n" [Bool lemmasAllowed];
  menus := [];
  menusVisible := false

let rec resettheory () =
  writef "RESETTHEORY\n" [];
  menus := [];
  menusVisible := false

let openproof name number =
  writef "OPENPROOF % %\n" [Str name; Int number]

let closeproof number report = writef "CLOSEPROOF % %\n" [Int number; Bool report]

(* close several proofs, quietly. Reply will be this same list, plus a focus (eventually) *)
let force_close ns = 
  writef "FORCECLOSE\n" [];
  List.iter (fun n -> writef "FORCECLOSEONE %\n" [Int n]) ns;
  writef "FORCECLOSEEND\n" []

(* this bit has to match JapeMenu.java, which says PROOFWINDOW_BAR=1 *)

let barKinds_of_bool proofsonly = if proofsonly then 1 else -1

let  newmenu proofsonly name =
  if existsmenu name then ()
  else writef "NEWMENU % %\n" [Str name; Int (barKinds_of_bool proofsonly)]

let rec menuentry menu proofsonly label keyopt entry =
  writef "MENUITEM % % % % %\n"
    [Str menu; Str label; Str (match keyopt with Some s -> s | None -> " "); Str entry;
     Int (barKinds_of_bool proofsonly)]

let rec menucheckbox menu proofsonly label cmd =
  writef "MENUCHECKBOX % % % %\n" [Str menu; Str label; Str cmd; Int (barKinds_of_bool proofsonly)]

let rec menuradiobutton menu proofsonly lcs =
  writef "MENURADIOBUTTON\n" [];
  List.iter
    (fun (label, cmd) ->
       writef "MENURADIOBUTTONPART % %\n" [Str label; Str cmd])
    lcs;
  writef "MENURADIOBUTTONEND % %\n" [Str menu; Int (barKinds_of_bool proofsonly)]

let rec menuseparator menu proofsonly =
  writef "MENUSEP % %\n" [Str menu; Int (barKinds_of_bool proofsonly)]

let rec enablemenuitem proofsonly menu label state =
  writef "ENABLEMENUITEM % % % %\n" [Str menu; Str label; Int (barKinds_of_bool proofsonly); Bool state]

let rec tickmenuitem proofsonly menu label tick =
  writef "TICKMENUITEM % % % %\n" [Str menu; Str label; Int (barKinds_of_bool proofsonly); Bool tick]

let rec makemenusVisible () =
  (* if !menusVisible then () else *) writef "MAKEMENUSVISIBLE\n" []

(*  mapmenus true  is called when menu construction is finished *)
(*  mapmenus false is called as menu construction starts *)
let rec mapmenus =
  function
    true -> makemenusVisible ()
  | false -> ()

(* *************************************** panels *************************************** *)

open Panelkind

let rec newpanel name panelkind =
  let kind =
    match panelkind with
      TacticPanelkind     -> "0"
    | ConjecturePanelkind -> "1"
    | GivenPanelkind      -> "2"
  in
  writef "NEWPANEL % %\n" [Str name; Str kind]

let rec panelentry name label entry =
  writef "PANELENTRY % % %\n" [Str name; Str label; Str entry]

let rec panelbutton name label cmd =
  writef "PANELBUTTON % %\n" [Str name; Str label]; 
  List.iter (fun c -> writef "PANELBUTTONINSERT % %\n"
                (match c with
                   StringInsert s -> [Int 0; Str s]
                 | LabelInsert    -> [Int 1; Str " "]
                 | CommandInsert  -> [Int 2; Str " "]
                )
            ) cmd;
  writef "PANELBUTTONEND\n" []

(*
    let rec panelcheckbox name label prefix =
      writef "PANELCHECKBOX % % % \n" [Str name; Str label; Str prefix]
    
    let rec panelradiobutton name labelcomlist =
      writef "PANELRADIOBUTTON\n" [];
      List.iter
        (fun (l, c) ->
           writef "PANELRADIOBUTTONPART % % %\n" [Str name; Str l; Str c])
        labelcomlist;
      writef "PANELRADIOBUTTONEND %\n" [Str name]
    
    let rec setpanelbutton name label value =
      writef "SETPANELBUTTON % % %\n" [Str name; Str label; Bool value]
 *)

let rec selectpanelentry name label =
  writef "SELECTPANELENTRY % %\n" [Str name; Str label]

let rec markpanelentry name cmd (proved, disproved) =
  writef "MARKPANELENTRY % % % %\n"
    [Str name; Str cmd; Bool proved; Bool disproved]

let rec greyen posn =
  let (x, y) = explodePos posn in writef "GREYEN % %\n" [Int x; Int y]

let rec blacken posn =
  let (x, y) = explodePos posn in writef "BLACKEN % %\n" [Int x; Int y]

let rec highlight posn classopt =
  let (x, y) = explodePos posn in
  match classopt with
    None   -> writef "UNHIGHLIGHT % %\n" [Int x; Int y]
  | Some c -> writef "HIGHLIGHT % % %\n" [Int x; Int y; Int (int_of_displayclass c)]

let rec readHighlight class__ =
  match intlist_of_ask "READHIGHLIGHT %\n" [Int class__] with
    [x; y] -> Some (pos x y)
  | _ -> None

let forceAllDisproofSelections (sels, textsels) = 
  List.iter (fun p -> let (x,y) = explodePos p in 
                      writef "DISPROOFSELECT % %\n" [Int x; Int y]) sels;
  List.iter (fun (p, ss) -> let (x,y) = explodePos p in
                            writef "DISPROOFTEXTSELPOS % %\n" [Int x; Int y];
                            List.iter (fun s -> writef "DISPROOFTEXTSEL %\n" [Str s]) ss;
                            writef "DISPROOFTEXTSELDONE\n" []) textsels
                            
let rec clearProvisoView () = writef "CLEARPROVISOVIEW\n" []

let displayGivens givens =
  out "CLEARGIVENS\n";
  List.iter (fun (n, g) -> writef "GIVEN % %\n" [Int n; Str g]) givens;
  out "SETGIVENS\n"

let displayProvisos ps =
  clearProvisoView ();
  List.iter (fun s -> writef "SHOWPROVISOLINE %\n" [Str s]) ps

let rec showfile filename = writef "SHOWFILE %\n" [Str filename]

let rec makeChoice heading =
  match intlist_of_ask "MAKECHOICE %\n" [Str heading] with
    [0] -> None
  | [n] -> Some (n - 1)
  | _   -> None

let rec clearChoices () = writef "CLEARCHOICES\n" []

let rec setChoice (show, reply) =
  writef "SETCHOICE % %\n" [Str show; Str reply]

(* let rec setChoiceLine () = writef "SETCHOICELINE\n" [] *)

let rec setChoices ((caption : string), (c : string list list)) =
  let rec cs__ =
    function
      n, [] -> ()
    | n, c :: cs -> ch__ (n, c); cs__ (n + 1, cs)
  and ch__ =
    function
      n, ch :: chs ->
        let ns = string_of_int (n : int) in
        setChoice (ch, ns);
        List.iter (fun l -> setChoice (l, "")) chs
    | n, [] -> ()
  in
  clearChoices (); cs__ (1, c)

let rec askChoice (query, menu) =
  setChoices (query, menu); makeChoice query

let rec quit () = writef "QUIT\n" []

let rec dontquit () = writef "DONTQUIT\n" []

let toplevelfiletype = 0 (* .jt *)
let theoryfiletype = 1 (* .j  *)
let prooffiletype = 2 (* .jp *)
let dbugfiletype = 3 (* whatever you like *)

let rec filetypeToString =
  function
    0 -> "jt"
  | 1 -> "j"
  | 2 -> "jp"
  | 3 -> "jdb"
  | n -> "jxx"

let rec readFileName message filetype =
  writef "READFILENAME % %\n"
    [Str message; Str (filetypeToString filetype)];
  match readline "READFILENAME" with
    "" -> None
  | s -> Some s

let rec writeFileName message filetype =
  writef "WRITEFILENAME % %\n"
    [Str message; Str (filetypeToString filetype)];
  match readline "WRITEFILENAME" with
    "" -> None
  | s -> Some s

let rec setmenuequiv (menu, entry, command) = ()

let positioned_textsels id =
  let l : (pos * string list) list ref = ref [] in
  while
    let line = readline id in
    match strings_of_reply line with
      x :: y :: ss -> l := (pos (atoi x) (atoi y), ss) :: !l; true
    | _            -> (* consolereport ["textselections terminate with "; Stringfuns.enQuote line]; *)
                      false
  do () done;
  (* consolereport ["positioned_textsels \""; id; "\" => "; 
           bracketed_string_of_list (Stringfuns.string_of_pair string_of_pos 
                                (bracketed_string_of_list Stringfuns.enQuote ",") ",") "; " !l]; *)
  !l
  
let rec getProofTextSelections () =
  writef "GETPROOFTEXTSELECTIONS\n" [];
  positioned_textsels "GETPROOFTEXTSELECTIONS"

let rec getProofSelections () =
  let l : (pos * displayclass) list ref = ref [] in
  writef "GETPROOFSELECTIONS\n" [];
  while
    let line = readline "GETPROOFSELECTIONS" in
    match ints_of_reply line with
      [x; y; classn] -> l := (pos x y, displayclass_of_int classn) :: !l; true
    | _ -> (* consolereport ["getFormulaSelection terminates on "; Stringfuns.enQuote line]; *) 
           false
  do () done;
  (* consolereport ["getFormulaSelection => "; 
      bracketed_string_of_list (Stringfuns.string_of_pair string_of_pos string_of_displayclass ",") "; " !l]; *)
  !l

let rec nontrivial_selections line =
  match strings_of_reply line with
    [""] -> []
  | xs   -> xs
    
let rec getDisproofSelections () =
  let l : pos list ref = ref [] in
  writef "GETDISPROOFSELECTIONS\n" [];
  while
    let line = readline "GETDISPROOFSELECTIONS" in
    match ints_of_reply line with
      [x; y] -> l := pos x y :: !l; true
    | _ -> (* consolereport ["getDisproofSelections terminates on "; Stringfuns.enQuote line]; *) 
           false
  do () done;
  (* consolereport ["getDisproofSelections => "; 
      bracketed_string_of_list string_of_pos "; " !l]; *)
  !l

let rec getDisproofTextSelections () =
  writef "GETDISPROOFTEXTSELECTIONS\n" [];
  positioned_textsels "GETDISPROOFTEXTSELECTIONS"

let rec getGivenSelection () =
  writef "GETGIVENTEXTSELECTIONS\n" [];
  nontrivial_selections (readline "GETGIVENTEXTSELECTIONS")

let rec getAllProofSelections () =
  (* unit -> 
        list of all proof selections, 
        list of all proof text selections, 
        the givens text selection 
   *)
  let prooftextsels = getProofTextSelections () in
  let proofsels = getProofSelections () in
  let givensel = getGivenSelection () in 
  proofsels, prooftextsels, givensel

let rec getAllDisproofSelections () =
  (* unit -> 
        list of disproof selections,
        list of disproof text selections 
   *)
  let disprooftextsels = getDisproofTextSelections () in
  let disproofsels = getDisproofSelections () in
  disproofsels, disprooftextsels

let rec draginfo s2pos t2pos els2els =
  let el2int m =
    Mappingfuns.mkmap ((fun (i,e) -> e,i) <* Listfuns.numbered (fst <* m))
  in
  let posof map e =
    match (Mappingfuns.(<@>) map e) with
      Some v -> v
    | None   -> raise 
                  (Catastrophe_ 
                     ["Japeserver.draginfo no mapping for "; string_of_element e; " in ";
                      Mappingfuns.string_of_mapping string_of_element string_of_int map
                     ])
  in
  let writelist sep0 sep wf ns =
    writef "%" [_Int (List.length ns)]; writef sep0 [];
    List.iter (fun n -> wf n; writef sep []) ns
  in
  let writeblock label =
    writef label []; writef " " [];
    writelist "\n" "\n"
  in
  let writeposmap (e, ps) =
    writelist " " " " (fun p -> let (x, y) = explodePos p in writef "% % " [_Int x; _Int y]) ps
  in
  writeblock "DRAGSOURCES" writeposmap s2pos;
  writeblock "DRAGTARGETS" writeposmap t2pos;
  let s2int = el2int s2pos in
  let t2int = el2int t2pos in
  let writedragmap (ss, ts) =
       let wf n = writef "% " [_Int n]
       in
       writelist " " " " wf (posof s2int <* ss); writef "\n" [];
       writelist " " " " wf (posof t2int <* ts) 
  in
  writeblock "DRAGMAP" writedragmap els2els

(* things added for version 5.0 *)

let rec setseqbox size =
  let (w, a, d) = explodeTextSize size in
  writef "SEQBOX % % %\n" (List.map _Int [w; a; d])

let rec emphasise pos b =
  let (x, y) = explodePos pos in
  writef "EMPHASISE % % %\n" [Int x; Int y; Bool b]

let rec settiles ts =
  writef "TILESSTART\n" [];
  List.iter (fun t -> writef "TILE %\n" [Str t]) ts;
  writef "TILESEND\n" []

let rec setworlds selected worlds =
  writef "WORLDSSTART\n" [];
  List.iter (fun ((cx, cy), emphasis, labels, children) ->
               writef "WORLD % % %\n" [Int cx; Int cy; Bool emphasis];
               List.iter
                 (fun (lemph,label) -> 
                    writef "WORLDLABEL % % % %\n" [Int cx; Int cy; Bool lemph; Str label])
                 labels;
               List.iter
                 (fun (chx, chy) -> writef "WORLDCHILD % % % %\n" [Int cx; Int cy; Int chx; Int chy])
                 children)
             worlds;
  List.iter (fun (sx, sy) -> writef "WORLDSELECT % %\n" [Int sx; Int sy]) selected;

exception GetPaneGeometry_ of string

let rec getPaneGeometry pane = 
    match intlist_of_ask "PANEGEOMETRY %\n" [Int (int_of_pane pane)] with
      [x; y; w; h] -> box (pos x y) (size w h)
    | _ -> raise (GetPaneGeometry_ (string_of_pane pane))

let rec clearPane pane = writef "CLEARPANE %\n" [Int (int_of_pane pane)]

type displaystyle = TreeStyle | BoxStyle

let int_of_displaystyle d =
  match d with
    BoxStyle  -> 0
  | TreeStyle -> 1

let rec setproofparams displaystyle linethicknessval =
  linethickness := linethicknessval;
  writef "SETPROOFPARAMS % %\n" [Int (int_of_displaystyle displaystyle); Int linethicknessval]

let askUnify s =
  writef "ASKUNIFY %\n" [Str s];
  match readline "ASKUNIFY" with
    "" -> None
  | s  -> Some s

let askLemma druleString thmString panels provisos =
  writef "ASKLEMMA % % % %\n" [Str druleString; Str thmString; 
                               Int (List.length panels); Int (List.length provisos)];
  List.iter (fun s -> out s; out "\n") panels;
  List.iter (fun s -> out s; out "\n") provisos;
  match strings_of_reply (readline "ASKLEMMA") with
    b :: lemma :: panel :: provisos  -> Some (atob b, 
                                              (if lemma="" then None else Some lemma), 
                                              panel, 
                                              (if provisos=[""] then [] else provisos))
  | _                                -> None
  
(********************************* export ***************************************)

let getfontname = getfontname <.> int_of_displayfont

let resetfontnames () = commentSet := false; resetfontnames() (* initialize cache *)  






























