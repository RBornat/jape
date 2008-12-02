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

open Moresys

type box = Box.box
and displayclass = Displayclass.displayclass
and element = Termtype.element
and font = Displayfont.displayfont
and pane = Displayfont.pane
and panelkind = Panelkind.panelkind
and panelbuttoninsert = Panelkind.panelbuttoninsert
and pos = Box.pos
and size = Box.size
and textsize = Box.textsize

val idlsignature : string
val getSignature : unit -> string
val sendVersion : string -> unit
val sendOperators : string list -> unit
val setFonts : string -> unit
(* send font-encoding name *)
val fontinfo : font -> int * int * int
(* ascent, descent, leading *)
   
(* By default all drawing takes place in the front window. 
* When fonts change, background windows have to be refreshed.
* The following allow the engine temporarily to redirect
* drawing to a background window.  If canbackgroundfocus is false, 
* it won't do such a thing.
*)
val canbackgroundfocus : bool
val setbackgroundfocus : int -> unit
val setforegroundfocus : unit -> unit

val setFontNames: string list -> unit

val measurestring : font -> string -> int * int * int

val procrustes : int -> string -> font -> string -> string
              (* width  "..."             string    what fits *)

(* drawmeasuredtext takes a selection class (0 is non-selectable),
                          an (offset*font*string) list and
                          a position
*)
val drawmeasuredtext : displayclass ->(pos*font*string) list -> pos -> unit
(* nowadays we can draw in the proof pane or the disproof pane *)
val drawinpane : pane -> unit

(* *************************************** disproof *************************************** *)

val setseqbox : textsize -> unit
val emphasise : pos -> bool -> unit                (* it's a textpos *)

val settiles  : string list -> unit
val setworlds : (int * int) list -> ((int * int) * bool * (bool*string) list * (int * int) list) list 
             (* selections          (coord       forced   labels               children        )      *)
             -> unit

(* *************************************** menus and panels *************************************** *)

val resettheory : unit -> unit
val emptymenusandpanels  : bool -> unit

(* *************************************** menus *************************************** *)

val newmenu         : bool -> string -> unit
                   (* true: proofs only; false: every menubar has this menu *)
val menuentry       : string -> bool -> string -> string option -> string -> unit
                   (* menu      ponly   label     key equiv        cmd *)
val menucheckbox    : string -> bool -> string -> string -> unit
                   (* menu      ponly label     cmd *)
val menuradiobutton : string -> bool -> (string * string) list -> unit
                   (* menu      ponly   (label,   cmd) *)
val menuseparator   : string -> bool -> unit
                   (* menu *)

(* I toyed with the idea that enablemenuitem and tickmenuitem should have an argument which 
   says that the change applies only to the focussed window, in order to cope with the fact 
   that in the GUI there are now multiple menu bars -- one per window.  But in fact the 
   single-menu model used in the engine works well enough: if there is a per-window change
   then the engine applies it each time the window focus changes.
   
   The alternative actions are still implemented in the GUI, if it's necessary to revive
   the focussedonly idea.
   RB 5/xi/02
 *)
val enablemenuitem : bool -> string -> string -> bool -> unit (* applies to entries, checkboxes, radio buttons *)
val tickmenuitem   : bool -> string -> string -> bool -> unit (* only for checkboxes, radio buttons *)

val mapmenus : bool -> unit (* false: menus under construction; true: menus constructed *)
 
(* *************************************** panels *************************************** *)

val newpanel : string -> panelkind -> unit
    (* A ConjecturePanel automatically includes buttons labelled NewÉ, Prove and Show Proof,
       which have to be supplied by the GUI (and the engine supplies a default Apply button 
       if the user defines no buttons at all). 
       Nothing else seems to have any default buttons.  Hmmm.
     *)

val panelentry       : string -> string -> string -> unit
val panelbutton      : string -> string -> panelbuttoninsert list -> unit
(* val panelcheckbox    : string -> string -> string -> unit
   val panelradiobutton : string -> (string * string) list -> unit
   val setpanelbutton   : string -> string -> bool -> unit
 *)
val selectpanelentry : string -> string -> unit
val markpanelentry   : string -> string -> (bool * bool) -> unit

(* this version of jape is started by the GUI *)

val listen       : unit -> string
val terminateGUI : unit -> unit

val drawLine   : pos -> pos -> unit (* don't extend beyond pos2; use linethickness from setproofparams *)
val drawRect   : box -> unit        (* stay inside the box; use linethickness from setproofparams *)

exception DeadGUI_

val openproof  : string -> int -> unit
val closeproof : int -> bool -> unit

val showfile : string -> unit
val echo     : string -> string

val displayProvisos : string list -> unit
val displayGivens   : (int * string) list -> unit (* numbered givens *)
   
val quit     : unit -> unit
val dontquit : unit -> unit

val getPaneGeometry   : pane -> box
val clearPane         : pane -> unit

(* To make findSelection (interaction.sml) work properly, and to get consistent results from tactics that
 * interpret the answers, getAllSelections should return its answers in time-click order.
 *)
val getAllProofSelections :
  unit -> (pos * displayclass) list * (pos * string list) list * string list
       (* proof selections            proof text selections      givens text selections *)

val getAllDisproofSelections: unit ->  pos list * (pos * string list) list
                                 (*  selections   text selections          *)
val forceAllDisproofSelections: pos list * (pos * string list) list -> unit
                                 (*  and the other way around *)
       
val highlight : pos -> displayclass option -> unit (* NOW TAKES TEXTPOS, NOT BOXPOS!!! *)

val greyen  : pos -> unit
val blacken : pos -> unit

val toplevelfiletype : int  (* .jt *)
val theoryfiletype   : int  (* .j  *)
val prooffiletype    : int  (* .jp *)
   
val dbugfiletype : int
(* whatever you like *)
   
(* In the following calls, 
 *   string is a message to put in the dialogue box,
 *   int is a filetype
 * Either argument may be safely ignored.
 *)
val writeFileName : string -> int -> string option
val readFileName  : string -> int -> string option

val resetfontnames    : unit -> unit (* forget font names *)
 
val setinvischars :
   string * string -> string * string -> string * string -> string * string -> unit
(*   onbra  ket         offbra ket         outbra ket        lockbra ket *)

type displaystyle = TreeStyle | BoxStyle
val setproofparams : displaystyle -> int -> unit
               (*    tree/box        linethickness *)

(* interface-specific help information *)
val howToText : string -> string

(* nascent variable mirroring *)
val settextselectionmode : string -> unit
val setmultihypsel       : string -> unit

val setComment : string -> unit
(* this demoted to a thing which sets a comment line *)

val askUnify : string -> string option
val askLemma : string -> string -> string list -> string list 
           (*  drule     thm       panels         provisos    *)
            -> (bool * string option * string * string list) option
             (* isthm  name            panel    provisos      *)

val ask_unpatched : int -> string -> string list -> int -> int 
      (* severity 0/1/2    message   buttons     default   which button was pressed, indexed from 0 *)
			(*                                                   -1 means window closed (equivalent to Cancel) *)

val askCancel_unpatched :
              int -> string -> string list -> int -> int option
(* severity 0/1/2    message   buttons      default  Some button (counting from 0) or None for Cancel 
 *             set default = length buttons to choose Cancel
 *)

val askDangerously_unpatched : string -> string -> string -> int option
														(* message   Do        Don't     Some button (counting from 0) or None for Cancel
														 * special version of ask_cancel, with Do as default, and
														 * the buttons in "Do/Don't" positions -- like this
														 * 
														 * ICON
														 * ICON                    message
														 * ICON 
														 * 
														 * Don't                Cancel  Do
														 *)

val askChoice : string * string list list -> int option

val getfontname : font -> string

val draginfo : (element * pos list) list -> (element * pos list) list 
            -> (element list * element list) list -> unit
