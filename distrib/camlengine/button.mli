(* $Id$ *)

type name = Name.name

(* things we can enable/disable from the interface: often they are entries in menus, but 
 * they can be real buttons, or whatever.  It's up to button.sml to translate.
 *)
type button = UndoProofbutton
            | RedoProofbutton
            | UndoDisproofbutton
            | RedoDisproofbutton
            | Finishedbutton
            | Resetbutton
            | Savebutton
            | SaveAsbutton
            | Disprovebutton

val enable : button * bool -> unit
val reloadmenusandpanels : (name -> bool option) -> string list -> unit
val markproof : bool -> string -> unit (* now marks proofs and disproofs *)
val initButtons : unit -> unit
val initFonts : unit -> unit
val getfontstuff : unit -> string option
val setfontstuff : string -> unit
val resetfontstuff : unit -> unit
