(* $Id$ *)

type displayfont = TermFont | ReasonFont | ProvisoFont
type pane = ProofPane | DisproofPane

val allfonts : displayfont list
val displayfontstring : displayfont -> string
val displayfont2int : displayfont -> int
val int2displayfont : int -> displayfont
val panestring : pane -> string

(* Useful translation for Japeserver marshalling.
 *
 *  ProofPane = 0
 *  DisproofPane = 1
 *
 *)
val pane2int : pane -> int
val int2pane : int -> pane
