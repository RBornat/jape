(* $Id$ *)

module type Displayfont =
  sig
    type displayfont = TermFont | ReasonFont | ProvisoFont
    val allfonts : displayfont list
    val displayfontstring : displayfont -> string
    val displayfont2int : displayfont -> int
    val int2displayfont : int -> displayfont
    type pane = ProofPane | DisproofPane
    val panestring : pane -> string
    val pane2int : pane -> int
    val int2pane : int -> pane
  end
(* $Id$ *)

module Displayfont (AAA : sig exception Catastrophe_ of string list end) :
  Displayfont =
  struct
    open AAA
    type displayfont = TermFont | ReasonFont | ProvisoFont
    let allfonts = [TermFont; ReasonFont; ProvisoFont]
    let rec displayfontstring =
      function
        TermFont -> "TermFont"
      | ReasonFont -> "ReasonFont"
      | ProvisoFont -> "ProvisoFont"
    (* Useful translation for japeserver.
     * Current C/Java/Tk interfaces believe in these integers.
     *
     *  TermFont = 0
     *  ReasonFont = 1
     *  ProvisoFont = 2
     *
     *)
    
    let rec displayfont2int =
      function
        TermFont -> 0
      | ReasonFont -> 1
      | ProvisoFont -> 2
    let rec int2displayfont =
      function
        0 -> TermFont
      | 1 -> ReasonFont
      | 2 -> ProvisoFont
      | n -> raise (Catastrophe_ ["int2displayfont "; string_of_int n])
    type pane = ProofPane | DisproofPane
    let rec panestring =
      function
        ProofPane -> "ProofPane"
      | DisproofPane -> "DisproofPane"
    (* Useful translation for japeserver.
     *
     *  ProofPane = 0
     *  DisproofPane = 1
     *
     *)
    
    let rec pane2int =
      function
        ProofPane -> 0
      | DisproofPane -> 1
    let rec int2pane =
      function
        0 -> ProofPane
      | 1 -> DisproofPane
      | n -> raise (Catastrophe_ ["int2pane "; string_of_int n])
  end
