(* $Id$ *)

module type Displayclass =
  sig
    type displayclass =
      DisplayHyp | DisplayConc | DisplayAmbig | DisplayReason | DisplayPunct
    val displayclassstring : displayclass -> string
    val displayclass2int : displayclass -> int
    val int2displayclass : int -> displayclass
  end
(* $Id$ *)

module Displayclass (AAA : sig exception Catastrophe_ of string list end) :
  Displayclass =
  struct
    open AAA
    type displayclass =
      DisplayHyp | DisplayConc | DisplayAmbig | DisplayReason | DisplayPunct
    let rec displayclassstring =
      function
        DisplayHyp -> "DisplayHyp"
      | DisplayConc -> "DisplayConc"
      | DisplayAmbig -> "DisplayAmbig"
      | DisplayReason -> "DisplayReason"
      | DisplayPunct -> "DisplayPunct"
    (* Useful translation for japeserver.
     * Current C/Java/Tk interfaces believe in these integers.
     *
     *   DisplayPunct  0
     *   DisplayConc   1
     *   DisplayHyp    2
     *   DisplayReason 3
     *   DisplayAmbig  4
     *
     *)
    
    let rec displayclass2int =
      function
        DisplayPunct -> 0
      | DisplayConc -> 1
      | DisplayHyp -> 2
      | DisplayReason -> 3
      | DisplayAmbig -> 4
    let rec int2displayclass =
      function
        0 -> DisplayPunct
      | 1 -> DisplayConc
      | 2 -> DisplayHyp
      | 3 -> DisplayReason
      | 4 -> DisplayAmbig
      | n -> raise (Catastrophe_ ["int2displayclass "; makestring n])
  end
