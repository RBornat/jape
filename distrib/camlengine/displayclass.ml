(* $Id$ *)

open Miscellaneous.M

type displayclass = DisplayHyp | DisplayConc | DisplayAmbig | DisplayReason | DisplayPunct

let rec displayclassstring =
  function
    DisplayHyp -> "DisplayHyp"
  | DisplayConc -> "DisplayConc"
  | DisplayAmbig -> "DisplayAmbig"
  | DisplayReason -> "DisplayReason"
  | DisplayPunct -> "DisplayPunct"

(* Useful translation for Japeserver marshalling below.
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
  | n -> raise (Catastrophe_ ["int2displayclass "; string_of_int n])
