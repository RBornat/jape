(* $Id$ *)

type displayclass = DisplayHyp
                  | DisplayConc
                  | DisplayAmbig
                  | DisplayReason
                  | DisplayPunct

val displayclassstring : displayclass -> string
val displayclass2int : displayclass -> int
val int2displayclass : int -> displayclass

