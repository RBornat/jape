(* $Id$ *)

open Idclass
open Symboltype

val canstartidclass : symbol -> bool
val canstartCollectionidclass : symbol -> bool
val parseidclass : string -> idclass
val unparseidclass : idclass -> string
