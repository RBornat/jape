(* $Id$ *)

open Term.Funs
open Provisotype

type proviso = Provisotype.proviso
 and visproviso

val catelim_provisostring : proviso -> string list -> string list
val earlierproviso : proviso -> proviso -> bool
val isFreshProviso : proviso -> bool
val maxprovisoresnum : proviso -> int
val mkparentedvisproviso : proviso -> bool * proviso -> visproviso
val mkvisproviso : bool * proviso -> visproviso
val parseProvisos : unit -> proviso list
        (* yes, really a list - it has to translate x,y NOTIN A, B into
         * x NOTIN A AND x NOTIN B AND y NOTIN A AND y NOTIN B; similarly
         * FRESH and all its derivatives
         *)
val provisoVIDs : proviso -> vid list
val provisoactual : visproviso -> proviso
val provisodebug : bool ref
val provisoparent : visproviso -> proviso
val provisoresetactual : visproviso -> proviso -> visproviso
val provisoselfparent : visproviso -> visproviso
val provisostring : proviso -> string
val provisovars : (term -> 'a) -> ('a -> 'a -> 'a) -> proviso -> 'a
val provisovisible : visproviso -> bool
val visprovisostring : visproviso -> string
val visprovisostringall : visproviso -> string
