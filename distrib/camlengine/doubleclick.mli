(* $Id$ *)

type seq = Sequent.Funs.seq
and tactic = Tactic.tactic
and term = Sequent.Funs.term

type dclick = DClickHyp | DClickConc

val adddoubleclick : dclick * tactic * seq -> unit
val deldoubleclick : dclick * seq -> unit
val cleardoubleclicks : unit -> unit
val matchdoubleclick : dclick -> seq -> tactic option
