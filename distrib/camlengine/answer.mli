(* $Id$ *)

type answer = Yes | Maybe | No

val allq : ('a -> answer) -> 'a list -> answer
val andalsoq : answer -> (unit -> answer) -> answer
val andq : answer * answer -> answer
val existsq : ('a -> answer) -> 'a list -> answer
val ifMq : answer -> (unit -> answer) -> (unit -> answer) -> (unit -> answer) -> answer
val ifq : answer -> answer -> answer -> answer
val notq : answer -> answer
val orelseq : answer -> (unit -> answer) -> answer
val orq : answer * answer -> answer
val qDEF : answer -> bool
val qDEFNOT : answer -> bool
val qUNSURE : answer -> bool
val takeNo : answer -> answer
val takeYes : answer -> answer
val unit2Maybe : unit -> answer
val unit2No : unit -> answer
val unit2Yes : unit -> answer

val answerstring : answer -> string
