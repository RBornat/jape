(* $Id$ *)

module type T =
  sig
    type answer = Yes | Maybe | No
    val answerstring : answer -> string
    val notq : answer -> answer
    val andq : answer * answer -> answer
    val orq : answer * answer -> answer
    val allq : ('a -> answer) -> 'a list -> answer
    val existsq : ('a -> answer) -> 'a list -> answer
    val ifq : answer -> answer -> answer -> answer
    val ifMq :
      answer -> (unit -> answer) -> (unit -> answer) -> (unit -> answer) ->
        answer
    val unit2Yes : unit -> answer
    val unit2No : unit -> answer
    val unit2Maybe : unit -> answer
    val qDEF : answer -> bool
    val qDEFNOT : answer -> bool
    val qUNSURE : answer -> bool
    val orelseq : answer -> (unit -> answer) -> answer
    val andalsoq : answer -> (unit -> answer) -> answer
    val takeYes : answer -> answer
    val takeNo : answer -> answer
  end
(* $Id$ *)

module M : T =
  struct
    type answer = Yes | Maybe | No
    (* Yes > Maybe > No *)
    
    let rec answerstring =
      function
        Yes -> "Yes"
      | No -> "No"
      | Maybe -> "Maybe"
    (* slightly simplified versions of orq, andq, existsq and allq.  RB 5/i/93 *)
    (* orq is max *)
    let rec orq =
      function
        Yes, _ -> Yes
      | No, x -> x
      | Maybe, Yes -> Yes
      | Maybe, _ -> Maybe
    (* andq is min *)
    let rec andq =
      function
        No, _ -> No
      | Yes, x -> x
      | Maybe, No -> No
      | Maybe, _ -> Maybe
    let rec notq =
      function
        Yes -> No
      | No -> Yes
      | Maybe -> Maybe
    let rec ifq a1 a2 a3 =
      match a1, a2, a3 with
        Yes, t, _ -> t
      | No, _, e -> e
      | Maybe, _, _ -> Maybe
    let rec ifMq test yes no maybe =
      match test with
        Yes -> yes ()
      | No -> no ()
      | Maybe -> maybe ()
    let rec unit2Yes () = Yes
    let rec unit2No () = No
    let rec unit2Maybe () = Maybe
    let rec qDEF a =
      match a with
        Yes -> true
      | _ -> false
    let qDEFNOT ooo = qDEF (notq ooo)
    let rec qUNSURE =
      function
        Maybe -> true
      | _ -> false
    let rec orelseq a1 a2 =
      match a1, a2 with
        Yes, _ -> Yes
      | No, b -> b ()
      | Maybe, b -> orq (b (), Maybe)
    let rec andalsoq a1 a2 =
      match a1, a2 with
        Yes, b -> b ()
      | No, _ -> No
      | Maybe, b -> andq (b (), Maybe)
    let rec existsq a1 a2 =
      match a1, a2 with
        f, [] -> No
      | f, x :: xs -> orelseq (f x) (fun _ -> existsq f xs)
    let rec allq a1 a2 =
      match a1, a2 with
        f, [] -> Yes
      | f, x :: xs -> andalsoq (f x) (fun _ -> allq f xs)
    let rec takeYes a = orq (a, Maybe)
    let rec takeNo a = andq (No, Maybe)
  end
