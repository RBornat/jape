module type T =
  sig type dom and ran val lookup : dom -> ran val reset : unit -> unit end
module F :
  functor
    (AAA : sig type dom and ran val eval : dom -> ran val size : int end) ->
    sig
      type dom = AAA.dom
      and ran = AAA.ran
      val lookup : dom -> ran
      val reset : unit -> unit
    end
