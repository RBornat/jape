(* $Id$ *)

module type TreeLayout =
  sig
    type term and ('a, 'b) mapping
    (* everything you can say in a LAYOUT tactic *)
    type treelayout =
        HideRootLayout
      | HideCutLayout
      | CompressedLayout of (term * term option)
      | NamedLayout of (term * term option)
    (* fmt * list of subtrees to show *)

    val treelayoutstring : treelayout -> string
    val smltreelayoutstring : treelayout -> string
    val remaptreelayout : (term, term) mapping -> treelayout -> treelayout
  end
(* $Id$ *)

module
  TreeLayout
  (AAA :
    sig
      type term and ('a, 'b) mapping
      val optionstring : ('a -> string) -> 'a option -> string
      val pairstring :
        ('a -> string) -> ('b -> string) -> string -> 'a * 'b -> string
      val remapterm : (term, term) mapping -> term -> term
      val smltermstring : term -> string
      val termstring : term -> string
      val try__ : ('a -> 'b) -> 'a option -> 'b option
    end)
  :
  TreeLayout =
  struct
    open AAA
    type term = term and ('a, 'b) mapping = ('a, 'b) mapping
    type treelayout =
        HideRootLayout
      | HideCutLayout
      | CompressedLayout of (term * term option)
      | NamedLayout of (term * term option)
    (* fmt * list of subtrees to show *)

    let rec treelayoutstring =
      function
        HideRootLayout -> "HIDEROOT"
      | HideCutLayout -> "HIDECUT"
      | CompressedLayout stuff ->
          begin match tls stuff with
            "\"%s\" ALL" -> "COMPRESS"
          | s -> "COMPRESS " ^ s
          end
      | NamedLayout stuff -> tls stuff
    and tls =
      function
        fmt, Some tns -> ((termstring fmt ^ " (") ^ termstring tns) ^ ")"
      | fmt, None -> termstring fmt ^ " ALL"
    let rec smltreelayoutstring =
      function
        HideRootLayout -> "HideRootLayout"
      | HideCutLayout -> "HideCutLayout"
      | CompressedLayout stuff -> "CompressedLayout" ^ stls stuff
      | NamedLayout stuff -> "NamedLayout" ^ stls stuff
    and stls stuff =
      pairstring smltermstring (optionstring smltermstring) "," stuff
    let rec remaptreelayout a1 a2 =
      match a1, a2 with
        env, HideRootLayout -> HideRootLayout
      | env, HideCutLayout -> HideCutLayout
      | env, CompressedLayout stuff -> CompressedLayout (rmtl env stuff)
      | env, NamedLayout stuff -> NamedLayout (rmtl env stuff)
    and rmtl env (fmt, tns) = remapterm env fmt, try__ (remapterm env) tns
  end
