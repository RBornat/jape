(* $Id$ *)

open Match
open Optionfuns
open Stringfuns
open Term.Funs
open Term.Termstring

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
