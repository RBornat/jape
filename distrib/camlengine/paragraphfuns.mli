(* $Id$ *)

open Forcedef
open Japeenv
open Name
open Paragraph 
open Paraparam
open Proofstate
open Proviso
open Runproof
open Sequent.Funs
open Thing

val interpret :
  (string list -> unit) ->
    (string list * string * string * int -> bool) -> paraparam list ->
    proviso list ->
    japeenv * (name * proofstate * (seq * model) option) list *
      (name * (string * bool -> unit)) list ->
    paragraph ->
    japeenv * (name * proofstate * (seq * model) option) list *
      (name * (string * bool -> unit)) list
val interpretParasFrom :
  (string list -> unit) ->
    (string list * string * string * int -> bool) ->
    japeenv * (name * proofstate * (seq * model) option) list *
      (name * (string * bool -> unit)) list ->
    string list ->
    japeenv * (name * proofstate * (seq * model) option) list *
      (name * (string * bool -> unit)) list
val conjecturename : paragraph -> name
