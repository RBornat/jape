(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the jape proof engine, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*)

open Mappingfuns
open Sml

let atoi = Miscellaneous.atoi
let bracketed_string_of_list = Listfuns.bracketed_string_of_list
let consolereport = Miscellaneous.consolereport
let enQuote = Stringfuns.enQuote
let idf = fun x -> x
let member = Listfuns.member
let sentencestring_of_list = Listfuns.sentencestring_of_list
let string_of_name = Name.string_of_name
let string_of_term = Termstring.string_of_term
let term_of_string = Termparse.asTactic Termparse.term_of_string

exception AtoI_ = Miscellaneous.AtoI_
exception Catastrophe_ = Miscellaneous.Catastrophe_
exception ParseError_ = Miscellaneous.ParseError_

exception OutOfRange_ of string 
exception NotJapeVar_ 
exception ReadOnly_

(* Because of problems with changing syntax, which means that identifiers
 * don't always have the same class, japevars actually work with strings. 
 * This means a certain amount of translation, but what the hell.
 *
 * For similar reasons, japeenvs are name |-> term, not term |-> term.
 *
 * And don't, please, put anything but japevars in the top-level environment.
 *)
 
type japevarrrec =
      { vals : string list option; init : string; set : string -> unit;
        get : unit -> string }
 and guardedjapevarrec = { guard : unit -> bool; var : japevar }
 and japevar = 
    Japevar of japevarrrec
  | GuardedJapevar of guardedjapevarrec

let string_of_japevarrec { vals = vals; init = init } =
  "{vals=" ^ Optionfuns.string_of_option (bracketed_string_of_list (fun s->s) ";") vals ^ 
  "; init=" ^ enQuote init ^
  "; set=...; get=...}"
  
let rec string_of_japevar v =
  match v with 
    Japevar r                -> "Japevar " ^ string_of_japevarrec r
  | GuardedJapevar {var = v} -> "GuardedJapevar {guard=...; var=" ^ string_of_japevar v ^ "}"

let rec guardedjapevar g v =
  GuardedJapevar {guard = g; var = v}

let bad vals = "one of " ^ sentencestring_of_list idf ", " " and " vals

let rec getjapevar =
  function
    Japevar {vals = vals; get = get} ->
      let v = get () in
      begin match vals with
        Some vals ->
          if member (v, vals) then v else raise (OutOfRange_ (bad vals))
      | None -> v
      end
  | GuardedJapevar {var = var} -> getjapevar var

let rec setjapevar =
  function
    Japevar {vals = vals; set = set}, v ->
      begin match vals with
        Some vals ->
          if member (v, vals) then set v
          else raise (OutOfRange_ (bad vals))
      | None -> set v
      end
  | GuardedJapevar {guard = guard; var = var}, v ->
      if guard () then setjapevar (var, v) else 
      try if getjapevar var=v then () else raise ReadOnly_ with _ -> raise ReadOnly_

let rec resetvar =
  function
    Japevar {init = init; set = set} as var -> setjapevar (var, init)
  | GuardedJapevar {guard = guard; var = var} ->
      if guard () then resetvar var else raise ReadOnly_

let rec japevar_range =
  function
    Japevar {vals = vals} ->
      begin match vals with
        Some vals -> vals
      | None -> []
      end
  | GuardedJapevar {guard = guard; var = var} ->
      if guard () then japevar_range var else []
let rec basejapevar valsopt v (set, get) =
  let var =
    Japevar{vals = valsopt; init = v; set = set; get = get}
  in
  setjapevar (var, v); var
let rec basejaperefvar valsopt v r =
  basejapevar valsopt v ((fun v -> r := v), (fun () -> !r))
let rec japevar vals = basejapevar (Some vals)
let rec japerefvar vals = basejaperefvar (Some vals)
let unboundedjapevar = basejapevar None
let unboundedjaperefvar = basejaperefvar None
let rec intjapevar v (set, get) =
  let rec i_of_s t =
    try atoi t with
      AtoI_ -> raise (OutOfRange_ "an integer")
  in
  let s_of_i : int -> string = string_of_int in
  basejapevar None (s_of_i v)
    ((set <.> i_of_s), (s_of_i <.> get))

let intjaperefvar v r =
  intjapevar v ((fun i -> r := i), (fun () -> !r))

let on = "true"
let off = "false"

let booljapevar v (set, get) =
  let rec b_of_s t = t = on in
  let rec s_of_b v = if v then on else off in
  basejapevar (Some [on; off]) (s_of_b v)
    ((set <.> b_of_s), (s_of_b <.> get))

let booljaperefvar v r =
  booljapevar v ((fun b -> r := b), (fun () -> !r))

type envval = Envterm of Termtype.term | Envvar of japevar

let string_of_envval v =
  match v with 
    Envterm t -> string_of_term t
  | Envvar  v -> string_of_japevar v

let rec (<@>) env name =
  match Mappingfuns.(<@>) env name with
    Some (Envterm t) -> Some t
  | Some (Envvar v) ->
      (try Some (term_of_string (getjapevar v)) 
       with ParseError_ rs ->
          raise (Catastrophe_ (["japeenv can't parse get()=\""; getjapevar v; "\" -- "] @ rs)))
  | None -> None

let stringset env name s=
  match Mappingfuns.(<@>) env name with
    Some (Envvar v) -> setjapevar (v, s)
  | _               -> raise NotJapeVar_

let termset env name t = stringset env name (string_of_term t)

let rec checkrange env name settings =
  match Mappingfuns.(<@>) env name with
    Some (Envvar v) ->
      let asyouwere =
        try Some (getjapevar v) with
          exn -> None
      in
      let rec reset () =
        match asyouwere with
          Some value -> setjapevar (v, value)
        | None -> ()
      in
      begin try List.iter (fun s -> setjapevar (v, s)) settings; reset () with
        exn -> reset (); raise exn
      end
  | _ -> raise NotJapeVar_

let (++) = (++)
let empty = empty
let ( |-> ) t t' = Mappingfuns.(|->) t (Envterm t')
let ( ||-> ) t var = Mappingfuns.(|->) t (Envvar var)

type japeenv = (Name.name, envval) mapping

let string_of_japeenv = Mappingfuns.string_of_mapping Name.string_of_name string_of_envval
