(* $Id$ *)

(* Variables now have names.  Their values are still strings. *)

module type T =
  sig
    type term and japevar and japeenv and name
    
    val japevar :
      string list -> string -> (string -> unit) * (unit -> string) -> japevar
    val japerefvar : string list -> string -> string ref -> japevar
    val unboundedjapevar :
      string -> (string -> unit) * (unit -> string) -> japevar
    val unboundedjaperefvar : string -> string ref -> japevar
    val booljapevar : bool -> (bool -> unit) * (unit -> bool) -> japevar
    val booljaperefvar : bool -> bool ref -> japevar
    val intjapevar : int -> (int -> unit) * (unit -> int) -> japevar
    val intjaperefvar : int -> int ref -> japevar
    val resetvar : japevar -> unit
    val guardedjapevar : (unit -> bool) -> japevar -> japevar
    (* guardedjapevars can't be set unless the guard says true.
     * example: vars which can be set until there is something in the thing store 
     *)
  
    val empty : japeenv
    val ( ++ ) : japeenv * japeenv -> japeenv
    val ( |-> ) : name * term -> japeenv
    val ( ||-> ) : name * japevar -> japeenv
    val at : japeenv * name -> term option
    val set : japeenv * name * term -> unit
    val checkrange : japeenv -> name -> string list -> unit

    exception OutOfRange_ of string 
    exception NotJapeVar_ 
    exception ReadOnly_
  end

module M : T with type term = Term.Funs.term
			  and type name = Name.M.name
=
  struct
    open Mappingfuns.M
    open Sml.M
    
    type term = Term.Funs.term
    and name = Name.M.name
    
	let atoi = Miscellaneous.M.atoi
	let bracketedliststring = Listfuns.M.bracketedliststring
	let consolereport = Miscellaneous.M.consolereport
	let enQuote = Stringfuns.M.enQuote
	let liststring2 = Listfuns.M.liststring2
	let member = Listfuns.M.member
	let namestring = Name.M.namestring
	let string2term = Termparse.M.asTactic Termparse.M.string2term
	let termstring = Term.Termstring.termstring
	
	exception AtoI_ = Miscellaneous.M.AtoI_
	exception Catastrophe_ = Miscellaneous.M.Catastrophe_
	exception ParseError_ = Miscellaneous.M.ParseError_
    
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
     and japevar = Japevar of japevarrrec
      | GuardedJapevar of guardedjapevarrec
    
    let rec guardedjapevar g v =
      GuardedJapevar {guard = g; var = v}
    let rec bad vals = "one of " ^ liststring2 (fun s -> s) ", " " and " vals
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
          if guard () then setjapevar (var, v) else raise ReadOnly_
    let rec resetvar =
      function
        Japevar {init = init; set = set} as var -> setjapevar (var, init)
      | GuardedJapevar {guard = guard; var = var} ->
          if guard () then resetvar var else raise ReadOnly_
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
      let rec s2i t =
        try atoi t with
          AtoI_ -> raise (OutOfRange_ "an integer")
      in
      let i2s : int -> string = string_of_int in
      basejapevar None (i2s v)
        ((set <*> s2i), (i2s <*> get))
    let rec intjaperefvar v r =
      intjapevar v ((fun i -> r := i), (fun () -> !r))
    let on = "true"
    let off = "false"
    let rec booljapevar v (set, get) =
      let rec s2b t = t = on in
      let rec b2s v = if v then on else off in
      basejapevar (Some [on; off]) (b2s v)
        ((set <*> s2b), (b2s <*> get))
    let rec booljaperefvar v r =
      booljapevar v ((fun b -> r := b), (fun () -> !r))
    type envval = Envterm of term | Envvar of japevar
    let rec at (env, name) =
      match Mappingfuns.M.at (env, name) with
        Some (Envterm t) -> Some t
      | Some (Envvar v) ->
          begin try Some (string2term (getjapevar v)) with
            ParseError_ rs ->
              raise
                (Catastrophe_
                   (["japeenv can't parse get()=\""; getjapevar v; "\" -- "] @
                      rs))
          end
      | None -> None
    let rec set (env, name, value) =
      match Mappingfuns.M.at (env, name) with
        Some (Envvar v) -> setjapevar (v, termstring value)
      | _ -> raise NotJapeVar_
    let rec checkrange env name settings =
      match Mappingfuns.M.at (env, name) with
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
    let ( |-> ) (t, t') = Mappingfuns.M.( |-> ) (t, Envterm t')
    let ( ||-> ) (t, var) = Mappingfuns.M.( |-> ) (t, Envvar var)
    
    type japeenv = (name, envval) mapping
  end
