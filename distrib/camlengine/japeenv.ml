(* $Id$ *)

(* Variables now have names.  Their values are still strings. *)

module type Japeenv =
  sig
    type term and japevar and japeenv
    type name
    exception OutOfRange_ of string exception NotJapeVar_ exception ReadOnly_
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
  end
(* $Id$ *)

module
  Japeenv
  (AAA :
    sig
      module mappingfuns : Mappingfuns
      type term and name
      val atoi : string -> int
      val bracketedliststring : ('a -> string) -> string -> 'a list -> string
      val enQuote : string -> string
      val liststring2 :
        ('a -> string) -> string -> string -> 'a list -> string
      val member : 'a * 'a list -> bool
      val namestring : name -> string
      val string2term : string -> term
      val termstring : term -> string
      exception AtoI_
      exception Catastrophe_ of string list
      exception ParseError_ of string list
    end)
  :
  Japeenv =
  struct
    open AAA
    open mappingfuns
    type name = name
    
    
    
    
    exception OutOfRange_ of string exception NotJapeVar_ exception ReadOnly_
    (* Because of problems with changing syntax, which means that identifiers
     * don't always have the same class, japevars actually work with strings. 
     * This means a certain amount of translation, but what the hell.
     *
     * For similar reasons, japeenvs are name |-> term, not term |-> term.
     *
     * And don't, please, put anything but japevars in the top-level environment.
     *)
     
    type japevar =
        Japevar of
          < vals : string list option; init : string; set : string -> unit;
            get : unit -> string >
      | GuardedJapevar of < guard : unit -> bool; var : japevar >
    let rec guardedjapevar g v =
      GuardedJapevar
        (let module M =
           struct
             class a =
               object
                 val guard = g
                 val var = v
                 method guard = guard
                 method var = var
               end
           end
         in
         new M.a)
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
        Japevar
          (let module M =
             struct
               class a =
                 object
                   val vals = valsopt
                   val init = v
                   val set = set
                   val get = get
                   method vals = vals
                   method init = init
                   method set = set
                   method get = get
                 end
             end
           in
           new M.a)
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
      let i2s : int -> string = makestring in
      basejapevar None (i2s v)
        ((fun ooo -> set (s2i ooo)), (fun ooo -> i2s (get ooo)))
    let rec intjaperefvar v r =
      intjapevar v ((fun i -> r := i), (fun () -> !r))
    let on = "true"
    let off = "false"
    let rec booljapevar v (set, get) =
      let rec s2b t = t = on in
      let rec b2s v = if v then on else off in
      basejapevar (Some [on; off]) (b2s v)
        ((fun ooo -> set (s2b ooo)), (fun ooo -> b2s (get ooo)))
    let rec booljaperefvar v r =
      booljapevar v ((fun b -> r := b), (fun () -> !r))
    type envval = Envterm of term | Envvar of japevar
    let rec at (env, name) =
      match mappingfuns.at (env, name) with
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
      match mappingfuns.at (env, name) with
        Some (Envvar v) -> setjapevar (v, termstring value)
      | _ -> raise NotJapeVar_
    let rec checkrange env name settings =
      match mappingfuns.at (env, name) with
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
    let rec ( |-> ) (t, t') = mappingfuns.( |-> ) (t, Envterm t')
    
    let rec ( ||-> ) (t, var) = mappingfuns.( |-> ) (t, Envvar var)
    type japeenv = (name, envval) mapping
    type term = term
  end
