(* $Id$ *)

module type T =
  sig
    type cxt and term
    val _Oracle :
      string -> cxt -> term -> term -> string -> string list -> cxt option
    val resetoracle : unit -> unit
  end
(* $Id$ *)

module M :T with type term = Term.Funs.term
             and type cxt = Context.Cxt.cxt
=
  struct
    open Context.Cxt
    open Env.M
    open Mappingfuns.M
    open Sml.M
    open Streamio.M
    open Stringfuns.M

	type term = Term.Funs.term
	 and cxt = Context.Cxt.cxt
	
	let atoi         = Miscellaneous.M.atoi
	let explodeCollection = Term.Funs.explodeCollection
	let isemptycollection = Term.Funs.isemptycollection
	let getfontstuff = Button.M.getfontstuff
	let setReason    = Reason.M.setReason
	let termstring   = Term.Termstring.termstring
	let termOrCollectionstring = Term.Termstring.termOrCollectionstring

    let rec words s =
      let rec wd a1 a2 =
        match a1, a2 with
          res, [] -> List.rev res, []
        | res, [c] -> if c <= " " then List.rev res, [] else List.rev (c :: res), []
        | res, "\"" :: cs -> qd res cs
        | res, c :: cs ->
            if c <= " " then List.rev res, unspace cs else wd (c :: res) cs
      and qd a1 a2 =
        match a1, a2 with
          res, "\"" :: cs -> List.rev res, cs
        | res, "\\" :: c :: cs -> qd (c :: res) cs
        | res, c :: cs -> qd (c :: res) cs
        | res, [] -> List.rev res, []
      and unspace =
        function
          [] -> []
        | c :: cs as this -> if c <= " " then unspace cs else this
      and wds =
        function
          [] -> []
        | cs ->
            let (word, rest) = wd [] (unspace cs) in
            if null word then wds rest else implode word :: wds rest
      in
      wds (explode s)
    type _Oracle =
      < translatehyps : term -> string; translateconcs : term -> string;
        turnstile : term -> term -> string; from_or : unit -> string;
        to_or : string -> unit; kill_or : unit -> unit >
    let mappings = ref (empty : (string, _Oracle) mapping)
    let rec _NoneBecause ss = setReason ss; None
    (* Should be in a module of its own *)
    let rec readmapping filename =
      let oracledir = getenv (getenv "." "JAPEHOME") "JAPEORACLE" in
      let _ =
        output_string
          !errstream ("[OPENING " ^ oracledir ^ "/" ^ filename ^ "]\n")
      in
      try
        let oldinput =
          _SwapStream instream (open_in ((oracledir ^ "/") ^ filename))
        in
        let mapping = Hashtbl.create 31 in
        let table =
          Array.init 256 
            (fun n -> if n < 128 then String.make 1 (Char.chr n) else "\\" ^ string_of_int n)
        in
        let mapped = ref false in
        while not (eof ()) do
          let line = words (readline ()) in
          match line with
            [] -> ()
          | "ASCII" :: wd :: (wd2 :: _ as wds) ->
              let n = try Pervasives.int_of_string wd with Failure _ -> ordof wd 0 in
              mapped := true;
              if 1 <= n && n <= 255 then Array.set table n (respace wds)
          | wd :: wds ->
              match String.get wd 0 with
                '#' -> ()
              | _ -> Hashtbl.add mapping wd (respace wds)
        done;
        output_string
          !errstream ("[CLOSING " ^ oracledir ^ "/" ^ filename ^ "]\n");
        close_in !instream;
        let _ = _SwapStream instream oldinput in
        Some (mapping, table, !mapped)
      with
        (* IO.Io *) _ ->
          _NoneBecause
            ["Cannot read oracle description from ";
             (oracledir ^ "/") ^ filename]
    let emptyCollection = isemptycollection
    let rec hypconcstring punct mapped table term =
      let rec translatewith table string =
        implode (List.map (fun c -> Array.get table (ord c)) (explode string))
      in
      let rec docollection term =
        ("(" ^ termOrCollectionstring ((")" ^ punct) ^ "(") term) ^ ")"
      in
      match explodeCollection term with
        [] -> ""
      | [_] ->
          (if mapped then fun ooo -> translatewith table (termstring ooo)
           else termstring)
            term
      | _ ->
          (if mapped then fun ooo -> translatewith table (docollection ooo)
           else docollection)
            term
	let rec onInterrupt f g =
	  (* does this catch two ctrl-Cs? RB *)
	  let now = Sys.signal Sys.sigint (Sys.Signal_handle (fun i -> f ())) in
	  g (); Sys.set_signal Sys.sigint now
    
	exception Interrupt
    let rec line_from s =
      let r = ref "" in
      onInterrupt (fun () -> raise Interrupt)
				  (fun () -> try r := input_line s with Interrupt -> r := "INTERRUPTED");
      !r
    
    let rec flush s =
      try Pervasives.flush s with
        _ -> output_string stderr "[Flush]"
    let rec string_to s t =
      try output_string s t with
        _ -> output_string stderr "[String To]"

    (* At this point I would have to understand Bernard's program to translate. RB *)
    
	let rec execute_oracle_in_env (n, a, e) =
	let (process : Unix.proc) = Unix.executeInEnv (n, a, e) in
	let (ii, oo) = Unix.streamsOf process in
	(ii, oo),
	(fun () ->
	   begin try TextIO.closeIn ii with
		 _ -> ()
	   end;
	   begin try TextIO.closeOut oo with
		 _ -> ()
	   end;
	   Unix.reap process;
	   ())

    let rec createoracle oraclename (store, table, mapped) =
      let rec trans d s =
        match store.at (store, s) with
          None -> d
        | Some v -> v
      in
      let rec transhyps =
        fun _HS -> hypconcstring (trans ", " "hypjoin") mapped table _HS
      in
      let rec transconcs =
        fun _CS -> hypconcstring (trans ", " "concjoin") mapped table _CS
      in
      let TS = trans "|-" "turnstile" in
      let rec turnstile =
        fun _HS ->
          fun _CS ->
            if emptyCollection _HS || emptyCollection _CS then "" else TS
      in
      match words (trans "" "program") with
        server :: args ->
          begin try
            let (((iii : in_channel), (ooo : out_channel)), kkk) =
              execute_oracle_in_env (server, args, System.environ ())
            in
            (Some
               {translatehyps = transhyps; translateconcs = transconcs;
                turnstile = turnstile; from_or = (fun () -> flush ooo; line_from iii);
                to_or = string_to ooo;
                kill_or = 
                (fun () ->
                    kkk ();
                    mappings := Mappingfuns.M.( -- ) (!mappings, [oraclename]))} : _Oracle option)
          with
            _ -> _NoneBecause ["_Oracle cannot start "; server]
          end
      | [] ->
          (* Doesn't work under Solaris: stalls in the first open_xx *)
          match words (trans "" "pipes") with
            inpipe :: outpipe :: _ ->
              begin try
                let ooo = open_out outpipe in
                let iii = open_in inpipe in
                (Some
                   (let module M =
                      struct
                        class a =
                          object
                            val translatehyps = transhyps
                            val translateconcs = transconcs
                            val turnstile = turnstile
                            val from_or = fun () -> flush ooo; line_from iii
                            val to_or = string_to ooo
                            val kill_or =
                              fun () ->
                                begin try close_in iii with
                                  _ -> ()
                                end;
                                begin try close_out ooo with
                                  _ -> ()
                                end;
                                mappings :=
                                  Mappingfuns.M.( -- ) (!mappings, [oraclename])
                            method translatehyps = translatehyps
                            method translateconcs = translateconcs
                            method turnstile = turnstile
                            method from_or = from_or
                            method to_or = to_or
                            method kill_or = kill_or
                          end
                      end
                    in
                    new M.a) :
                 _Oracle option)
              with
                _ ->
                  _NoneBecause
                    ["_Oracle couldn't open pipes "; inpipe; " "; outpipe]
              end
          | [_] ->
              _NoneBecause
                ["_Oracle pipes attribute needs in and out parts"]
          | _ ->
              _NoneBecause
                ["_Oracle not specified by program or pipes attributes."]
    let rec getmapping oraclename =
      match Mappingfuns.M.at (!mappings, oraclename) with
        Some m -> Some m
      | None ->
          match readmapping (oraclename ^ ".jo") with
            Some m ->
              begin match createoracle oraclename m with
                Some ( or ) ->
                  mappings :=
                    Mappingfuns.M.( ++ )
                      (!mappings, Mappingfuns.M.( |-> ) (oraclename, ( or )));
                  Some ( or )
              | None -> None
              end
          | None -> None
    let rec resetoracle () =
      Mappingfuns.M.formappingpairs
        ((fun (name, ({kill_or = kill_or} : _Oracle)) -> kill_or ()),
         !mappings)
    let rec _Oracle (turnstile : string) (cxt : Context.Cxt.cxt) =
      fun (_HS : Context.Cxt.term) ->
        fun (_CS : Context.Cxt.term) (oracle : string) (args : string list) ->
          let oracle = unQuote oracle in
          match getmapping oracle with
            (Some
               {translatehyps = translatehyps;
                translateconcs = translateconcs;
                turnstile = turnstile;
                from_or = from_or;
                to_or = to_or;
                kill_or = kill_or} :
             _Oracle option) ->
              let TS = turnstile _HS _CS in
              let _HS = translatehyps _HS in
              let _CS = translateconcs _CS in
              to_or _HS;
              to_or TS;
              to_or _CS;
              to_or "\n";
              begin match from_or () with
                "yes\n" -> Some cxt
              | "no\n" ->
                  _NoneBecause
                    ["_Oracle "; oracle; " couldn't prove it"]
              | "INTERRUPTED" ->
                  kill_or ();
                  _NoneBecause
                    ["_Oracle "; oracle; " was interrupted (killed it)"]
              | "" ->
                  kill_or ();
                  _NoneBecause
                    ["_Oracle "; oracle; " was interrupted (killed it)"]
              | s ->
                  _NoneBecause
                    ["_Oracle "; oracle; " replied \""; s; "\""]
              end
          | None -> None
  end











