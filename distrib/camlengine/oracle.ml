(* $Id$ *)

open Context.Cxt
open Env
open Mappingfuns
open Sml
open Stringfuns

let atoi                   = Miscellaneous.atoi
let consolereport          = Miscellaneous.consolereport
let explodeCollection      = Term.Funs.explodeCollection
let isemptycollection      = Term.Funs.isemptycollection
let getfontstuff           = Button.getfontstuff
let setReason              = Reason.setReason
let termstring             = Term.Termstring.termstring
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

type oraclerec =
  { translatehyps : term -> string; translateconcs : term -> string;
    turnstile : term -> term -> string; from_or : unit -> string;
    to_or : string -> unit; kill_or : unit -> unit }

let mappings = ref (empty : (string, oraclerec) mapping)

let rec _NoneBecause ss = setReason ss; None

(* Should be in a module of its own *)
let rec readmapping filename =
  let oracledir = getenv (getenv "." "JAPEHOME") "JAPEORACLE" in
  let _ = consolereport ["[OPENING "; oracledir; "/"; filename; "]\n"] in
  try
    let in_mapping = open_in ((oracledir ^ "/") ^ filename) in
    let mapping = Hashtbl.create 31 in
    let table =
      Array.init 256 
        (fun n -> if n < 128 then String.make 1 (Char.chr n) else "\\" ^ string_of_int n)
    in
    let mapped = ref false in
    (try
	   while true do
		 let line = words (input_line in_mapping) in
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
	   done
	 with End_of_file -> ()
	);
    consolereport ["[CLOSING "; oracledir; "/"; filename; "]\n"];
    close_in in_mapping;
    Some (mapping, table, !mapped)
  with
    exn ->
      _NoneBecause
        ["(exception "; Printexc.to_string exn; ") -- cannot read oracle description from ";
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

exception Interrupt
let rec line_from s =
  let r = ref "" in
  Moresys.onInterrupt (fun _ -> raise Interrupt)
    (fun () -> try r := input_line s with Interrupt -> r := "INTERRUPTED");
  !r

let rec flush s =
  try Pervasives.flush s with
    _ -> output_string stderr "[Flush]"
let rec string_to s t =
  try output_string s t with
    _ -> output_string stderr "[String To]"

let rec createoracle oraclename (store, table, mapped) =
  let trans d s = try Hashtbl.find store s with Not_found -> d in
  let rec transhyps _HS = hypconcstring (trans ", " "hypjoin") mapped table _HS in
  let rec transconcs _CS = hypconcstring (trans ", " "concjoin") mapped table _CS in
  let _TS = trans "|-" "turnstile" in
  let rec turnstile =
    fun _HS ->
      fun _CS ->
        if emptyCollection _HS || emptyCollection _CS then "" else _TS
  in
  match words (trans "" "program") with
    server :: args ->
      begin try
        let (ppp, iii, ooo) = Moresys.execute server args in
        (Some
           {translatehyps = transhyps; translateconcs = transconcs;
            turnstile = turnstile; from_or = (fun () -> flush ooo; line_from iii);
            to_or = string_to ooo;
            kill_or = 
            (fun () ->
                (try close_in iii with _ -> ());
                (try close_out ooo with _ -> ());
                Moresys.reap ppp;
                mappings := Mappingfuns.( -- ) (!mappings, [oraclename]))} : oraclerec option)
      with
        _ -> _NoneBecause ["Oracle cannot start "; server]
      end
  | [] ->
      (* Doesn't work under Solaris: stalls in the first open_xx *)
      match words (trans "" "pipes") with
        inpipe :: outpipe :: _ ->
          begin try
            let ooo = open_out outpipe in
            let iii = open_in inpipe in
            (Some
               {translatehyps = transhyps; 
                translateconcs = transconcs;
                turnstile = turnstile; 
                from_or = (fun () -> flush ooo; line_from iii);
                to_or = string_to ooo;
                kill_or =
				  (fun () ->
					begin try close_in iii with
					  _ -> ()
					end;
					begin try close_out ooo with
					  _ -> ()
					end;
					mappings :=
					  Mappingfuns.( -- ) (!mappings, [oraclename]))} :
             oraclerec option)
          with
            _ ->
              _NoneBecause
                ["Oracle couldn't open pipes "; inpipe; " "; outpipe]
          end
      | [_] ->
          _NoneBecause
            ["Oracle pipes attribute needs in and out parts"]
      | _ ->
          _NoneBecause
            ["Oracle not specified by program or pipes attributes."]

let rec getmapping oraclename =
  match Mappingfuns.at (!mappings, oraclename) with
    Some m -> Some m
  | None ->
      match readmapping (oraclename ^ ".jo") with
        Some m ->
          begin match createoracle oraclename m with
            Some ( or ) ->
              mappings :=
                Mappingfuns.( ++ )
                  (!mappings, Mappingfuns.( |-> ) (oraclename, ( or )));
              Some ( or )
          | None -> None
          end
      | None -> None

let rec resetoracle () =
  Mappingfuns.formappingpairs
    ((fun (name, ({kill_or = kill_or} : oraclerec)) -> kill_or ()),
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
         oraclerec option) ->
          let _TS = turnstile _HS _CS in
          let _HS = translatehyps _HS in
          let _CS = translateconcs _CS in
          to_or _HS;
          to_or _TS;
          to_or _CS;
          to_or "\n";
          begin match from_or () with
            "yes\n" -> Some cxt
          | "no\n" ->
              _NoneBecause
                ["Oracle "; oracle; " couldn't prove it"]
          | "INTERRUPTED" ->
              kill_or ();
              _NoneBecause
                ["Oracle "; oracle; " was interrupted (killed it)"]
          | "" ->
              kill_or ();
              _NoneBecause
                ["Oracle "; oracle; " was interrupted (killed it)"]
          | s ->
              _NoneBecause
                ["Oracle "; oracle; " replied \""; s; "\""]
          end
      | None -> None











