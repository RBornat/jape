(* $Id$ *)

module type DoubleClick =
  sig
    type seq and tactic and term
    type dclick = DClickHyp | DClickConc
    val adddoubleclick : dclick * tactic * seq -> unit
    val deldoubleclick : dclick * seq -> unit
    val cleardoubleclicks : unit -> unit
    val matchdoubleclick : dclick -> seq -> tactic option
  end
(* $Id$ *)

module
  DoubleClick
  (AAA :
    sig
      type seq and tactic and ('a, 'b) mapping and term
      val ( <| ) : ('a -> bool) * 'a list -> 'a list
      val empty : ('a, 'b) mapping
      val eqseqs : seq * seq -> bool
      val findfirst : ('a -> 'b option) -> 'a list -> 'b option
      val seqmatch :
        seq -> seq -> (term, term) mapping -> (term, term) mapping option
      val remaptactic : (term, term) mapping -> tactic -> tactic
    end)
  :
  DoubleClick =
  struct
    open AAA
    type seq = seq and tactic = tactic and term = term
    
    type dclick = DClickHyp | DClickConc
    type doubleclickdef = dclick * tactic * seq
    let doubleclickdefs : doubleclickdef list ref = ref []
    let rec adddoubleclick (b, s, seq as p) =
      let rec insert =
        function
          [] -> [p]
        | (b', _, seq' as p') :: doubleclicks ->
            if b = b' && eqseqs (seq, seq') then p :: doubleclicks
            else p' :: insert doubleclicks
      in
      doubleclickdefs := insert !doubleclickdefs
    let rec deldoubleclick (b, seq) =
      doubleclickdefs :=
        ( <| )
          ((fun (b', _, seq') -> b <> b' || not (eqseqs (seq, seq'))),
           !doubleclickdefs)
    let rec cleardoubleclicks () = doubleclickdefs := []
    let rec matchdoubleclick sense seq =
      let rec match1 (sense', action', seq') =
        if sense = sense' then
          match seqmatch seq' seq empty with
            Some env -> Some (remaptactic env action')
          | None -> None
        else None
      in
      findfirst match1 !doubleclickdefs
  end
