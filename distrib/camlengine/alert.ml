module type T =
  sig
    type alertspec =
        Alert of (string * (string * alertspec option) list * int)
      | HowToTextSelect
      | HowToFormulaSelect
      | HowToDrag
    type alertseverity = StopNow | Decide | ReadThis
    val defaultseverity : 'a list -> alertseverity
    val defaultseverity_alert : alertseverity
    val patchalert : string * alertspec -> unit
    val resetalertpatches : unit -> unit
    val setComment : string -> unit
    (* this demoted to a thing which sets a comment line *)
    val showAlert : alertseverity -> string -> unit
    (* this pops up a window *)
    val ask : alertseverity -> string -> (string * 'a) list -> int -> 'a
    (* message   buttons  actions   defaultindex  result   *)

    val askCancel :
      alertseverity -> string -> (string * 'a) list -> 'a -> int -> 'a
    (* message   buttons   actions  cancelaction  defaultindex  result   
     *             set defaultindex = List.length buttons to choose Cancel
     *)


    val askDangerously : string -> string * 'a -> string * 'a -> 'a -> 'a
    (* message      Do            Don't      Cancel   Result
     *
     * special version of askCancel, with Do as default, and
     * the buttons in "Do/Don't" positions -- like this
     * 
     * ICON
     * ICON                    message
     * ICON 
     * 
     * Don't                Cancel  Do
     *)

    val askChoice : string * string list list -> int option
  end
(* $Id$ 
 *
 * Now ANY alert can be patched. ANY of them, from ANYWHERE. RB 19/x/99 
 *
 *)

module M : T =
  struct
    open Japeserver.M
    open Optionfuns.M
    open Sml.M
    open Miscellaneous.M
    open Listfuns.M
    
    let askChoice = Japeserver.M.askChoice
    let setComment = Japeserver.M.setComment
    
    type alertspec =
        Alert of (string * (string * alertspec option) list * int)
      | HowToTextSelect
      | HowToFormulaSelect
      | HowToDrag
    type alertseverity = StopNow | Decide | ReadThis
    let rec defaultseverity bs = if List.length bs <= 1 then StopNow else Decide
    let defaultseverity_alert = StopNow
    let rec intseverity =
      function
        StopNow -> 2
      | Decide -> 1
      | ReadThis -> 0
    let alertpatches : (string * alertspec) list ref = ref []
    (* kept in inverse lexical order, so that "abcd" comes before "abc" *)
  
    let rec alertpatch s =
      let rec patched (h, a) =
        if String.length h <= String.length s && String.sub (s) (0) (String.length h) = h then Some a
        else None
      in
      match findfirst patched !alertpatches with
        Some (Alert ("", bs, def)) -> Some (Alert (s, bs, def))
      | aopt -> aopt
    let rec resetalertpatches () = alertpatches := []
    let rec patchalert (h, a) =
      let rec f =
        function
          (h', a' as p) :: ps ->
            if h > h' then (h, a) :: p :: ps
            else if h = h' then (h, a) :: ps
            else p :: f ps
        | [] -> [h, a]
      in
      alertpatches := f !alertpatches
    let rec ask code m (bs : (string * 'a) list) def =
      if null bs then raise (Catastrophe_ ["ask no buttons \""; m; "\""])
      else if def < 0 || def >= List.length bs then
        raise
          (Catastrophe_
             ["ask bad default \""; m; "\"";
              bracketedliststring fst "," bs; " "; string_of_int def])
      else
        let i =
          ask_unpatched (intseverity code) m (List.map fst bs) def
        in
        try snd (List.nth bs i) with
          Failure "nth" ->
            raise
              (Catastrophe_
                 ["ask bad result \""; m; "\"";
                  bracketedliststring fst "," bs; " ";
                  string_of_int def; " => "; string_of_int i])
    let rec askCancel code m (bs : (string * 'a) list) c def =
      if null bs then
        raise (Catastrophe_ ["askCancel no buttons \""; m; "\""])
      else if def < 0 || def > List.length bs then
        raise
          (Catastrophe_
             ["askCancel bad default \""; m; "\"";
              bracketedliststring fst "," bs; " "; string_of_int def])
      else
        match
          askCancel_unpatched (intseverity code) m (List.map fst bs) def
        with
          Some i ->
            snd 
              (try List.nth bs i with
                 Failure "nth" ->
                   raise
                     (Catastrophe_
                        ["ask bad result \""; m; "\"";
                         bracketedliststring fst "," bs; " ";
                         string_of_int def; " => "; string_of_int i]))
        | None -> c
    let rec askDangerously m (dol, doa) (dontl, donta) cancela =
      match askDangerously_unpatched m dol dontl with
        Some 0 -> doa
      | Some 1 -> donta
      | None -> cancela
      | Some i ->
          raise (Catastrophe_ ["askDangerously_unpatched => "; string_of_int i])
    (* we allow the user to patch an alert *)
    let rec showAlert code s =
      let rec display code s = ask code s ["OK", ()] 0 in
      let rec patch code aopt =
        match aopt with
          None -> ()
        | Some (Alert (m, bs, def)) ->
            let code' = if List.length bs > 1 then Decide else code in
            patch ReadThis (ask code' m bs def)
        | Some HowToTextSelect -> display ReadThis (howtoTextSelect ())
        | Some HowToFormulaSelect -> display ReadThis (howtoFormulaSelect ())
        | Some HowToDrag -> display ReadThis (howtoDrag ())
      in
      match alertpatch s with
        None -> display code s
      | aopt -> patch code aopt
  end
