type alertspec =
    Alert of (string * (string * alertspec option) list * int)
  | HowToTextSelect
  | HowToFormulaSelect
  | HowToDrag

and alertseverity = StopNow | Decide | ReadThis

val ask : alertseverity -> string -> (string * 'a) list -> int       -> 'a
                        (* message   buttons  actions      defaultindex  result   *)

val askCancel :
  alertseverity -> string -> (string * 'a) list -> 'a      -> int        -> 'a
                (* message   buttons   actions  cancelaction  defaultindex  result   
                 *    (set defaultindex = List.length buttons to choose Cancel)
                 *)


val askDangerously : string    -> string * 'a -> string * 'a -> 'a   -> 'a
                  (* message      Do             Don't          Cancel   Result
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
val defaultseverity       : 'a list -> alertseverity
val defaultseverity_alert : alertseverity
val patchalert            : string * alertspec -> unit
val resetalertpatches     : unit -> unit
val setComment            : string -> unit (* this demoted to a thing which sets a comment line *)
val showAlert             : alertseverity -> string -> unit (* this pops up a window *)

