(* $Id$ *)

module type Rewinf =
  sig
    type rewinf
    type term and vid
    val nullrewinf : rewinf
    val rewinf_vars : rewinf -> term list
    val rewinf_uVIDs : rewinf -> vid list
    val rewinf_badres : rewinf -> int list
    val rewinf_psig : rewinf -> int option
    val rewinf_setvars : rewinf -> term list -> rewinf
    val rewinf_setuVIDs : rewinf -> vid list -> rewinf
    val rewinf_setbadres : rewinf -> int list -> rewinf
    val rewinf_setpsig : rewinf -> int option -> rewinf
    val rewinf_addvars : rewinf -> term list -> rewinf
    val rewinf_adduVIDs : rewinf -> vid list -> rewinf
    val rewinf_addbadres : rewinf -> int list -> rewinf
    val mkrewinf : term list * vid list * int list * int option -> rewinf
    val rew2rawinf : rewinf -> term list * vid list * int list * int option
    val rewinfstring : rewinf -> string
    val rewinf_merge : rewinf * rewinf -> rewinf
  end
(* $Id$ *)

module
  Rewinf
  (AAA :
    sig
      type vid and resnum and term
      val bracketedliststring : ('a -> string) -> string -> 'a list -> string
      val mergevars : term list -> term list -> term list
      val mergeVIDs : vid list -> vid list -> vid list
      val optionstring : ('a -> string) -> 'a option -> string
      val quadruplestring :
        ('a -> string) -> ('b -> string) -> ('c -> string) ->
          ('d -> string) -> string -> 'a * 'b * 'c * 'd -> string
      val sortedmerge : ('a * 'a -> bool) -> 'a list * 'a list -> 'a list
      val termliststring : term list -> string
      val vidstring : vid -> string
    end)
  :
  Rewinf =
  struct
    open AAA
    type vid = vid and term = term and resnum = resnum
    (* you'd think it would know ... *)
       
       (* see rewrite.sml for an explanation of this data structure *)
    type rewinf = Rewinf of (term list * vid list * int list * int option)
    let nullrewinf = Rewinf ([], [], [], None)
    let mkrewinf = Rewinf
    let rec rew2rawinf = fun (Rewinf r) -> r
    let rec rewinf_vars = fun (Rewinf (vars, _, _, _)) -> vars
    let rec rewinf_uVIDs = fun (Rewinf (_, uVIDs, _, _)) -> uVIDs
    let rec rewinf_badres = fun (Rewinf (_, _, badres, _)) -> badres
    let rec rewinf_psig = fun (Rewinf (_, _, _, psig)) -> psig
    let rec rewinf_setvars =
      fun (Rewinf (_, uVIDs, badres, psig)) vars ->
        Rewinf (vars, uVIDs, badres, psig)
    let rec rewinf_setuVIDs =
      fun (Rewinf (vars, _, badres, psig)) uVIDs ->
        Rewinf (vars, uVIDs, badres, psig)
    let rec rewinf_setbadres =
      fun (Rewinf (vars, uVIDs, _, psig)) badres ->
        Rewinf (vars, uVIDs, badres, psig)
    let rec rewinf_setpsig =
      fun (Rewinf (vars, uVIDs, badres, _)) psig ->
        Rewinf (vars, uVIDs, badres, psig)
    let rec rewinf_addvars =
      fun (Rewinf (vars, uVIDs, badres, psig)) vars' ->
        Rewinf (vars' @ vars, uVIDs, badres, psig)
    let rec rewinf_adduVIDs =
      fun (Rewinf (vars, uVIDs, badres, psig)) uVIDs' ->
        Rewinf (vars, uVIDs' @ uVIDs, badres, psig)
    let rec rewinf_addbadres =
      fun (Rewinf (vars, uVIDs, badres, psig)) badres' ->
        Rewinf (vars, uVIDs, badres' @ badres, psig)
    let rec rewinfstring =
      fun (Rewinf r) ->
        "Rewinf" ^
          quadruplestring termliststring (bracketedliststring vidstring ",")
            (bracketedliststring string_of_int ",") (optionstring string_of_int) ","
            r
    let rec rewinf_merge =
      fun
        (Rewinf (allvars, uVIDs, badres, psig),
         Rewinf (allvars', uVIDs', badres', psig')) ->
        Rewinf
          (mergevars allvars allvars', mergeVIDs uVIDs uVIDs',
           sortedmerge (fun (x, y) -> x < y) (badres, badres'),
           (match psig, psig' with
              Some n, Some n' -> Some (min (n) (n'))
            | Some _, None -> psig
            | _ -> psig'))
  end
