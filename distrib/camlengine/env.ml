(* $Id$ *)

module type T =
  sig
    val getenv : string -> string -> string (* search acquired environment       *)
    val setenv : string * string -> unit    (* augment ditto                     *)
    val variables : unit -> string list     (* domain of the environment         *)
    val reset : unit -> unit                (* reset to the original environment *)
    val currentenv : unit -> string list
  end

module M : T  =
  struct
    open SML.M
    
    let (env : (string * string) list ref) = ref []
    let rec reset () = env := []
    let rec split (s : string) =
      let eq = Char.code '=' in
      let p = ref 0 in
      while !p <> String.length s - 1 && Char.code (String.get s !p) <> eq do incr p done;
      match !p with
        0 -> s, s
      | n -> String.sub s 0 n, String.sub s (n+1) (String.length s - n - 1)
    let rec declenv s = env:=List.map split s; !env
    let rec fetchenv () =
      match !env with
        [] -> declenv (Array.to_list (Unix.environment ()))
      | env -> env
    let rec currentenv () = List.map (fun (n, v) -> (n ^ "=") ^ v) (fetchenv ())
    let rec setenv nv = env := nv :: fetchenv ()
    let rec getenv d s =
      let rec _F =
        function
          [] -> d
        | (n, v) :: nvs -> if n = s then v else _F nvs
      in
      _F (fetchenv ())
    let rec nodups l =
      nj_fold
        (fun (x, xs) -> if List.exists (fun x' -> x = x') xs then xs else x :: xs)
        l []
    let rec variables () = nodups (List.map fst (fetchenv ()))
  end

