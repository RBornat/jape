(* $Id$ *)

module type env =
  sig
    val getenv : string -> string -> string
    (* search acquired environment       *)
    val setenv : string * string -> unit
    (* augment ditto                     *)
    val variables : unit -> string list
    (* domain of the environment         *)
    val reset : unit -> unit
    (* reset to the original environment *)
    val currentenv : unit -> string list
  end
(* $Id$ *)

module env : env =
  struct
    let (env : (string * string) list ref) = ref []
    let rec reset () = env := []
    let rec split (s : string) =
      let eq = Char.code '='
      and p = ref 0 in
      while !p <> String.length s - 1 && Char.code (String.get s (!p)) <> eq do inc p done;
      match !p with
        0 -> s, s
      | n -> substring (s, 0, n), substring (s, n + 1, String.length s - n - 1)
    let rec declenv s = env := map split s; !env
    let rec fetchenv () =
      match !env with
        [] -> declenv (System.environ ())
      | env -> env
    let rec currentenv () = map (fun (n, v) -> (n ^ "=") ^ v) (fetchenv ())
    let rec setenv nv = env := nv :: fetchenv ()
    let rec getenv d s =
      let rec F =
        function
          [] -> d
        | (n, v) :: nvs -> if n = s then v else F nvs
      in
      F (fetchenv ())
    let rec nodups l =
      fold
        (fun (x, xs) -> if List.exists (fun x' -> x = x') xs then xs else x :: xs)
        l []
    let rec variables () = nodups (map sml__hash__1 (fetchenv ()))
  end

