(* $Id$ *)

open Sml

let usestack = ref ["./"]
let rec makerelative =
  function
    "" -> ""
  | path ->
      match String.sub path 0 1 with
        "." -> path
      | "/" -> path
      | _ -> List.hd !usestack ^ path
let rec startusing path =
  let rec stem path = implode (List.rev (removestern (List.rev (explode path))))
  and removestern =
    function
      [] -> ["./"]
    | "/" :: xs -> "/" :: xs
    | x :: xs -> removestern xs
  in
  usestack := stem path :: !usestack
exception Matchinstopusing
(* spurious *)
let rec stopusing () =
  match !usestack with
    [path] -> ()
  | path :: paths -> usestack := paths
  | _ -> raise Matchinstopusing
