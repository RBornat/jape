let rec fix ff = ff (fix ff)

open Hashtbl
let rec memofix mem ff =
  let rec proxy k =
	try find mem k with Not_found -> let v = ff proxy k in add mem k v; v
  in
  proxy

