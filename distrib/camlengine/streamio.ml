(* $Id$ *)

let rec _SwapStream ({contents = old} as loc) new__ = loc := new__; old
let instream = ref stdin
let outstream = ref stdout
let errstream = ref stderr
let writechar v = output_char !outstream v
let (write : string -> unit) = fun v -> output_string !outstream v
exception WritefGet
let rec writef writer format items =
  let rec wf a1 a2 a3 =
	match a1, a2, a3 with
	  0, _, _ -> ()
	| n, m, items ->
		match String.get format m with
		  '%' -> writer (List.hd items); wf (n - 1) (m + 1) (List.tl items)
		| '/' ->
			writechar (String.get format (m + 1));
			wf (n - 2) (m + 2) items
		| c -> writechar c; wf (n - 1) (m + 1) items
  in
  (try wf (String.length format) 0 items with _ -> raise WritefGet)

let rec readline () = input_line !instream
let rec readch () = input_char !instream 
let rec eof () = in_channel_length !instream = 0
