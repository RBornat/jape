(* $Id$ *)

module type Streamio =
  sig
    val instream : Pervasives.in_channel ref
    (* current input stream *)
    val outstream : Pervasives.out_channel ref
    (* current output stream *)
    val errstream : Pervasives.out_channel ref
    (* current error stream *)
    val swapstream : 'a ref -> 'a -> 'a
    val eof : unit -> bool
    (* instream is at end of file *)
    val readch : unit -> char
    (* read character from instream *)
    val readline : unit -> string
    (* read line from instream *)
    val writestring : string -> unit
    (* write string to outstream *)
    val write : string -> unit
    (* write string to outstream *)
    val writebool : bool -> unit
    val writeint : int -> unit
    val writerj : string -> int -> unit
    val writereal : float -> unit
    val writelist : ('a -> 'b) -> string -> 'a list -> unit
    val writeoption : ('a -> 'b) -> string -> 'a option -> unit
    val writef : ('a -> 'b) -> string -> 'a list -> unit
  end

(* $Id$ *)

module Streamio : Streamio =
  struct
    let rec swapstream ({contents = old} as loc) new__ = loc := new__; old
    let instream = ref stdin
    let outstream = ref stdout
    let errstream = ref stderr
    let writechar v = output_char !outstream v
    let (writestring : string -> unit) = fun v -> output_string !outstream v
    let (write : string -> unit) = fun v -> output_string !outstream v
    let (writeint : int -> unit) = fun v -> writestring (string_of_int v)
    let rec writerj s n =
      if String.length s < n then begin write " "; writerj s (n - 1) end else write s
    let (writereal : float -> unit) = fun v -> writestring (string_of_float v)
    let (writebool : bool -> unit) = fun v -> writestring (string_of_bool v)
    let rec writelist writeitem comma =
      let rec wl =
        function
          [] -> ()
        | x :: xs ->
            writeitem x;
            if xs=[] then () else begin writestring comma; wl xs end
      in
      wl
    let rec writeoption witem none =
      function
        None -> writestring none
      | Some i -> witem i; ()
    let rec writef writer format items =
      let rec wf a1 a2 a3 =
        match a1, a2, a3 with
          0, _, _ -> ()
        | n, m, items ->
            match Char.chr (Char.code (String.get format m)) with
              '%' -> writer (List.hd items); wf (n - 1) (m + 1) (List.tl items)
            | '/' ->
                writechar (Char.chr (Char.code (String.get format (m + 1))));
                wf (n - 2) (m + 2) items
            | c -> writechar c; wf (n - 1) (m + 1) items
      in
      wf (String.length format) 0 items
    
    let rec readline () = input_line !instream
    let rec readch () = input_char !instream 
    let rec eof () = in_channel_length !instream = 0
  
  end

