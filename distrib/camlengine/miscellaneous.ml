(* $Id$ *)

module type T = 
sig    
    val atoi : string -> int
    exception AtoI_
    val error : string list -> 'a
    exception Error_
    val observe : string list -> unit
    val sum : int list -> int
    val charpred : string -> (string -> bool) * (string * bool -> unit)
    val isdigit : string -> bool
    val islcletter : string -> bool
    val isletter : string -> bool
    val isucletter : string -> bool
    val iter : (int -> 'a) -> int * int -> unit
    val curry2 : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val uncurry2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
    val create_reportfile : string -> unit
    val close_reportfile : unit -> unit
    val consolereport : string list -> unit
    val consolequery : string list * string * string * int -> bool
    val refstring : ('a -> string) -> 'a ref -> string
    val earlierpair :
      ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a * 'b) -> ('a * 'b) -> bool
    val onbra : char
    val onket : char
    val offbra : char
    val offket : char
    val outbra : char
    val outket : char
    val lockbra : char
    val lockket : char
    val invisible : string -> bool
    val lemmacount : int ref
    (* number of lemmas during THIS proof *)
    val applyconjectures : bool ref
    (* conjectures allowed in proofs *)
    val applyderivedrules : bool ref
    (* derived rules allowed in proofs *)
    val autoselect : bool ref
    (* show 'goal' when printing proofs *)
    val givenMenuTactic : string ref
    (* what to use when the interface says applygiven *)
    val foldformulae : bool ref
    (* whether to fold long lines in boxdraw *)
    val truncatereasons : bool ref
    (* whether to shorten reasons in boxdraw *)
    val seektipselection : bool ref
    (* look for a tip to work on in boxdraw *)
    val textselectionmode : string ref
    (* how to press-and-drag over text *)

    val screenpositiondebug : bool ref
    exception Catastrophe_ of string list
    exception ParseError_ of string list
    exception Tacastrophe_ of string list
end

(* $Id$ *)

module M : T =
  struct
    open SML.M
       
    let rec iter f (l, h) =
      if l > h then () else begin f l; iter f (l + 1, h) end
    let rec charpred s =
      let v = Array.make 256 false in
      iter (fun i -> Array.set v (Char.code (String.get s i)) true) (0, String.length s - 1);
      (fun c -> Array.get v (Char.code (String.get c 0))),
      (fun (c, b) -> Array.set v (Char.code (String.get c 0)) b)
    let (islcletter, _) = charpred "abcdefghijklmnopqrstuvwxyz"
    let (isucletter, _) = charpred "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let rec isletter c = islcletter c || isucletter c
    let (isdigit, _) = charpred "0123456789"
    exception AtoI_
    let atoi s = try Pervasives.int_of_string s with Failure _ -> raise AtoI_
    let errstream : out_channel ref = ref stderr
    let reporteropen = ref false
    let rec create_reportfile s =
      if !reporteropen then close_out !errstream;
      errstream := open_out s;
      reporteropen := true
    let rec close_reportfile () =
      if !reporteropen then close_out !errstream;
      errstream := stderr;
      reporteropen := false
    let rec consolereport strings =
      let e = !errstream in
      List.iter (output_string e) strings; output_string e "\n"; flush e
    let observe = consolereport
    let rec consolequery (message, yes, no, def) =
      List.iter (output_string stdout) message;
      output_string stdout "  ";
      let rec q () =
        List.iter (output_string stdout) [yes; "(y)/"; no; "(n)? "];
        flush stdout;
        match input_char stdin with
          'Y' -> true
        | 'y' -> true
        | 'N' -> false
        | 'n' -> false
        | _ -> s ()
      and s () =
        match input_char stdin with
          '\n' -> output_string stdout "Pardon? "; q ()
        | _ -> s ()
      in
      q ()
    exception Error_
    let rec error strings = consolereport strings; raise Error_
    let rec sum ns = List.fold_left (fun x y -> x + y) 0 ns
    let rec curry2 f a b = f (a, b)
    let rec uncurry2 f (a, b) = f a b
    let rec refstring f {contents = a} = ("ref(" ^ f a) ^ ")"
    let rec earlierpair lta ltb (a, b) (a', b') =
      lta a a' || not (lta a' a) && ltb b b' (* this is trying not to use equality ... *)
    let lemmacount = ref 0
    (* number of lemmas during THIS proof *)
    let applyconjectures = ref true
    (* whether to allow application of conjectures *)
    let applyderivedrules = ref true
    (* whether to allow application of derived rules *)
    let autoselect = ref true
    (* whether to highlight 'next goal' when printing proofs *)
    let givenMenuTactic = ref ""
    (* tactic to use when the interface says applygiven *)
     
    let foldformulae = ref false
    (* whether to fold long lines in boxdraw *)
    let truncatereasons = ref false
    (* whether to shorten reasons in boxdraw *)
      
    let seektipselection = ref true
    (* whether to look for a tip to work on in boxdraw *)
      
    let textselectionmode = ref "subformula"
    let screenpositiondebug = ref true
    (* this collection avoids low characters which might be useful *)
    (* 0 is NUL; can't appear in a C string, so don't use *)
    let onbra = '\001'  (* SOH *)
    let onket = '\002'  (* STX *)
    let offbra = '\003' (* ETX *)
    let offket = '\004' (* EOT *)
    let outbra = '\005' (* ENQ *)
    let outket = '\006' (* ACK *)
       (* 7 is bell
          8 is backspace
          9 is tab
          10 is lf
          11 is vt
          12 is ff
          13 is cr
        *)
    let lockbra = '\014' (* SO *)
    let lockket = '\015' (* SI *)
       
	(* this function records a decision NEVER to put printable characters below space 
	   (ASCII decimal 32) in a font.
	 *)
    let invisible_char c = (onbra <= c && c <= outket) || c = lockbra || c = lockket
    let invisible s =
      not (List.exists (not <*> invisible_char) (char_explode s))
    
    exception Catastrophe_ of string list
    exception ParseError_ of string list
    exception Tacastrophe_ of string list
  
  end

