(* $Id$ *)

module type Server =
  sig
    type box
    and displayclass
    and font
    and pane
    and panelkind
    and panelbuttoninsert
    and pos
    and size
    and textsize
    val idlsignature : string
    val getSignature : unit -> string
    val sendVersion : string -> unit
    val sendOperators : string list -> unit
    val setFonts : string -> unit
    (* send font-encoding name *)
    val fontinfo : font -> int * int * int
    (* ascent, descent, leading *)
       
       (* By default all drawing takes place in the front window. 
        * When fonts change, background windows have to be refreshed.
        * The following allow the engine temporarily to redirect
        * drawing to a background window.  If canbackgroundfocus is false, 
        * it won't do such a thing.
        *)
    val canbackgroundfocus : bool
    val setbackgroundfocus : int -> unit
    val setforegroundfocus : unit -> unit
    val measurestring : font * string -> int * int * int
    val procrustes : int -> string -> font -> string -> string
    (* width   ...               string    what fits *)

(* drawmeasuredtext takes a selection class (0 is non-selectable),
                 an (offset*font*string) list and
                 a position
*)
    val drawmeasuredtext :
      displayclass -> (pos * font * string) list -> pos -> unit
    (* nowadays we can draw in the proof pane or the disproof pane *)
    val drawinpane : pane -> unit
    (* disproof has sequent and term-buttons and worlds; disproof sequent is drawn separately *)
    val setdisproofseqbox : box -> unit
    (* followed by some drawing *)
    val setdisprooftiles : string list -> unit
    val setdisproofworlds :
      (int * int) list ->
        ((int * int) * string list * (int * int) list) list -> unit
    (* the mad world of menus and panels *)
    val cancelmenusandpanels : unit -> unit
    val emptymenusandpanels : unit -> unit
    val newmenu : string -> unit
    val menuentry : string * string * string option * string -> unit
    (* menu, label, key equiv, cmd *)
    val menucheckbox : string * string * string -> unit
    (* menu, label, cmd *)
    val menuradiobutton : string * (string * string) list -> unit
    (* menu, (label, cmd) *)
    val menuseparator : string -> unit
    (* menu *)
    val enablemenuitem : string * string * bool -> unit
    (* applies to entries, checkboxes, radio buttons *)
    val tickmenuitem : string * string * bool -> unit
    (* only for checkboxes, radio buttons *)

    val mapmenus : bool -> unit
    (* false: menus under construction; true: menus constructed *)
     
    val newpanel : string * panelkind -> unit
    val panelentry : string * string * string -> unit
    val panelbutton : string * string * panelbuttoninsert list -> unit
    val panelcheckbox : string * string * string -> unit
    val panelradiobutton : string * (string * string) list -> unit
    val setpanelbutton : string * string * bool -> unit
    val selectpanelentry : string * string -> unit
    val markpanelentry : string -> string -> bool -> unit
    val listen : unit -> string
    val terminate : unit -> unit
    val closedown : unit -> unit
    val killserver : unit -> unit
    val drawLine : box -> unit
    (* top right to bottom left, staying within the box, 
                                    using linethickness from setproofparams
                                  *)
    val drawRect : box -> unit
    (* just inside the box, using linethickness from setproofparams *)

    val running : bool ref
    val servername : string ref
    val startserver : string -> string list -> unit
    val stopserver : unit -> unit
    val openproof : string * int -> unit
    val closeproof : int -> unit
    val showfile : string -> unit
    val echo : string -> string
    val setProvisos : font * string list -> unit
    (* font * provisos *)
    val setGivens : (int * string) list -> unit
    (* numbered givens *)
       
    val quit : unit -> unit
    val dontquit : unit -> unit
    val getProofPane : unit -> box
    val getDisproofPane : unit -> box
    val clearProofPane : unit -> unit
    val clearDisproofPane : unit -> unit
    (* To make findSelection (interaction.sml) work properly, and to get consistent results from tactics that
     * interpret the answers, getAllSelections should return its answers in time-click order.
     *)
    val getAllSelections :
      unit ->
        (pos * displayclass) list * (pos * string list) list * string list
    (* selections               prooof text selections     givens text selections *)

    val highlight : pos -> displayclass option -> unit
    (* NOW TAKES TEXTPOS, NOT BOXPOS!!! *)
    val emphasise : pos -> bool -> unit
    (* also textpos; used in disproof *)

    val greyen : pos -> unit
    val blacken : pos -> unit
    val toplevelfiletype : int
    (* .jt *)
    val theoryfiletype : int
    (* .j  *)
    val prooffiletype : int
    (* .jp *)
       
    val dbugfiletype : int
    (* whatever you like *)
       
       (* In the following calls, 
        *   string is a message to put in the dialogue box,
        *   int is a filetype
        * Either argument may be safely ignored.
        *)
    val writeFileName : string -> int -> string option
    val readFileName : string -> int -> string option
    val resetcache : unit -> unit
    (* forget all cached information *)
     
     (* how to draw things *)
    val setinvischars :
      string * string -> string * string -> string * string ->
        string * string -> unit
    (*   onbra  ket         offbra ket         outbra ket         lockbra ket *)
    val setproofparams : string -> int -> unit
    (*    tree/box  linethickness *)

(* interface-specific help information *)
    val howtoTextSelect : unit -> string
    val howtoFormulaSelect : unit -> string
    val howtoDrag : unit -> string
    (* nascent variable mirroring *)
    val settextselectionmode : string -> unit
  end

module type Alert =
  sig
    val setComment : string -> unit
    (* this demoted to a thing which sets a comment line *)
       
    val ask_unpatched : int -> string -> string list -> int -> int
    (* severity 0/1/2    message   buttons     default   which one was pressed, indexed from 0*)

    val askCancel_unpatched :
      int -> string -> string list -> int -> int option
    (* severity 0/1/2    message   buttons      default  Some button (counting from 0) or None for Cancel 
     *             set default = length buttons to choose Cancel
     *)


    val askDangerously_unpatched : string -> string -> string -> int option
    (* message   Do        Don't     Some button (counting from 0) or None for Cancel
     * special version of ask_cancel, with Do as default, and
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

(*
        tcl/tk implementation of the Jape server
        Bernard Sufrin,
        Oxford July 1993

        $Id$

*)



module M : sig include Server include Alert end 
           with type box = Box.M.box
			and type displayclass = Displayclass.M.displayclass
			and type font = Displayfont.M.displayfont
			and type pane = Displayfont.M.pane
			and type panelbuttoninsert = Panelkind.M.panelbuttoninsert
			and type panelkind = Panelkind.M.panelkind
			and type pos = Box.M.pos
			and type size = Box.M.size
            and type textsize = Box.M.textsize
=
  struct
    open Array

    open Box.M
    open Displayfont.M
    open Displayclass.M
    open Listfuns.M
    open Miscellaneous.M
    open Sml.M
    
    type box = Box.M.box
     and displayclass = Displayclass.M.displayclass
     and font = Displayfont.M.displayfont
     and pane = Displayfont.M.pane
     and panelbuttoninsert = Panelkind.M.panelbuttoninsert
     and panelkind = Panelkind.M.panelkind
     and pos = Box.M.pos
     and size = Box.M.size
     and textsize = Box.M.textsize
    
    let version =
      "$Id$"

    (* Fit Richard's box adt back to into his earlier concrete form *)
    let rec explodeBox box = bPos box, bSize box
    let rec explodePos pos = posX pos, posY pos
    let rec explodeSize s = sW s, sH s
    let rec explodeTextSize s = tsW s, tsA s, tsD s
    (* width, ascent, descent *)

   (*  Low level details [should be done in idlbase] *)

    exception Server_input_terminated
    exception Server_output_terminated
    
    let (infromserver, outtoserver) = ref stdin, ref stdout
    let running = ref false
    let servername = ref "<no server>"
    let rec stopserver () =
      if !running then
        begin
          begin try close_in !infromserver with
            _ -> ()
          end;
          begin try close_out !outtoserver with
            _ -> ()
          end;
          (* System.Unsafe.CInterface.exit 2 *) ()
        end
    let rec startserver server args =
      let (iii, ooo) =
        stopserver (); (stdin, stdout) (* IO.execute_in_env (server, args, System.environ ()) *)
      in
      (* Import from OPSYS *)
      servername := server;
      infromserver := iii;
      outtoserver := ooo;
      running := true
    and write s = out s; out "\n"; flush ()
    and out s = output_string !outtoserver s
    and flush () =
      (* try flush !outtoserver with
        Io s ->
          output_string (std_err, ("[WARNING: Server " ^ !servername) ^ " died]\n");
          stopserver () *) Pervasives.flush !outtoserver
    let rec visible s = implode (List.map vis (explode s))
    and vis c = if c < " " then "\\" ^ string_of_int (ord c) else c
    let rec front =
      function
        "" -> ""
      | s ->(* outputc std_err ("<= "^s); flush std_err; *)  String.sub s 0 (String.length s - 1)
    let rec readline s = flush (); front (input_line !infromserver)
    let rec nn_ =
      function
        "-" :: ds -> - dd_ 0 ds
      | ds -> dd_ 0 ds
    and dd_ a1 a2 =
      match a1, a2 with
        r, [] -> r
      | r, d :: ds -> dd_ (r * 10 + ord d - ord "0") ds
    let rec ints readline =
      let rec ff_ a1 a2 a3 =
        match a1, a2, a3 with
          s, r, [] -> nn_ s :: r
        | s, r, " " :: xs -> ff_ [] (nn_ s :: r) xs
        | s, r, x :: xs -> ff_ (x :: s) r xs
      in
      ff_ [] [] (List.rev (explode readline))
    let rec atoi s = nn_ (explode s)

    type _ITEM = Int of int | Str of string
    let fInt v = Int v
    let fStr v = Str v
    
    let rec writef s is =
      let rec ww_ a1 a2 =
        match a1, a2 with
          [], _ -> ()
        | "%" :: "%" :: f, is -> out "%"; ww_ f is
        | "%" :: f, Int i :: is -> out (intstring i); ww_ f is
        | "%" :: f, Str s :: is -> outs s; ww_ f is
        | c :: cs, is -> out c; ww_ cs is
      in
      ww_ (explode s) is
    and intstring i = if i < 0 then "-" ^ string_of_int (- i) else string_of_int i
    and outs s =
      List.iter
        (function
           "[" -> out "\\["
         | "]" -> out "\\]"
         | "{" -> out "\\{"
         | "}" -> out "\\}"
         | "$" -> out "\\$"
         | ";" -> out "\\;"
         | " " -> out8 " "
         | "\n" -> out "\\n"
         | "\"" -> out "\\\""
         | "\\" -> out "\\\\"
         | c -> if ord c < 32 then out8 c else out c)
        (explode s)
    and out8 c =
      let i = ord c in
      let r =
        implode
          ["\\"; string_of_int (i / 64); string_of_int (i mod 64 / 8);
           string_of_int (i mod 8)]
      in
      (*consolereport ["mks8 ", string_of_int i, " => ", r];*)
      out r
    let rec askf s is = writef s is; ints (readline s)
    let rec ask s = out s; out "\n"; readline s
    let rec listen () = writef "GET\n\n\n" []; readline "GET"
    let rec terminate () = writef "TERMINATE\n" []
    let rec closedown () = writef "TERMINATE\n" []
    let rec killserver () = writef "TERMINATE\n" []
    (*  Local to the interface *)

            (* client stuff *)

    let idlsignature = ""
    let rec getSignature () = ""
    (* By default all drawing takes place in the front window.
     * When fonts change, background windows have to be refreshed.
     * The following allow the engine temporarily to redirect
     * drawing to a background window.  If canbackgroundfocus is false,
     * it won't do such a thing.
     *)
     
    let canbackgroundfocus = false
    let rec setbackgroundfocus _ = ()
    let rec setforegroundfocus _ = ()
    let rec sendVersion v = writef "VERSION %\n" [Str v]
    let rec sendOperators strings =
      writef "OPERATORSBEGIN\n" [];
      List.iter (fun s -> writef "OPERATOR %\n" [Str s]) strings;
      writef "OPERATORSEND\n" []
    (* fonts *)

    let rec setFonts stringfromtheory =
      writef "SETFONTS \"%\"\n" [Str stringfromtheory]
    exception Fontinfo_
    let rec fontinfo fontn =
        match askf "FONTINFO %\n" [Int (displayfont2int fontn)] with 
          [asc; desc; lead]  -> asc, desc, lead
        | _ -> raise Fontinfo_
    let rec getPointSize n =
      (* actually it's font height, and it isn't used *)
      match askf "POINTSIZE %\n" [Int n] with
        [n] -> n
      | _ -> output_string stderr "[POINTSIZE FAILS]\n"; 0
    let invischars : string list ref = ref []
    let rec printable s =
      implode ((fun c -> not (member (c, !invischars))) <| explode s)
    (* NEED TO CACHE THE RESULTS OF THIS *)
    exception Measurestring_
    let rec measurestring (fontn, string) =
	  match askf "STRINGSIZE % \"%\"\n" [Int (displayfont2int fontn); Str (printable string)] with
		[width; asc; desc] -> width, asc, desc
	  | _ -> raise Measurestring_
    let rec loadFont (fontn, name) =
      writef "LOADFONT % \"%\"\n" [Int fontn; Str name]
    (***************)

    let linethickness = ref 1
    let rec setinvischars
      (onbra, onket) (offbra, offket) (outbra, outket) (lockbra, lockket) =
      invischars :=
        [onbra; onket; offbra; offket; outbra; outket; lockbra; lockket];
      writef "SETINVISCHARS % % % % % % % %\n"
        (List.map (fun ooo -> Int (ord ooo)) !invischars)
    let rec settextselectionmode m = writef "SETTEXTSELECTIONMODE %\n" [Str m]
    let rec drawLine box =
      let (pos, size) = explodeBox box in
      let (x, y) = explodePos pos in
      let (w, h) = explodeSize size in
      writef "DRAWLINE % % % %\n" (List.map fInt [x; y; x + w; y])
    let rec drawRect box =
      let (pos, size) = explodeBox box in
      let (x, y) = explodePos pos in
      let (w, h) = explodeSize size in
      writef "DRAWRECT % % % % %\n"
        (List.map fInt [!linethickness; x - 1; y - 1; x + w + 1; y + h + 1])
    let rec drawinpane =
      function
        ProofPane -> writef "DRAWINPANE proof\n" []
      | DisproofPane -> writef "DRAWINPANE disproof\n" []
    let rec drawstring (font, class__, s, pos) =
      let (x, y) = explodePos pos in
      writef "DRAWSTRING % % % % % %\n"
        [Int x; Int y; Int font; Int class__; Str (printable s); Str s]
    let rec drawmeasuredtext class__ lines pos =
      (* : displayclass ->(pos*font*string) list -> pos -> unit *)
      let classn = displayclass2int class__ in
      match lines with
        [pos', font, string] ->
          let fontn = displayfont2int font in
          drawstring (fontn, classn, string, Box.M.( +->+ ) (pos, pos'))
      | [] -> ()
      | _ ->
          raise
            (Catastrophe_
               ["drawmeasuredtext sees list of texts of length ";
                string_of_int (List.length lines)])
    (* this won't work with annotated text ... fix later. RB *)
    let rec procrustes width ellipsis font text =
      (* if stringwidth(text in font) < width then text else [text] cut to width - widthof ...*)
      match
        askf "PROCRUSTES % % % %\n"
          [Int width; Str ellipsis; Int (displayfont2int font);
           Str (printable text)]
      with
        [n] -> String.sub text 0 n
      | _ -> "Procrustes says 'goodness me'"
    let rec howtoTextSelect () = ask "HOWTO textselect"
    let rec howtoFormulaSelect () = ask "HOWTO formulaselect"
    let rec howtoDrag () = ask "HOWTO drag"
    (***************)
    let commentSet = ref false
    let rec setComment s =
      if s = "" && not !commentSet then ()
      else begin commentSet := true; writef "SETCOMMENT \"%\"\n" [Str s] end
    let rec showAlert s = writef "SETALERT   \"%\"\n" [Str s]
    let rec ask_unpatched severity message buttons default =
      writef "ASKSTART\n" [];
      List.iter (fun but -> writef "ASKBUTTON {%}\n" [Str but]) buttons;
      match
        askf "ASKNOW % {%} %\n" [Int severity; Str message; Int default]
      with
        [n] -> n
      | _ -> raise (Catastrophe_ ["ask protocol failure"])
    let rec askDangerously_unpatched message doit dont =
      match
        askf "ASKDANGEROUS {%} {%} {%}\n" [Str message; Str doit; Str dont]
      with
        [0] -> None
      | [n] -> Some (n - 1)
      | _ -> raise (Catastrophe_ ["askdangerous protocol failure"])
    let rec askCancel_unpatched severity message buttons default =
      let n = ask_unpatched severity message (buttons @ ["Cancel"]) default in
      if n = List.length buttons then None else Some n
    let rec echo s = s
    let menus = ref ([] : (string * int) list)
    let menucount = ref 0
    let menusVisible = ref false
    let rec inventmenu name =
      let n = let r = !menucount in incr menucount; r in
      menus := (name, n) :: !menus; string_of_int n
    exception Findmenu_
    let rec findmenu name =
        let rec ff_ =
          function ((n, i) :: m) -> if name = n then string_of_int i else ff_ m
                   | _ -> raise Findmenu_
        in
        if name = "File" then "file"
        else if name = "Edit" then "edit"
        else ff_ !menus
         
    let rec existsmenu name =
      try let _ = findmenu name in true with
        _ -> false
    let rec emptymenusandpanels () =
      writef "EMPTYMENUSANDPANELS\n" [];
      menucount := 0;
      menus := [];
      menusVisible := false
    let rec cancelmenusandpanels () =
      writef "CANCELMENUSANDPANELS\n" [];
      menucount := 0;
      menus := [];
      menusVisible := false
    let rec openproof (name, number) =
      writef "OPENPROOF % %\n" [Str name; Int number]
    let rec closeproof number = writef "CLOSEPROOF %\n" [Int number]
    let rec newmenu name =
      if existsmenu name then ()
      else writef "NEWMENU % \"%\"\n" [Str (inventmenu name); Str name]
    let rec menuentry
      ((menuname : string), (label : string), (keyopt : string option),
       (entry : string)) :
      unit =
      writef "MENUENTRY % % %\n"
        [Str (findmenu menuname); Str label; Str entry]
    let rec menuseparator (menuname : string) =
      writef "MENUSEP %\n" [Str (findmenu menuname)]
    let rec setmenuentryequiv (menuname, label, key) : unit =
      let n = findmenu menuname in
      writef "MENUENTRYEQUIV % \"%\" \"%\"\n" [Str n; Str label; Str key]
    let rec makemenusVisible () =(*if !menusVisible then () else *)  writef "MAKEMENUSVISIBLE\n" []
    (*  mapmenus true  is called when menu construction is finished *)
    (*  mapmenus false is called as menu construction starts *)
    let rec mapmenus =
      function
        true -> makemenusVisible ()
      | false -> ()
    let rec enablemenuitem (menuname, label, state) =
      let n = findmenu menuname in
      let state = if state then "1" else "0" in
      writef "ENABLEMENUITEM % \"%\" \"%\"\n" [Str n; Str label; Str state]
    let rec tickmenuentry (menu, label, state) =
      let n = findmenu menu in
      let state = if state then "1" else "0" in
      writef "TICKMENUENTRY % \"%\" %\n" [Str n; Str label; Str state]
    open Panelkind.M
    let rec newpanel (name, panelkind) =
      let kind =
        match panelkind with
          TacticPanelkind -> "0"
        | ConjecturePanelkind -> "1"
        | GivenPanelkind -> "2"
      in
      writef "NEWPANEL % %\n" [Str name; Str kind]
    let rec panelbutton (name, label, cmd) =
      let rec ins =
        function
          StringInsert s -> " " ^ s
        | LabelInsert -> " %-%LABEL%-%"
        | CommandInsert -> " %-%COMMAND%-%"
      in
      let cmd = implode (List.map ins cmd) in
      writef "PANELBUTTON % % %\n" [Str name; Str label; Str cmd]
    let rec panelcheckbox (name, label, prefix) =
      writef "PANELCHECKBOX % % % \n" [Str name; Str label; Str prefix]
    let rec setpanelbutton (name, label, value) =
      writef "SETPANELBUTTON % % %\n"
        [Str name; Str label; Str (if value then "1" else "0")]
    let rec panelradiobutton (name, labelcomlist) =
      writef "BEGINRADIOBUTTON  %\n" [Str name];
      List.iter
        (fun (l, c) ->
           writef "RADIOBUTTONENTRY % % %\n" [Str name; Str l; Str c])
        labelcomlist;
      writef "ENDRADIOBUTTON  %\n" [Str name]
    let rec panelentry (name, label, entry) =
      writef "PANELENTRY % % %\n" [Str name; Str label; Str entry]
    let rec selectpanelentry (name, label) =
      writef "SELECTPANELENTRY % %\n" [Str name; Str label]
    let rec markpanelentry name label boolmark =
      writef "MARKPANELENTRY % % %\n"
        [Str name; Str label; Int (if boolmark then 1 else 0)]
    let rec greyen posn =
      let (x, y) = explodePos posn in writef "GREYEN % %\n" [Int x; Int y]
    let rec blacken posn =
      let (x, y) = explodePos posn in writef "BLACKEN % %\n" [Int x; Int y]
    let rec highlight posn classopt =
      let (x, y) = explodePos posn in
      match classopt with
        None -> writef "UNHIGHLIGHT % %\n" [Int x; Int y]
      | Some c ->
          writef "HIGHLIGHT % % %\n" [Int x; Int y; Int (displayclass2int c)]
    let rec readHighlight class__ =
      match askf "READHIGHLIGHT %\n" [Int class__] with
        [x; y] -> Some (pos (x, y))
      | _ -> None
    let rec clearProvisoView () = writef "CLEARPROVISOVIEW\n" []
    let rec showProvisoLine (fontn, s) =
      writef "SHOWPROVISOLINE % \"%\"\n" [Int fontn; Str s]
    let (setGivens : (int * string) list -> unit) =
      (* numbered givens *) fun givens ->
        out "CLEARGIVENS\n";
        List.iter (fun (n, g) -> writef "GIVEN % %\n" [Int n; Str g]) givens;
        out "SETGIVENS\n"
    let (setProvisos : font * string list -> unit) =
      (* font * provisos *) fun (fontn, ps) ->
        let fnn = Int (displayfont2int fontn) in
        clearProvisoView ();
        List.iter (fun s -> writef "SHOWPROVISOLINE % \"%\"\n" [fnn; Str s]) ps
    let rec showfile filename = writef "SHOWFILE \"%\"\n" [Str filename]
    let rec resetcache () = commentSet := false;(* initialize cache *)  writef "RESETCACHE\n" []
    let rec makeChoice heading =
      match askf "MAKECHOICE \"%\"\n" [Str heading] with
        [0] -> None
      | [n] -> Some (n - 1)
      | _ -> None
    let rec clearChoices () = writef "CLEARCHOICES\n" []
    let rec setChoice (show, reply) =
      writef "SETCHOICE \"%\" \"%\"\n" [Str show; Str reply]
    let rec setChoiceLine () = writef "SETCHOICELINE\n" []
    let rec setChoices ((caption : string), (c : string list list)) =
      let rec cs__ =
        function
          n, [] -> ()
        | n, c :: cs -> ch__ (n, c); cs__ (n + 1, cs)
      and ch__ =
        function
          n, ch :: chs ->
            let ns = string_of_int (n : int) in
            setChoice (ch, ns);
            List.iter (fun l -> setChoice (l, "")) chs;
            setChoiceLine ()
        | n, [] -> setChoiceLine ()
      in
      clearChoices (); cs__ (1, c); setChoice ("<None of the above>", "0")
    let rec askChoice (query, menu) =
      setChoices (query, menu); makeChoice query
    let rec quit () = writef "QUIT\n" []
    let rec dontquit () = writef "DONTQUIT\n" []
    let toplevelfiletype = 0
    (* .jt *)
    let theoryfiletype = 1
    (* .j  *)
    let prooffiletype = 2
    (* .jp *)
    let dbugfiletype = 3
    (* whatever you like *)
  
    let rec filetypeToString =
      function
        0 -> "jt"
      | 1 -> "j"
      | 2 -> "jp"
      | 3 -> "jdb"
      | n -> "jxx"
    let rec readFileName message filetype =
      writef "READFILENAME \"%\" \"%\"\n"
        [Str message; Str (filetypeToString filetype)];
      match readline "READFILENAME" with
        "" -> None
      | s -> Some s
    let rec writeFileName message filetype =
      writef "WRITEFILENAME \"%\" \"%\"\n"
        [Str message; Str (filetypeToString filetype)];
      match readline "WRITEFILENAME" with
        "" -> None
      | s -> Some s
    let rec setmenuequiv (menu, entry, command) = ()
    let rec explode_ s =
      let rec ff_ a1 a2 a3 =
        match a1, a2, a3 with
          w, r, [] -> implode w :: r
        | w, r, "\n" :: cs -> ff_ [] [] cs
        | w, r, "\001" :: cs -> ff_ [] (implode w :: r) cs
        | w, r, c :: cs -> ff_ (c :: w) r cs
      in
      ff_ [] [] (List.rev (explode s))
    let rec getSelection () =
      let l : (pos * string list) list ref = ref [] in
      writef "GETSELECTION\n" [];
      while
        match explode_ (readline "GETSELECTIONANSWER") with
          x :: y :: ss -> l := (pos (atoi x, atoi y), ss) :: !l; true
        | _ -> false
      do ()
      done;
      !l, []
    let rec getTextSelection () =
      let l : (pos * string list) list ref = ref [] in
      writef "GETTEXTSELECTION\n" [];
      while
        match explode_ (readline "GETTEXTSEL") with
          x :: y :: ss -> l := (pos (atoi x, atoi y), ss) :: !l; true
        | _ -> false
      do ()
      done;
      !l
    let rec getFormulaSelection () =
      let l : (pos * displayclass) list ref = ref [] in
      writef "GETBOXSELECTION\n" [];
      while
        match explode_ (readline "GETBOXSEL") with
          [x; y; classn] ->
            l := (pos (atoi x, atoi y), int2displayclass (atoi classn)) :: !l;
            true
        | _ -> false
      do ()
      done;
      !l
    let rec getGivenSelection () =
      writef "GETGIVENSELECTION\n" [];
      match explode_ (readline "GETGIVENSEL") with
        [""] -> []
      | xs -> xs
    let rec getAllSelections () =
      (* unit -> ... *)
         (* list of all formula selections, 
            list of all text selections, 
            the givens text selection 
         *)
         (* a text selection is a position, text-selection *)
      let textsel = getTextSelection () in
      let formsel = getFormulaSelection () in
      let givensel = getGivenSelection () in formsel, textsel, givensel
    let rec setOrigin p =
      let (x, y) = explodePos p in writef "SETORIGIN % %\n" [Int x; Int y]
    let rec dragtargets (segvars : string list) =
      writef "DROPBEGIN\n" [];
      List.iter (fun t -> writef "DROP %\n" [Str t]) segvars;
      writef "DROPEND\n" []
    (* things added for version 5.0 *)
    let rec setdisproofseqbox box =
      let (pos, size) = explodeBox box in
      let (x, y) = explodePos pos in
      let (w, h) = explodeSize size in
      writef "DISPROOFSEQBOX % % % %\n" (List.map fInt [x; y; x + w; y + h])
    let rec setdisprooftiles ts =
      writef "DISPROOFTILESSTART\n" [];
      List.iter (fun t -> writef "DISPROOFTILE %\n" [Str t]) ts;
      writef "DISPROOFTILESEND\n" []
    let rec setdisproofworlds selected worlds =
      writef "DISPROOFWORLDSSTART\n" [];
      List.iter
        (fun (sx, sy) -> writef "DISPROOFWORLDSELECT % %\n" [Int sx; Int sy])
        selected;
      List.iter
        (fun ((cx, cy), labels, children) ->
           writef "DISPROOFWORLD % %\n" [Int cx; Int cy];
           List.iter
             (fun label ->
                writef "DISPROOFWORLDLABEL % % %\n"
                  [Int cx; Int cy; Str label])
             labels;
           List.iter
             (fun (chx, chy) ->
                writef "DISPROOFWORLDCHILD % % % %\n"
                  [Int cx; Int cy; Int chx; Int chy])
             children)
        worlds;
      writef "DISPROOFWORLDSEND\n" []
    let rec menucheckbox (menu, label, cmd) =
      writef "MENUCHECKBOX % % %\n" [Str (findmenu menu); Str label; Str cmd]
    let rec menuradiobutton (menu, lcs) =
      writef "MENURADIOBUTTONSTART %\n" [Str (findmenu menu)];
      List.iter
        (fun (label, cmd) ->
           writef "MENURADIOBUTTONPART % %\n" [Str label; Str cmd])
        lcs;
      writef "MENURADIOBUTTONEND\n" []
    let rec tickmenuitem (menu, label, b) =
      writef "TICKMENUITEM % % %\n"
        [Str (findmenu menu); Str label; Int (if b then 1 else 0)]
    exception GetGeometry_
    let rec getGeometry pane =
        match askf "%PANEGEOMETRY\n" [Str pane] with
          [w; h; x; y] -> box (pos (x, y), size (w, h))
        | _ -> raise GetGeometry_
    let rec getProofPane () = getGeometry "PROOF"
    let rec getDisproofPane () = getGeometry "DISPROOF"
    let rec clearProofPane () = writef "CLEARPROOFPANE\n" []
    let rec clearDisproofPane () = writef "CLEARDISPROOFPANE\n" []
    let rec emphasise pos b =
      let (x, y) = explodePos pos in
      writef "DISPROOFEMPHASISE % % %\n"
        [Int x; Int y; Int (if b then 1 else 0)]
    let rec setproofparams displaystyle linethicknessval =
      linethickness := linethicknessval;
      writef "SETPROOFPARAMS % %\n" [Str displaystyle; Int linethicknessval]
  end


 

































