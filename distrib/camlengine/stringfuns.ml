(* $Id$ *)

module type T =
  sig
    val isQuoted : string -> bool
    val unQuote : string -> string
    val enQuote : string -> string
    val words : string -> string list
    val respace : string list -> string
    val lowercase : string -> string
    val uppercase : string -> string
    val pairstring :
      ('a -> string) -> ('b -> string) -> string -> 'a * 'b -> string
    val triplestring :
      ('a -> string) -> ('b -> string) -> ('c -> string) -> string ->
        'a * 'b * 'c -> string
    val quadruplestring :
      ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
        string -> 'a * 'b * 'c * 'd -> string
    val quintuplestring :
      ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
        ('e -> string) -> string -> 'a * 'b * 'c * 'd * 'e -> string
    val sextuplestring :
      ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
        ('e -> string) -> ('f -> string) -> string ->
        'a * 'b * 'c * 'd * 'e * 'f -> string
    val septuplestring :
      ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
        ('e -> string) -> ('f -> string) -> ('g -> string) -> string ->
        'a * 'b * 'c * 'd * 'e * 'f * 'g -> string
    val octuplestring :
      ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
        ('e -> string) -> ('f -> string) -> ('g -> string) ->
        ('h -> string) -> string -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h ->
        string
    val catelim_pairstring :
      ('a -> string list -> string list) ->
        ('b -> string list -> string list) -> string -> 'a * 'b ->
        string list -> string list
    val catelim_triplestring :
      ('a -> string list -> string list) ->
        ('b -> string list -> string list) ->
        ('c -> string list -> string list) -> string -> 'a * 'b * 'c ->
        string list -> string list
    val catelim_quadruplestring :
      ('a -> string list -> string list) ->
        ('b -> string list -> string list) ->
        ('c -> string list -> string list) ->
        ('d -> string list -> string list) -> string -> 'a * 'b * 'c * 'd ->
        string list -> string list
    val catelim_quintuplestring :
      ('a -> string list -> string list) ->
        ('b -> string list -> string list) ->
        ('c -> string list -> string list) ->
        ('d -> string list -> string list) ->
        ('e -> string list -> string list) -> string ->
        'a * 'b * 'c * 'd * 'e -> string list -> string list
    val catelim_sextuplestring :
      ('a -> string list -> string list) ->
        ('b -> string list -> string list) ->
        ('c -> string list -> string list) ->
        ('d -> string list -> string list) ->
        ('e -> string list -> string list) ->
        ('f -> string list -> string list) -> string ->
        'a * 'b * 'c * 'd * 'e * 'f -> string list -> string list
    val catelim_septuplestring :
      ('a -> string list -> string list) ->
        ('b -> string list -> string list) ->
        ('c -> string list -> string list) ->
        ('d -> string list -> string list) ->
        ('e -> string list -> string list) ->
        ('f -> string list -> string list) ->
        ('g -> string list -> string list) -> string ->
        'a * 'b * 'c * 'd * 'e * 'f * 'g -> string list -> string list
    val catelim_octuplestring :
      ('a -> string list -> string list) ->
        ('b -> string list -> string list) ->
        ('c -> string list -> string list) ->
        ('d -> string list -> string list) ->
        ('e -> string list -> string list) ->
        ('f -> string list -> string list) ->
        ('g -> string list -> string list) ->
        ('h -> string list -> string list) -> string ->
        'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> string list -> string list
    val catelim_arraystring :
      ('a -> string list -> string list) -> string -> 'a array ->
        string list -> string list
    val arraystring : ('a -> string) -> string -> 'a array -> string
  
    val quotedstring_of_char : char -> string
  end

module M : T =
  struct
    open Miscellaneous
    open Listfuns
    open Sml.M
    
    let rec isQuoted s =
      String.sub (s) (0) (1) = "\"" && String.sub (s) (String.length s - 1) (1) = "\""
    let rec unQuote s =
      try
        let size = String.length s in
        match String.sub (s) (0) (1) with
          "\"" ->
            begin match String.sub (s) (size - 1) (1) with
              "\"" -> String.sub (s) (1) (size - 2)
            | _ -> s
            end
        | _ -> s
      with
        _ -> s
    let rec enQuote s = ("\"" ^ s) ^ "\""
    let rec words =
      function
        "" -> []
      | s ->
          let rec wds =
            function
              [] -> [[]]
            | [' '] -> [[]]
            | ' ' :: ' ' :: cs -> wds (' ' :: cs)
            | ' ' :: cs -> [] :: wds cs
            | '"' :: cs -> let ws = qds cs in ('"' :: List.hd ws) :: List.tl ws
            | c :: cs -> let ws = wds cs in (c :: List.hd ws) :: List.tl ws
          and qds =
            function
              [] -> [[]]
            | ['"'] -> [['"']]
            | '"' :: ' ' :: cs -> qds ('"' :: cs)
            | '"' :: cs -> ['"'] :: wds cs
            | c :: cs -> let ws = qds cs in (c :: List.hd ws) :: List.tl ws
          in
          List.map char_implode (wds (char_explode s))
    let respace ws = String.concat " " ws
    
    let lowercase = String.lowercase
    let uppercase = String.uppercase

    let rec catelim_pairstring fa fb sep (a, b) tail =
      "(" :: fa a (sep :: fb b (")" :: tail))
    let rec catelim_triplestring fa fb fc sep (a, b, c) tail =
      "(" :: fa a (sep :: fb b (sep :: fc c (")" :: tail)))
    let rec catelim_quadruplestring fa fb fc fd sep (a, b, c, d) tail =
      "(" :: fa a (sep :: fb b (sep :: fc c (sep :: fd d (")" :: tail))))
    let rec catelim_quintuplestring fa fb fc fd fe sep (a, b, c, d, e) tail =
      "(" ::
        fa a
           (sep ::
              fb b (sep :: fc c (sep :: fd d (sep :: fe e (")" :: tail)))))
    let rec catelim_sextuplestring
      fa fb fc fd fe ff sep (a, b, c, d, e, f) tail =
      "(" ::
        fa a
           (sep ::
              fb b
                 (sep ::
                    fc c
                       (sep ::
                          fd d (sep :: fe e (sep :: ff f (")" :: tail))))))
    let rec catelim_septuplestring
      fa fb fc fd fe ff fg sep (a, b, c, d, e, f, g) tail =
      "(" ::
        fa a
           (sep ::
              fb b
                 (sep ::
                    fc c
                       (sep ::
                          fd d
                             (sep ::
                                fe e
                                   (sep ::
                                      ff f (sep :: fg g (")" :: tail)))))))
    let rec catelim_octuplestring
      fa fb fc fd fe ff fg fh sep (a, b, c, d, e, f, g, h) tail =
      "(" ::
        fa a
           (sep ::
              fb b
                 (sep ::
                    fc c
                       (sep ::
                          fd d
                             (sep ::
                                fe e
                                   (sep ::
                                      ff f
                                         (sep ::
                                            fg g
                                               (sep ::
                                                  fh h (")" :: tail))))))))
    let s = stringfn2catelim
    let rec pairstring fa fb sep =
      catelim2stringfn (catelim_pairstring (s fa) (s fb) sep)
    let rec triplestring fa fb fc sep =
      catelim2stringfn (catelim_triplestring (s fa) (s fb) (s fc) sep)
    let rec quadruplestring fa fb fc fd sep =
      catelim2stringfn
        (catelim_quadruplestring (s fa) (s fb) (s fc) (s fd) sep)
    let rec quintuplestring fa fb fc fd fe sep =
      catelim2stringfn
        (catelim_quintuplestring (s fa) (s fb) (s fc) (s fd) (s fe) sep)
    let rec sextuplestring fa fb fc fd fe ff sep =
      catelim2stringfn
        (catelim_sextuplestring (s fa) (s fb) (s fc) (s fd) (s fe) (s ff) sep)
    let rec septuplestring fa fb fc fd fe ff fg sep =
      catelim2stringfn
        (catelim_septuplestring (s fa) (s fb) (s fc) (s fd) (s fe) (s ff)
           (s fg) sep)
    let rec octuplestring fa fb fc fd fe ff fg fh sep =
      catelim2stringfn
        (catelim_octuplestring (s fa) (s fb) (s fc) (s fd) (s fe) (s ff)
           (s fg) (s fh) sep)
    let rec catelim_arraystring f sep a ss =
      let rec el i ss =
        if i = Array.length a then ss
        else
          let rec doit ss = string_of_int i :: ": " :: f (Array.get a i) ss in
          doit (if i = Array.length a - 1 then ss else sep :: el (i + 1) ss)
      in
      "Ç" :: el 0 ("È" :: ss)
    let rec arraystring f sep =
      catelim2stringfn (catelim_arraystring (s f) sep)
  
  	let quotedstring_of_char c = "'" ^ (Char.escaped c) ^ "'"

  end
