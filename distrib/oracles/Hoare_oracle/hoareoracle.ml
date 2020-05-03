open Hoareparser
open Parsetree

(* switches *)
let debugging = ref false

let strict = ref true

(* if the parser has the priority and associativity right, we don't have   *)
(* to do anything ... but what are the rules of cvc3? The manual doesn't   *)
(* say. Simplest to bracket everything!                                    *)

let string_of_list elf sep els = String.concat sep (List.map elf els)

let string_of_pair f g sep (a, b) = f a ^ sep ^ g b

let string_of_variable (Var s) = s

let rec querystring_of_formula f =
  match f with
  | Ident v -> string_of_variable v
  | Number s -> s
  | Bottom -> "FALSE"
  | Top -> "TRUE"
  | Except (a, i, e) ->
      "(" ^ querystring_of_formula a ^ " exceptat " ^ querystring_of_formula i
      ^ " read " ^ querystring_of_formula e ^ ")"
  | InfixLogic (op, l, r) ->
      "(" ^ querystring_of_formula l
      ^ ( match op with
        | Implies -> " => "
        | Iff -> " <=> "
        | And -> " AND "
        | Or -> " OR " )
      ^ querystring_of_formula r ^ ")"
  | Bind (b, v, f) ->
      "("
      ^ (match b with All -> "FORALL " | Exists -> "EXISTS ")
      ^ "("
      ^ (match v with Var s -> s)
      ^ ": INT): " ^ "(" ^ querystring_of_formula f ^ "))"
  | Compare (op, l, r) -> (
      let infix op =
        "(" ^ querystring_of_formula l ^ " " ^ op ^ " "
        ^ querystring_of_formula r ^ ")"
      in
      match op with
      | Lt -> infix "<"
      | Gt -> infix ">"
      | Le -> infix "<="
      | Ge -> infix ">="
      | Ne -> infix "/="
      | Eq -> infix "=" )
  | InfixArith (op, l, r) -> (
      let infix op =
        "(" ^ querystring_of_formula l ^ " " ^ op ^ " "
        ^ querystring_of_formula r ^ ")"
      in
      match op with
      | Plus -> infix "+"
      | Minus -> "-"
      | Times -> "*"
      | Div -> "/"
      | Mod ->
          "(mod(" ^ querystring_of_formula l ^ "," ^ querystring_of_formula r
          ^ "))"
      | Exp -> infix "^" )
  | Not f -> "(" ^ " NOT " ^ querystring_of_formula f ^ ")"
  | Skip -> "skip"
  | Tilt -> "tilt"
  | Sequence (a, b) ->
      querystring_of_formula a ^ "; " ^ querystring_of_formula b
  | Assign (a, b) ->
      querystring_of_formula a ^ " := " ^ querystring_of_formula b
  | Choice (e, a, b) ->
      "(" ^ " IF " ^ querystring_of_formula e ^ " THEN ("
      ^ querystring_of_formula a ^ ") ELSE (" ^ querystring_of_formula b ^ ")"
      ^ ")"
  | Repetition (e, a) ->
      "(" ^ " WHILE " ^ querystring_of_formula e ^ " DO ("
      ^ querystring_of_formula a ^ ")" ^ ")"
  | Juxtaposition (a, (Subscript _ as sub)) ->
      (* we don't have to tuple this, says cvc3 -- but we do have to bracket the formula sometimes, see below *)
      querystring_of_formula a ^ querystring_of_formula sub
  | Juxtaposition (a, b) ->
      (* we have to translate this into a tupled application *)
      let rec tupled args f =
        match f with
        | Juxtaposition (f, a) -> tupled (a :: args) f
        | _ -> (f, args)
      in
      let f, args = tupled [] f in
      "(" ^ querystring_of_formula f ^ "("
      ^ string_of_list querystring_of_formula "," args
      ^ ") )"
  | Substitution (a, vs, es) ->
      "(" ^ querystring_of_formula a ^ " WITH "
      ^ string_of_list string_of_variable ", " vs
      ^ " FOR "
      ^ string_of_list querystring_of_formula ", " es
      ^ ")"
  | Bra fs -> "(" ^ string_of_list querystring_of_formula ", " fs ^ ")"
  | Subscript fs ->
      "["
      ^ ( match fs with
        | [ f ] -> querystring_of_formula f
        | _ -> "(" ^ string_of_list querystring_of_formula ", " fs ^ ")" )
      ^ "]"
  | PrePost f -> "{" ^ querystring_of_formula f ^ "}"

let querystring_of_antecedent a =
  match a with
  | Gamma -> "Gamma"
  | Integer v -> ""
  | Assert f -> "ASSERT " ^ querystring_of_formula f ^ ";"

let querystring_of_sequent s =
  match s with
  | Sequent (antecedents, conclusion) ->
      String.concat "\n" (List.map querystring_of_antecedent antecedents)
      ^ "\n"
      ^ ("QUERY " ^ querystring_of_formula conclusion)
      ^ ";\n"

let japestring_of_token t =
  match t with
  | GAMMA -> "Γ"
  | COMMA -> ","
  | STILE -> "⊢"
  | BRA -> "("
  | KET -> ")"
  | SQBRA -> "["
  | SQKET -> "]"
  | CURLYBRA -> "{"
  | CURLYKET -> "}"
  | SUBSTBRA -> "«"
  | SUBSTSEP -> "/"
  | SUBSTKET -> "»"
  | BOTTOM -> "⊥"
  | TOP -> "⊤"
  | BECOMES -> ":="
  | SEMICOLON -> ";"
  | EXCEPTAT -> "⊕"
  | MAPSTO -> "↦"
  | IMPLIES -> "→"
  | IFF -> "↔"
  | NOT -> "¬"
  | OR -> "∨"
  | AND -> "∧"
  | BIND All -> "∀"
  | BIND Exists -> "∃"
  | DOT -> "."
  | COMPARE Lt -> "<"
  | COMPARE Gt -> ">"
  | COMPARE Le -> "≤"
  | COMPARE Ge -> "≥"
  | COMPARE Ne -> "≠"
  | COMPARE Eq -> "="
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "×"
  | DIV -> "÷"
  | EXP -> "↑"
  | MOD -> "mod"
  | INTEGER -> "integer"
  | IFBRA -> "if"
  | THENSEP -> "then"
  | ELSESEP -> "else"
  | FIKET -> "fi"
  | WHILEBRA -> "while"
  | DOSEP -> "do"
  | ODKET -> "od"
  | SKIP -> "skip"
  | TILT -> "tilt"
  | NUMBER s -> s
  | IDENT s -> s
  | EOL -> "\n"

(* If I have the precedences right, I shouldn't need to generate any       *)
(* additional brackets Spaces, on the other hand ...                       *)

let rec japestring_of_formula f =
  match f with
  | Ident (Var s) -> s
  | Number s -> s
  | Bottom -> japestring_of_token BOTTOM
  | Top -> japestring_of_token TOP
  | Except (a, i, e) ->
      japestring_of_formula a
      ^ japestring_of_token EXCEPTAT
      ^ japestring_of_formula i ^ japestring_of_token MAPSTO
      ^ japestring_of_formula e ^ ")"
  | InfixLogic (op, l, r) ->
      japestring_of_formula l
      ^ ( match op with
        | Implies -> japestring_of_token IMPLIES
        | Iff -> japestring_of_token IFF
        | And -> japestring_of_token AND
        | Or -> japestring_of_token OR )
      ^ japestring_of_formula r
  | Bind (b, v, f) ->
      japestring_of_token (BIND b)
      ^ (match v with Var s -> s)
      ^ japestring_of_token DOT ^ japestring_of_formula f
  | Compare (op, l, r) ->
      japestring_of_formula l
      ^ japestring_of_token (COMPARE op)
      ^ japestring_of_formula r
  | InfixArith (op, l, r) ->
      japestring_of_formula l
      ^ ( match op with
        | Plus -> japestring_of_token PLUS
        | Minus -> japestring_of_token MINUS
        | Times -> japestring_of_token TIMES
        | Div -> japestring_of_token DIV
        | Mod -> " " ^ japestring_of_token MOD ^ " "
        | Exp -> japestring_of_token EXP )
      ^ japestring_of_formula r
  | Not f -> japestring_of_token NOT ^ japestring_of_formula f
  | Skip -> japestring_of_token SKIP
  | Tilt -> japestring_of_token TILT
  | Sequence (a, b) ->
      japestring_of_formula a
      ^ japestring_of_token SEMICOLON
      ^ japestring_of_formula b
  | Assign (a, b) ->
      japestring_of_formula a
      ^ japestring_of_token BECOMES
      ^ japestring_of_formula b
  | Choice (e, a, b) ->
      " " ^ japestring_of_token IFBRA ^ " " ^ japestring_of_formula e ^ " "
      ^ japestring_of_token THENSEP
      ^ " " ^ japestring_of_formula a ^ " "
      ^ japestring_of_token ELSESEP
      ^ " " ^ japestring_of_formula b ^ " " ^ japestring_of_token FIKET ^ " "
  | Repetition (e, a) ->
      " "
      ^ japestring_of_token WHILEBRA
      ^ " " ^ japestring_of_formula e ^ " " ^ japestring_of_token DOSEP ^ " "
      ^ japestring_of_formula a ^ " " ^ japestring_of_token ODKET ^ " "
  | Juxtaposition (a, b) ->
      japestring_of_formula a ^ " " ^ japestring_of_formula b
  | Substitution (a, vs, es) ->
      japestring_of_formula a
      ^ japestring_of_token SUBSTBRA
      ^ string_of_list string_of_variable ", " vs
      ^ japestring_of_token SUBSTSEP
      ^ string_of_list japestring_of_formula ", " es
      ^ japestring_of_token SUBSTKET
  | Bra fs ->
      japestring_of_token BRA
      ^ string_of_list japestring_of_formula ", " fs
      ^ japestring_of_token KET
  | Subscript fs ->
      japestring_of_token SQBRA
      ^ string_of_list japestring_of_formula ", " fs
      ^ japestring_of_token SQKET
  | PrePost f ->
      japestring_of_token CURLYBRA
      ^ japestring_of_formula f
      ^ japestring_of_token CURLYKET

let japestring_of_antecedent a =
  match a with
  | Gamma -> "Gamma"
  | Integer v -> "integer " ^ string_of_variable v
  | Assert f -> "ASSERT " ^ japestring_of_formula f

let japestring_of_sequent s =
  match s with
  | Sequent (antecedents, conclusion) ->
      List.append
        (List.map japestring_of_antecedent antecedents)
        [ "QUERY " ^ japestring_of_formula conclusion ]

let filterundesirables = false

(* a not-very-efficient mapping system, from Bernard via Jape *)
type ('a, 'b) mapping = ('a * 'b) list

let empty = []

let isempty xs = xs = []

let rec ( |-> ) a b = [ (a, b) ]

let rec ( ++ ) m n = n @ m

let rec mapped same mapping a =
  let rec ff = function
    | [] -> None
    | (x, y) :: mapping -> if same (x, a) then Some y else ff mapping
  in
  ff mapping

(* -- [x] is the inverse of ++ x |-> : that is, it deletes only the        *)
(* outermost value of x. * -- [x,x] deletes two, and so on.                *)
let rec listsub eq xs ys =
  let rec sf a1 a2 =
    match (a1, a2) with
    | [], ys -> []
    | xs, [] -> xs
    | xs, y :: ys ->
        let rec strip = function
          | [] -> []
          | x :: xs -> if eq (x, y) then xs else x :: strip xs
        in
        sf (strip xs) ys
  in
  sf xs ys

let rec ( -- ) xs ys =
  match (xs, ys) with
  | [], ys -> []
  | (x, xv) :: ps, ys ->
      if List.mem x ys then ps -- listsub (fun (x, y) -> x = y) ys [ x ]
      else (x, xv) :: (ps -- ys)

let rec ( <@> ) mapping a = mapped (fun (x, y) -> x = y) mapping a

(* eta-conversion doesn't work here *)

let string_of_mapping fa fb m =
  string_of_list (string_of_pair fa fb "|->") " ++ " m

(* types are setup to be unifiable. The type environment maps unknowns to  *)
(* types. We could do this with refs but I get confused                    *)

exception TypeError of string

exception Inappropriate of string

type ftype =
  | Unknown of int (* unique unknown id *)
  | Int
  | Bool
  | Command
  | Arrow of ftype * ftype
  | Array of ftype * ftype
  | Tuple of ftype list

let rec typeclass te t =
  match t with
  | Unknown u -> (
      match te <@> u with Some t' -> typeclass te t' | None -> t )
  | _ -> t

let rec string_of_ftype te t =
  let sft = string_of_ftype te in
  match typeclass te t with
  | Unknown i -> "Unknown " ^ string_of_int i
  | Int -> "Int"
  | Bool -> "Bool"
  | Command -> "Command"
  | Arrow (a, r) -> "(" ^ sft a ^ ")->" ^ sft r
  | Array (i, e) -> "array[" ^ sft i ^ "] of " ^ sft e
  | Tuple ts -> "Tuple[" ^ String.concat "; " (List.map sft ts) ^ "]"

let string_of_typeenv = string_of_mapping string_of_int (string_of_ftype [])

let string_of_symboltable te =
  string_of_mapping (fun s -> s) (string_of_ftype te)

let rec unify te t1 t2 =
  if !debugging then
    print_string
      ( "unifying " ^ string_of_ftype te t1 ^ " with " ^ string_of_ftype te t2
      ^ "\n" )
  else ();
  let nounify () =
    raise
      (TypeError
         ( "Can't unify types " ^ string_of_ftype te t1 ^ " and "
         ^ string_of_ftype te t2 ))
  in
  match (typeclass te t1, typeclass te t2) with
  | Unknown u1, (Unknown u2 as t2) -> if u1 = u2 then te else te ++ (u1 |-> t2)
  | Unknown u1, t2 -> te ++ (u1 |-> t2)
  | t1, Unknown u2 -> te ++ (u2 |-> t1)
  | Int, Int -> te
  | Bool, Bool -> te
  | Command, Command -> te
  | Arrow (at1, rt1), Arrow (at2, rt2) ->
      let te' = unify te at1 at2 in
      unify te' rt1 rt2
  | Array (it1, et1), Array (it2, et2) ->
      let te' = unify te it1 it2 in
      unify te' et1 et2
  | Tuple t1s, Tuple t2s -> (
      try unify_lists te t1s t2s with _ -> nounify () )
  | t1, t2 -> nounify ()

and unify_lists te t1s t2s =
  match (t1s, t2s) with
  | [], [] -> te
  | t1 :: t1s, t2 :: t2s -> unify_lists (unify te t1 t2) t1s t2s
  | _ -> raise (TypeError "incompatible lengths of type lists")

let unknowns = ref 0

let newUnknown () =
  unknowns := !unknowns + 1;
  Unknown !unknowns

let lookup st s =
  match st <@> s with
  | Some t -> (st, t)
  | None ->
      let t = newUnknown () in
      (st ++ (s |-> t), t)

let modfacts = ref false (* true when we need to say stuff about mod to cvc3 *)

let rec typecheck_formula (st, te, t) f =
  let override (st, te, _) t = (st, te, t) in
  let inappropriate s =
    raise (Inappropriate ("Can't handle " ^ s ^ " " ^ japestring_of_formula f))
  in
  if !debugging then
    print_string
      ( "typechecking " ^ japestring_of_formula f ^ ":" ^ string_of_ftype te t
      ^ "\n" )
  else ();
  match f with
  | Ident (Var s) ->
      let st', tv = lookup st s in
      let te' = unify te tv t in
      (st', te', typeclass te' t)
  | Number _ -> (st, unify te t Int, Int)
  | Bottom -> (st, unify te t Bool, Bool)
  | Top -> (st, unify te t Bool, Bool)
  | Skip ->
      if !strict then inappropriate "command"
      else (st, unify te t Command, Command)
  | Tilt ->
      if !strict then inappropriate "command"
      else (st, unify te t Command, Command)
  | Not e ->
      let te' = unify te t Bool in
      typecheck_formula (st, te', Bool) e
  | InfixLogic (_, a, b) ->
      let te' = unify te t Bool in
      List.fold_left typecheck_formula (st, te', Bool) [ a; b ]
  | Bind (_, Var s, e) ->
      if !strict then inappropriate "quantification"
      else
        let te' = unify te t Bool in
        (* we only quantify over integers in Jape *)
        let st' = st ++ (s |-> Int) in
        let st'', te'', t'' = typecheck_formula (st', te', Bool) e in
        (* we've lost the type of the binder after this *)
        (st'' -- [ s ], te'', t'')
  | Compare (_, a, b) ->
      let te' = unify te t Bool in
      override (List.fold_left typecheck_formula (st, te', Int) [ a; b ]) Bool
  | InfixArith (op, a, b) -> (
      let te' = unify te t Int in
      let doargs st te =
        List.fold_left typecheck_formula (st, te, Int) [ a; b ]
      in
      match op with
      | Mod ->
          let st', tm = lookup st "mod" in
          let te'' = unify te' tm (Arrow (Tuple [ Int; Int ], Int)) in
          modfacts := true;
          doargs st' te''
      | _ -> doargs st te' )
  | Sequence (a, b) ->
      if !strict then inappropriate "command"
      else
        let te' = unify te t Command in
        List.fold_left typecheck_formula (st, te', Command) [ a; b ]
  | Assign (a, b) ->
      if !strict then inappropriate "command"
      else
        let te' = unify te t Command in
        List.fold_left typecheck_formula (st, te', newUnknown ()) [ a; b ]
  | Choice (e, a, b) ->
      if !strict then inappropriate "command"
      else
        let te' = unify te t Command in
        let st'', te'', _ = typecheck_formula (st, te', Bool) e in
        List.fold_left typecheck_formula (st'', te'', Command) [ a; b ]
  | Repetition (e, a) ->
      if !strict then inappropriate "command"
      else
        let te' = unify te t Command in
        let st'', te'', _ = typecheck_formula (st, te', Bool) e in
        typecheck_formula (st'', te'', Command) a
  | Juxtaposition (Juxtaposition (left, c), PrePost post) ->
      (* this is not an application! *)
      if !strict then inappropriate "command"
      else
        let st', te', _ = typecheck_formula (st, unify te t Bool, Bool) post in
        let st'', te'', _ = typecheck_formula (st', te', Command) c in
        typecheck_formula (st'', te'', Bool) left
  | Juxtaposition (PrePost pre, c) ->
      if !strict then inappropriate "command"
      else
        let st', te', _ = typecheck_formula (st, unify te t Bool, Bool) pre in
        typecheck_formula (st', te', Command) c
  | Juxtaposition (a, Subscript subs) ->
      let st', te', subt = typecheck_subscripts st te subs in
      let st'', te'', _ = typecheck_formula (st', te', Array (subt, t)) a in
      (st'', te'', t)
  | Juxtaposition (f, a) ->
      let ta, tr = (newUnknown (), t) in
      let st', te', _ = typecheck_formula (st, te, Arrow (ta, tr)) f in
      let st'', te'', _ = typecheck_formula (st', te', ta) a in
      (st'', te'', tr)
  | Substitution (a, vs, es) ->
      if !strict then inappropriate "substitution"
      else
        let rec checkes st te vs es =
          match (vs, es) with
          | [], [] -> (st, te)
          | Var s :: vs, e :: es ->
              let st', te', t = typecheck_formula (st, te, newUnknown ()) e in
              let st'', te'' = checkes st' te' vs es in
              (st'' ++ (s |-> t), te'')
          | _ ->
              inappropriate
                "malformed substitution -- number of names doesn't match \
                 number of formulae"
        in
        let rec stripvs st vs =
          match vs with
          | [] -> st
          | Var s :: vs ->
              let st' = stripvs st vs in
              st' -- [ s ]
        in
        let st', te' = checkes st te vs es in
        let st'', te'', t = typecheck_formula (st', te', t) a in
        (stripvs st vs, te'', t)
  | Bra es -> (
      match es with
      | [ e ] -> typecheck_formula (st, te, t) e
      | [] -> inappropriate "empty bracketed formula"
      | _ ->
          let st', te', ts = typecheck_formulalist (st, te) es in
          (st', te', Tuple ts) )
  | PrePost e ->
      if !strict then inappropriate "Hoare pre/postcondition"
      else
        let te' = unify te t Bool in
        typecheck_formula (st, te', Bool) e
  | Except (a, i, e) ->
      if !strict then inappropriate "array exception formula"
      else
        let te' = unify te t Int in
        List.fold_left typecheck_formula (st, te', Int) [ a; i; e ]
  | Subscript subs -> typecheck_subscripts st te subs

and typecheck_formulalist (st, te) es =
  match es with
  | [] -> (st, te, [])
  | e :: es ->
      let st', te', t = typecheck_formula (st, te, newUnknown ()) e in
      let st'', te'', ts = typecheck_formulalist (st', te') es in
      (st'', te'', t :: ts)

and typecheck_subscripts st te subs =
  let st', te', subts = typecheck_formulalist (st, te) subs in
  ( st',
    List.fold_left (fun te t -> unify te Int t) te' subts,
    (* arrays have to be subscripted by Int, surely *)
    match subts with [ subt ] -> subt | _ -> Tuple subts )

let typecheck_antecedent (st, te) a =
  let nogo () =
    raise
      (Inappropriate ("Can't deal with antecedent " ^ japestring_of_antecedent a))
  in
  match a with
  | Gamma -> nogo ()
  | Integer (Var s) ->
      let st', tv = lookup st s in
      let te' = unify te tv Int in
      (st', te')
  | Assert f ->
      let st', te', _ = typecheck_formula (st, te, Bool) f in
      (st', te')

let typecheck_sequent (st, te) (Sequent (antes, conseq)) =
  let st', te' = List.fold_left typecheck_antecedent (st, te) antes in
  typecheck_formula (st', te', Bool) conseq

let _ =
  let tree =
    try
      let lexbuf = Lexing.from_channel stdin in
      let result = Hoareparser.sequent Hoarelexer.token lexbuf in
      result
    with _ ->
      print_string "ParseError\n";
      exit 0
  in
  try
    let st, te, _ = typecheck_sequent (empty, empty) tree in
    let rec declare = function
      | [] -> ()
      | (s, t) :: st ->
          let rec showtype t =
            match typeclass te t with
            | Unknown _ ->
                raise
                  (TypeError
                     ( "Can't discover type of variable " ^ s
                     ^
                     if !debugging then
                       ": " ^ string_of_ftype te t ^ ": " ^ string_of_typeenv te
                     else "" ))
            | Int -> "INT"
            | Bool -> "BOOL"
            | Command ->
                raise (Inappropriate ("Variable " ^ s ^ " has type 'command'"))
            | Arrow (argt, rest) ->
                (* cvc3 demands tupled arguments *)
                let rec tupled argts t =
                  match typeclass te t with
                  | Arrow (t, t') -> tupled (t :: argts) t'
                  | t' -> showtype (Tuple (List.rev argts)) ^ "->" ^ showtype t'
                in
                tupled [] t
            | Array (subt, elt) -> (
                (* this doesn't have to be tupled, says cvc3 - but I think we have to bracket the elt sometimes  *)
                "ARRAY " ^ showtype subt ^ " OF "
                ^
                match elt with
                | Array _ -> "(" ^ showtype elt ^ ")"
                | _ -> showtype elt )
            | Tuple ts ->
                (* square brackets, it seems *)
                "[" ^ String.concat "," (List.map showtype ts) ^ "]"
          in
          print_string (s ^ ":" ^ showtype t ^ ";\n");
          declare st
    in
    declare st;
    if !modfacts then
      print_string
        "ASSERT FORALL (x: INT): mod(x, 1) = 0;\n\
         ASSERT FORALL (x: INT): x /= 0 => mod(x, x) = 0;\n\
         ASSERT FORALL (x: INT, n: INT): x > 0 => ( 0 <= mod(n, x) AND mod(n, \
         x) < x );\n"
    else ();
    print_string (querystring_of_sequent tree)
  with
  | TypeError s ->
      print_string ("TypeError\n" ^ s ^ "\n");
      exit 0
  | Inappropriate s ->
      print_string ("Inaproppriate\n" ^ s ^ "\n");
      exit 0
