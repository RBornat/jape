{
    (* lexer for Jape's Hoare logic encoding *)
    open Parsetree
		open Hoareparser
 
		exception Error of char * int * int
}
let letter = ['A'-'Z' 'a'-'z']
let underscore = ['_']
let digit = ['0'-'9']
let prime = ['\'']

let idtrail = letter | digit | prime
let ident = underscore letter idtrail* | letter idtrail*
let num = digit+
let eol = ['\r' '\n' '\x85'] (* also 0x2028 and 0x2029, see below *)

let blank = [' ' '\t']

rule token = parse
  | blank+      { token lexbuf }
  
  | "Γ"         { GAMMA }
  | ","         { COMMA }
  | "⊢"         { STILE }
  
  | "("         { BRA }
  | ")"         { KET }
  | "["         { SQBRA }
  | "]"         { SQKET }
  | "{"         { CURLYBRA }
  | "}"         { CURLYKET }
  | "«"         { SUBSTBRA }
  | "/"         { SUBSTSEP }
  | "»"         { SUBSTKET }

  | "⊥"         { BOTTOM }
  | "⊤"         { TOP }
  
  | ":="        { BECOMES }
  | ";"         { SEMICOLON }
  
  | "⊕"         { EXCEPTAT }
  | "↦"         { MAPSTO }
  
  | "→"         { IMPLIES } 
  | "↔"         { IFF }
  | "¬"         { NOT }
  | "∨"         { OR }
  | "∧"         { AND }
  | "∀"         { BIND(All) }
  | "∃"         { BIND(Exists) }
  | "."         { DOT }
  
  | "<"         { COMPARE(Lt) }
  | ">"         { COMPARE(Gt) }
  | "≤"         { COMPARE(Le) }
  | "≥"         { COMPARE(Ge) }
  | "≠"         { COMPARE(Ne) }
  | "="         { COMPARE(Eq) }
  
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "×"         { TIMES }
  | "÷"         { DIV }
  | "↑"         { EXP }
  
  | "mod"       { MOD }
  | "integer"   { INTEGER }

  | "if"        { IFBRA }
  | "then"      { THENSEP }
  | "else"      { ELSESEP }
  | "fi"        { FIKET }
  
  | "while"     { WHILEBRA }
  | "do"        { DOSEP }
  | "od"        { ODKET }
  
  | "skip"      { SKIP }
  | "tilt"      { TILT }
  
  | num         { NUMBER(Lexing.lexeme lexbuf) }
  | ident       { IDENT(Lexing.lexeme lexbuf) }
  
  | eol             { EOL }
  | "\xe2\x80\xa8"  { EOL } (* Unicode line separator *)
  | "\xe2\x80\xa9"  { EOL } (* Unicode paragraph separator *)
  
  | _           { raise (Error((Lexing.lexeme lexbuf).[0],
                     Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }
  
{ (* trailer *) }
