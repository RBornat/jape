/* $Id$ */

CONCHIT λx.E:T IS "λx.E : T1→T2"
CONCHIT x:T IS "x:T"
CONCHIT F G:T IS "F G : T"
CONCHIT (E,F):T IS "(E,F) : T1×T2"
CONCHIT n:T IS "n:num"
CONCHIT s:T IS "s:string"
CONCHIT true:T IS "true:bool"
CONCHIT false:T IS "false:bool"
CONCHIT letrec x=E in F end:T IS letrecrules
CONCHIT let x=E in F end:T IS letrules
CONCHIT if E then ET else EF fi:T  IS "if E then ET else EF fi : T"
CONCHIT T«S IS generalise

