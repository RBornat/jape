/*
    $Id$

    Copyright (C) 2004-5 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the Hoare logic example distribution, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*/

RULE "skip" IS {A} skip {A}

RULE "tilt" IS {⊥} tilt {A}

RULE sequence(C) IS FROM {A} F {B} AND  {B} G {C} INFER  {A} (F;G) {C}

/* RULES "Ntuple" ARE 
    FROM {A} B {C} AND {C} D {E} INFER  {A} B {C} D {E} AND
    FROM {A} B {C} D {E} AND {E} F {G} INFER  {A} B {C} D {E} F {G} AND
    FROM {A} B {C} D {E} F {G} AND {G} H {I} INFER  {A} B {C} D {E} F {G} H {I} AND
    FROM {A} B {C} D {E} F {G} H {I} AND {I} J {K} INFER  {A} B {C} D {E} F {G} H {I}  J {K}
END */

/* Since triples are parsed as juxtapositions (sigh!) this works */

RULES "Ntuple" ARE
    FROM A {B} AND {B} C {D} INFER A {B} C {D}
AND FROM {A}B{C} AND {C}D{E} INFER {A}(B{C}D){E}
END

RULE "choice" IS FROM (E→A)∧(¬E→B)∧(E computes) simplifiesto G 
                  AND {A} F1 {C} 
                  AND {B} F2 {C} 
                INFER {G} if E then F1 else F2 fi {C}

RULE "variable-assignment" IS FROM R«E/x»∧(E computes) simplifiesto Q
                              INFER {Q} (x:=E) {R}

RULE "array-element-assignment" IS 
     FROM B«a⊕E↦F/a»∧(a[E] computes) simplifiesto C 
      AND C∧(F computes) simplifiesto D
    INFER {D} (a[E]:=F) {B}

RULE "while"(I, M, OBJECT Km) WHERE FRESH Km AND Km NOTIN M IS
     FROM ⊤∧(E computes) simplifiesto G
      AND I→G 
      AND {I∧E} F {I}
      AND I∧E→M>0
      AND integer Km  ⊢ {I∧E∧M=Km} F {M<Km}
    INFER { I } while E do F od {I∧¬E}

RULE "consequence(L)" IS FROM A→B AND {B} F {C} INFER {A} F {C}

RULE "consequence(R)" IS FROM {A} F {B} AND B→C INFER {A} F {C}

/* to help with arithmetic */
RULE "obviously0" IS INFER A 
RULE "obviously1" IS FROM A INFER B 
RULE "obviously2" IS FROM A AND B INFER C 
RULE "obviously3" IS FROM A AND B AND C INFER D 
RULE "obviously4" IS FROM A AND B AND C AND D INFER E 


