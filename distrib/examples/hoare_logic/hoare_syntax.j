/*
    $Id$

    Copyright (C) 2004 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the jape examples distribution, which is part of jape.

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

CLASS VARIABLE a b c d e f g h i j k l m n o p q r s t u v w x y z 
CLASS FORMULA A B C D E F G H I J L M N O P Q R S T U V W X Y Z
CLASS CONSTANT K
CLASS BAG FORMULA Γ
        
CONSTANT true false mod
CONSTANT ⊥ /* to satisfy I2L syntax */

INFIX   5L  ≜ /* equals def */
INFIX   5L  simplifiesto equivto /* see hoare_arith.j */

PREFIX  10  actual integer /* actual not used, but we have to satisfy PUSHSYNTAX */
POSTFIX 10  defined

INFIX 10 L ;
INFIX 12 L :=

INFIX 50 L ⊕ /* circle plus */
INFIX 60 L ↦ /* maps to */

INFIX   100R    → ↔ /* implies, iff */
INFIX   120L    ∨
INFIX   140L    ∧

LEFTFIX 180 ∀ .
LEFTFIX 180 ∃ .

INFIX   300L    <   >   ≤   ≥   ≠   =  

INFIX   400 L   + -
INFIX   410 L   × ÷
INFIX   420 R   ↑

PREFIX  1200    ¬

JUXTFIX     9000
SUBSTFIX    10000   « E / x  » /* so that { }, [ ] are available for other uses */

BIND x SCOPE A IN ∀x . A
BIND x SCOPE A IN ∃x . A

SEQUENT IS BAG ⊢ FORMULA

INITIALISE autoAdditiveLeft true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates true /* allow predicate syntax ... */

OUTFIX { } /* for assertions */
OUTFIX [ ] /* for indexing */

OUTFIX if then else fi
OUTFIX while do od

CONSTANT skip tilt

INITIALISE hidetransitivity true

KEYBOARD → ↔ ∧ ∨ ¬ ⊥ ∀ ∃ ⊢ ⊧ ≤ ≥ ≠ ≜ × ÷ ↦ ⊕ « » 
INITIALISE hidereflexivity true
