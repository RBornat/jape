/* $Id$ */

CLASS VARIABLE x y z c d
CLASS FORMULA A B C P Q R S
CLASS BAG FORMULA Γ

PREFIX	10		var
POSTFIX	10		inscope

INFIX		100R	→
INFIX		120L		∨
INFIX		140L		∧

LEFTFIX	180		∀ .
LEFTFIX	180		∃ .

PREFIX	200		¬
JUXTFIX	300
SUBSTFIX	400 

BIND x SCOPE P IN ∀x . P
BIND x SCOPE P IN ∃x . P

SEQUENT IS BAG ⊢ FORMULA

INITIALISE autoAdditiveLeft	true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates	true /* allow predicate syntax ... */
