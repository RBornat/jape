/* $Id$ */

SEQUENT IS BAG ⊢ FORMULA

CLASS VARIABLE w x y z c d e f
CLASS FORMULA A B C D E F P Q R S
CONSTANT ⊥

SUBSTFIX 2000
JUXTFIX  1900

PREFIX     450     ¬
INFIX      400L    = ≠
INFIX      300L    ∧
INFIX      200L    ∨
INFIX      100R    → ↔
INFIX       50L    ≜

LEFTFIX     10  ∀ .
LEFTFIX     10  ∃ .
LEFTFIX     10  ∃! .

PREFIX      5   var
POSTFIX     5   inscope

BIND x SCOPE P IN ∀ x . P
BIND x y SCOPE P IN ∀ (x,y) . P
BIND x y z SCOPE P IN ∀ (x,y,z) . P
BIND w x y z SCOPE P IN ∀ (w,x,y,z) . P

BIND x SCOPE P IN ∃ x . P
BIND x y SCOPE P IN ∃ (x,y) . P
BIND x y z SCOPE P IN ∃ (x,y,z) . P
BIND w x y z SCOPE P IN ∃ (w,x,y,z) . P

BIND x SCOPE P IN ∃! x . P
BIND x y SCOPE P IN ∃! (x,y) . P
BIND x y z SCOPE P IN ∃! (x,y,z) . P
BIND w x y z SCOPE P IN ∃! (w,x,y,z) . P

INITIALISE autoAdditiveLeft true /* avoid explicit statement of left context */
