/* $Id$ */

/* syntax of very simple set theory, for QMW IDS course 1997 */

CLASS VARIABLE u v w
CONSTANT Ø ⊥ U EQ

PREFIX  1000            Pow
PREFIX  800             ∪∪ ∩∩
POSTFIX 800             ⁻¹
INFIX           700L            ∪ ∩ -
INFIX           720L            •
INFIX           740L            ×
INFIX           600L            ⊆
INFIX           500L            ∈ ¬∈
/* highest priority PREFIX 450 ¬ in BnE-Fprime_syntax.j */

OUTFIX < >
OUTFIX { }
OUTFIX { | }

BIND y SCOPE P IN { y | P }
BIND x y SCOPE P IN { <x,y> | P }
