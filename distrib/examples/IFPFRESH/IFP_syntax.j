/*
        $Id: IFP_syntax.j 647 2014-09-30 16:51:20Z sufrin $
*/

CLASS VARIABLE  x y z
                i j k
CLASS FORMULA   A B C D  
                E F G H  
                P Q 
                R S T
CLASS BAG FORMULA Γ

CONSTANT ⊥ ⊤

PREFIX 10 fresh term

INFIX   100R    ↔ 
INFIX   100R    →
INFIX   120L    ∨
INFIX   140L    ∧

PREFIX  200 ¬

LEFTFIX 200 ∀ .
LEFTFIX 200 ∃ .

INFIX 500T =

JUXTFIX 3000
SUBSTFIX    4000 

BIND x SCOPE A IN ∀x . A
BIND x SCOPE B IN ∃x . B

SEQUENT IS BAG ⊢ FORMULA

INITIALISE autoAdditiveLeft     true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates  true /* allow predicate syntax ... */








