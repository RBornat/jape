/* $Id$ */ 

CLASS VARIABLE a b c d e f g h i j k l m n o p q r s t u v w x y z 
CLASS FORMULA A B C D E F G H I J L M N O P Q R S T U V W X Y Z
CLASS CONSTANT K
CLASS BAG FORMULA Γ
        
CONSTANT true false
CONSTANT ⊥ /* to satisfy I2L syntax */

INFIX   5L  ≜

PREFIX  10  actual integer /* actual not used, but we have to satisfy PUSHSYNTAX */

INFIX 10 L ;
INFIX 12 L :=

INFIX 50 L ⊕
INFIX 60 L ↦

INFIX   100R    → ↔
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
