/* $Id$ */

SEQUENT IS BAG æ FORMULA

CLASS VARIABLE w x y z c d e f
CLASS FORMULA A B C D E F P Q R S
CONSTANT Ù

SUBSTFIX    2000
JUXTFIX 1900

PREFIX     450     Â
INFIX       400L        = ­
INFIX       300L        ¦
INFIX       200L        ë
INFIX       100R        ç ê
INFIX       50L     ÷

LEFTFIX     10  è .
LEFTFIX     10  ä .
LEFTFIX     10  ä! .

PREFIX      5       var
POSTFIX     5       inscope

BIND x SCOPE P IN è x . P
BIND x y SCOPE P IN è (x,y) . P
BIND x y z SCOPE P IN è (x,y,z) . P
BIND w x y z SCOPE P IN è (w,x,y,z) . P
BIND x SCOPE P IN ä x . P
BIND x y SCOPE P IN ä (x,y) . P
BIND x y z SCOPE P IN ä (x,y,z) . P
BIND w x y z SCOPE P IN ä (w,x,y,z) . P
BIND x SCOPE P IN ä! x . P
BIND x y SCOPE P IN ä! (x,y) . P
BIND x y z SCOPE P IN ä! (x,y,z) . P
BIND w x y z SCOPE P IN ä! (w,x,y,z) . P

INITIALISE autoAdditiveLeft true /* avoid explicit statement of left context */
