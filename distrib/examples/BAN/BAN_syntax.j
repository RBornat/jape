/* $Id$ */

PREFIX #
POSTFIX ø

INFIX 300L Ÿ,  Ø, ê
INFIX 200R •
INFIX 150R š
INFIX 100R Š
INFIX 50L ‘
INFIX 10L +

OUTFIX {  }
OUTFIX <  >

LEFTFIX è .

CLASS VARIABLE x, k
CLASS FORMULA X, Y, Z
CLASS CONSTANT P, Q, R, K, N, T
CONSTANT A, B, S

BIND x SCOPE P IN èx . P

SEQUENT IS BAG æ FORMULA

INITIALISE autoAdditiveLeft true /* allow rules to be stated without an explicit left context */
