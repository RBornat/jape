/* $Id$ */

/* All the things we need to play around with substitution of equals for equals */

INITIALISE autoAdditiveLeft true /* avoid explicit statement of left context */

CLASS FORMULA A B C D E
CLASS VARIABLE x

INFIX		100L ≜

INFIX		300L = ≠

JUXTFIX		9000

SUBSTFIX	10000 « E / x  » 

