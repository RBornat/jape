/* $Id$ */

CLASS VARIABLE x k
CLASS FORMULA W X Y Z
CLASS CONSTANT P Q R K N T
CONSTANT A B S


SUBSTFIX	700
JUXTFIX		600
PREFIX 		500		#
POSTFIX 	500		ø
INFIX			300L		Ÿ  Ø ê
INFIX			200R		•
INFIX			150R		š
LEFTFIX 		110		è .
INFIX			100R		Š
INFIX			50L		‘

OUTFIX {  }
OUTFIX <  >


BIND x SCOPE P IN èx . P

SEQUENT IS BAG æ FORMULA

INITIALISE autoAdditiveLeft true /* allow rules to be stated without an explicit left context */
