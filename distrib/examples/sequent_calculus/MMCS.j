/* $Id$ */

INITIALISE applyconjectures false
INITIALISE displaystyle tree

USE "MMCS_rules.j"

CONJECTUREPANEL "Conjectures"
  THEOREM	modusponens(A,B)	IS A, AçB æ B
  THEOREM	contradiction(A,B)	IS A, ÂA æ 
END

USE "MCS+SCS_problems.j"
