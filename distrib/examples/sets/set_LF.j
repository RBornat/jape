/* $Id$ */

/* stuff to add LF vars to the naive set theory encoding */

USE "BnE-Fprime_LF.j" 

RULE "§-I"(OBJECT c) WHERE FRESH c IS FROM new c, cÚA æ cÚB INFER A§B
