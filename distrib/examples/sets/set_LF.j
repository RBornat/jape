/* $Id$ */

/* stuff to add LF vars to the naive set theory encoding */

USE "BnE-Fprime_LF.j" 

RULE "§-I"(OBJECT c) WHERE FRESH c IS FROM var c, cÚA æ cÚB INFER A§B
