/* $Id$ */

TACTIC TheoremForwardOrBackward(thm) IS
  WHEN (LETHYP _P cut (WITHSELECTIONS thm)) thm
  
/* These theorems are all stated without an explicit left context ‚. That is possible because, in ItL_rules.j,
  * we declared a WEAKEN structure rule: Jape will automatically discard any unmatched left-context
  * formulae.
  */
  
CONJECTUREPANEL Conjectures
  THEOREMS NaturalDeductionConjectures ARE
  		P, P ç Q æ Q
  AND	P ç Q, Q ç R , P æ R	
  AND	P ç (Q ç R), P ç Q, P æ R
  AND	P ç Q, Q ç R æ P ç R
  AND	P ç (Q ç R) æ Q ç (P ç R)
  AND	P ç (Q ç R) æ (P ç Q) ç (P ç R)
  AND	P æ Q ç P
  AND	æ P ç (Q ç P)
  AND	P ç Q æ (Q ç R) ç (P ç R)
  AND	P ç (Q ç (R ç S)) æ R ç (Q ç (P ç S ))
  AND	æ (P ç (Q ç R)) ç ((P ç Q) ç (P ç R))
  
  AND	P, Q æ P ¦ Q
  AND	P ¦ Q æ P
  AND	P ¦ Q æ Q
  AND	(P ¦ Q) ç R æ P ç (Q ç R)
  AND	P ç (Q ç R) æ (P ¦ Q) ç R
  
  AND	P æ P ë Q	
  AND	Q æ P ë Q	
  
  AND	P ë Q æ Q ë P
  AND	Q ç R æ (P ë Q) ç (P ë R)
  AND	P ë P æ P
  AND	P æ P ë P
  AND	P ë (Q ë R) æ (P ë Q) ë R
  AND	(P ë Q) ë R æ P ë (Q ë R)
  AND	P ¦ (Q ë R) æ (P ¦ Q) ë (P ¦ R)
  AND	(P ¦ Q) ë (P ¦ R) æ P ¦ (Q ë R)
  AND	P ë (Q ¦ R) æ (P ë Q) ¦ (P ë R)
  AND	(P ë Q) ¦ (P ë R) æ P ë (Q ¦ R)
  AND	P ç R, Q ç R æ (P ë Q) ç R 
  
  AND	æ Â ÂP ç P
  AND	P æ Â ÂP
  AND	P ç Q æ ÂQ ç ÂP
  AND	ÂQ ç ÂP æ P ç Q
  AND	æ P ë ÂP
  AND	P ë Q æ Â(ÂP ¦ ÂQ)
  AND	Â(ÂP ¦ ÂQ) æ P ë Q
  AND	P ¦ Q æ Â(ÂP ë ÂQ)
  AND	Â(ÂP ë ÂQ) æ P ¦ Q
  AND	Â(P ë Q) æ ÂP ¦ ÂQ
  AND	ÂP ¦ ÂQ æ Â(P ë Q)
  AND	Â(P ¦ Q) æ ÂP ë ÂQ
  AND	ÂP ë ÂQ æ Â(P ¦ Q)
  AND	æ Â(P ¦ ÂP)
  AND	Q ç P, P ç R æ Q ç R
  AND	æ (P ç Q) ë (Q ç P)
  AND	P ¦ ÂP æ Q
  AND	æ ((P ç Q) ç P) ç P
  
  AND	P(c), èx.(P(x) ç Q(x)) æ Q(c)
  AND	èx.P(x) ç Q(x) æ (èx.P(x)) ç (èx.Q(x))
  AND	èx.P(x) ç Q(x), èx.Q(x) ç R(x) æ èx.P(x) ç R(x)
  AND	(èx.P(x)) ¦ (èx.Q(x)) æ èx.P(x) ¦ Q(x)
  AND	èx.P(x) ¦ Q(x) æ (èx.P(x)) ¦ (èx.Q(x))
  AND	èx.P(x) æ äx.P(x)
  AND	èx.P(x) ç Q(x), äx.P(x) æ äx.Q(x)
  AND	äx.P(x) ¦ Q(x) æ (äx.P(x)) ¦ (äx.Q(x))
  AND	(äx.P(x)) ë (äx.Q(x)) æ äx.P(x) ë Q(x)
  AND	äx.P(x) ë Q(x) æ (äx.P(x)) ë (äx.Q(x))
  AND	èx.P(x) æ Â(äx. ÂP(x))
  AND	Â(äx. ÂP(x)) æ èx.P(x)
  AND	äx.P(x) æ Â(èx. ÂP(x))
  AND	Â(èx. ÂP(x)) æ äx.P(x)
  AND	Â(èx.P(x)) æ äx. ÂP(x)
  AND	äx. ÂP(x) æ Â(èx.P(x))
  AND	Â(äx.P(x)) æ èx. ÂP(x)
  AND	èx. ÂP(x) æ Â(äx.P(x))
  END
  
  THEOREM "(èx.P(x)) ç (èx.Q(x)) æ èx.P(x) ç Q(x) NOT" IS (èx.P(x)) ç (èx.Q(x)) æ èx.P(x) ç Q(x)
  THEOREM "(äx.P(x)) ¦ (äx.Q(x)) æ äx.P(x) ¦ Q(x) NOT" IS (äx.P(x)) ¦ (äx.Q(x)) æ äx.P(x) ¦ Q(x)
  
  PREFIXBUTTON Apply IS apply TheoremForwardOrBackward
END
