/* $Id$ */

TACTIC TheoremForwardOrBackward(thm) IS
  WHEN	(LETHYP _P cut (ALT (WITHSELECTIONS thm) (RESOLVE (WITHSELECTIONS thm))))
		(ALT (WITHSELECTIONS thm) (RESOLVE (WITHSELECTIONS thm)) 
			(SEQ cut (ALT (WITHSELECTIONS thm) (RESOLVE (WITHSELECTIONS thm))))
		)
  
/* These theorems are all stated without an explicit left context ‚. That is possible because, in ItL_rules.j,
  * we declared a WEAKEN structure rule: Jape will automatically discard any unmatched left-context
  * formulae.
  */
  
CONJECTUREPANEL Conjectures
	THEOREM INFER	P, P ç Q æ Q
	THEOREM INFER	P ç Q, Q ç R , P æ R	
	THEOREM INFER	P ç (Q ç R), P ç Q, P æ R
	THEOREM INFER	P ç Q, Q ç R æ P ç R
	THEOREM INFER	P ç (Q ç R) æ Q ç (P ç R)
	THEOREM INFER	P ç (Q ç R) æ (P ç Q) ç (P ç R)
	THEOREM INFER	P æ Q ç P
	THEOREM INFER	P ç (Q ç P)
	THEOREM INFER	P ç Q æ (Q ç R) ç (P ç R)
	THEOREM INFER	P ç (Q ç (R ç S)) æ R ç (Q ç (P ç S ))
	THEOREM INFER	(P ç (Q ç R)) ç ((P ç Q) ç (P ç R))
  
	THEOREM INFER	P, Q æ P ¦ Q
	THEOREM INFER	P ¦ Q æ P
	THEOREM INFER	P ¦ Q æ Q
	THEOREM INFER	(P ¦ Q) ç R æ P ç (Q ç R)
	THEOREM INFER	P ç (Q ç R) æ (P ¦ Q) ç R
  
	THEOREM INFER	P æ P ë Q	
	THEOREM INFER	Q æ P ë Q	
  
	THEOREM INFER	P ë Q æ Q ë P
	THEOREM INFER	Q ç R æ (P ë Q) ç (P ë R)
	THEOREM INFER	P ë P æ P
	THEOREM INFER	P æ P ë P
	THEOREM INFER	P ë (Q ë R) æ (P ë Q) ë R
	THEOREM INFER	(P ë Q) ë R æ P ë (Q ë R)
	THEOREM INFER	P ¦ (Q ë R) æ (P ¦ Q) ë (P ¦ R)
	THEOREM INFER	(P ¦ Q) ë (P ¦ R) æ P ¦ (Q ë R)
	THEOREM INFER	P ë (Q ¦ R) æ (P ë Q) ¦ (P ë R)
	THEOREM INFER	(P ë Q) ¦ (P ë R) æ P ë (Q ¦ R)
	THEOREM INFER	P ç R, Q ç R æ (P ë Q) ç R 
  
	THEOREM INFER	ÂÂP ç P
	THEOREM INFER	P æ Â ÂP
	THEOREM INFER	P ç Q æ ÂQ ç ÂP
	THEOREM INFER	ÂQ ç ÂP æ P ç Q
	THEOREM INFER	P ë ÂP
	THEOREM INFER	P ë Q æ Â(ÂP ¦ ÂQ)
	THEOREM INFER	Â(ÂP ¦ ÂQ) æ P ë Q
	THEOREM INFER	P ¦ Q æ Â(ÂP ë ÂQ)
	THEOREM INFER	Â(ÂP ë ÂQ) æ P ¦ Q
	THEOREM INFER	Â(P ë Q) æ ÂP ¦ ÂQ
	THEOREM INFER	ÂP ¦ ÂQ æ Â(P ë Q)
	THEOREM INFER	Â(P ¦ Q) æ ÂP ë ÂQ
	THEOREM INFER	ÂP ë ÂQ æ Â(P ¦ Q)
	THEOREM INFER	æ Â(P ¦ ÂP)
	THEOREM INFER	Q ç P, P ç R æ Q ç R
	THEOREM INFER	(P ç Q) ë (Q ç P)
	THEOREM INFER	P ¦ ÂP æ Q
	THEOREM INFER	((P ç Q) ç P) ç P
  
	THEOREM INFER	var c, P(c), èx.(P(x) ç Q(x)) æ Q(c)
	THEOREM INFER	èx.(P(x) ç Q(x)) æ èx.P(x) ç èx.Q(x)
	THEOREM INFER	èx.(P(x) ç Q(x)), èx.(Q(x) ç R(x)) æ èx.(P(x) ç R(x))
	THEOREM INFER	èx.P(x) ¦ èx.Q(x) æ èx.(P(x) ¦ Q(x))
	THEOREM INFER	èx.(P(x) ¦ Q(x)) æ èx.P(x) ¦ èx.Q(x)
	THEOREM INFER	èx.(P(x) ç Q(x)), äx.P(x) æ äx.Q(x)
	THEOREM INFER	äx.(P(x) ¦ Q(x)) æ äx.P(x) ¦ äx.Q(x)
	THEOREM INFER	äx.P(x) ë äx.Q(x) æ äx.(P(x) ë Q(x))
	THEOREM INFER	äx.(P(x) ë Q(x)) æ äx.P(x) ë äx.Q(x)
	THEOREM INFER	var c, èx.P(x) æ äx.P(x)
	THEOREM INFER	èx.P(x) æ Â(äx. ÂP(x))
	THEOREM INFER	Â(äx. ÂP(x)) æ èx.P(x)
	THEOREM INFER	äx.P(x) æ Â(èx. ÂP(x))
	THEOREM INFER	Â(èx. ÂP(x)) æ äx.P(x)
	THEOREM INFER	Â(èx.P(x)) æ äx. ÂP(x)
	THEOREM INFER	äx. ÂP(x) æ Â(èx.P(x))
	THEOREM INFER	Â(äx.P(x)) æ èx. ÂP(x)
	THEOREM INFER	èx. ÂP(x) æ Â(äx.P(x))
  
	THEOREM	"èx.P(x) æ äx.P(x) NOT" IS èx.P(x) æ äx.P(x)
	THEOREM	"P(c), èx.(P(x) ç Q(x)) æ Q(c) NOT" IS P(c), èx.(P(x) ç Q(x)) æ Q(c)
	THEOREM "var c, Q(c) æ èx.(P(x) ç Q(x)) NOT" IS var c, Q(c) æ èx.(P(x) ç Q(x))
	THEOREM "(èx.P(x)) ç (èx.Q(x)) æ èx.(P(x) ç Q(x)) NOT" IS èx.P(x) ç èx.Q(x) æ èx.(P(x) ç Q(x))
	THEOREM "(äx.P(x)) ¦ (äx.Q(x)) æ äx.(P(x) ¦ Q(x)) NOT" IS äx.P(x) ¦ äx.Q(x) æ äx.(P(x) ¦ Q(x))
  
	PREFIXBUTTON Apply IS apply TheoremForwardOrBackward
END
