/* $Id$ */

TACTIC TheoremForwardOrBackward(X) IS
  WHEN (LETHYP _P cut (WITHSELECTIONS X)) X
  
CONJECTUREPANEL "Set Conjectures" IS
	THEOREMS "Set Conjectures" ARE
		(äx.P(x) ¦ (èy. P(y) ç y=x)) ê (äu. P(u)) ¦ (èv,w. P(v) ¦ P(w) ç v=w)
	AND 	A§U
	AND	A=B ê A§B ¦ B§A
	AND	A§A
	AND	A§B, B§C æ A§C
	AND	¯§A
	AND CÚAïBêCÚAëCÚB
	AND CÚAßBêCÚA¦CÚB
	AND CÚA-BêCÚA¦CÂÚB
	AND	AïB=BïA
	AND AßB=BßA
	AND (AïB)ïC=Aï(BïC)
	AND (AßB)ßC=Aß(BßC)
	AND Aï(BßC)=(AïB)ß(AïC)
	AND Aß(BïC)=(AßB)ï(AßC)
	AND A-(BïC)=(A-B)ß(A-C)
	AND A-(BßC)=(A-B)ï(A-C)
	AND AïA=A
	AND AßA=A
	AND Aï¯=A
	AND Aß¯=¯
	AND A-¯=A
	AND A-B§A
	AND A§B, C§D æ (AïC)§(BïD)
	AND A§B, C§D æ (AßC)§(BßD)
	AND A§AïB
	AND AßB§A
	AND A§B æ AïB=B
	AND A§B æ AßB=A
	AND Aß(B-A)=¯
	AND Aï(B-A)=AïB
	AND AïAø=U
	AND AßAø=¯
	AND Aø=B ê (AïB=U)¦(AßB=¯)
	AND Aøø=A
	AND (AïB)ø=AøßBø
	AND (AßB)ø=AøïBø
	AND (OBJECT y) INFER <C,D>ÚA¥B ê (äy.<C,y>ÚA¦<y,D>ÚB)
	AND (A¥B)¥C=A¥(B¥C)

	END

	PREFIXBUTTON Apply IS apply TheoremForwardOrBackward
	PREFIXBUTTON "AêÉ" IS apply ForwardSubstHiding "rewrite ê Ç" "rewrite ê È"
	PREFIXBUTTON "ÉêB" IS apply ForwardSubstHiding "rewrite ê È" "rewrite ê Ç"
	PREFIXBUTTON "A=É" IS apply ForwardSubstHiding "rewrite = Ç" "rewrite = È"
	PREFIXBUTTON "É=B" IS apply ForwardSubstHiding "rewrite = È" "rewrite = Ç"
END
