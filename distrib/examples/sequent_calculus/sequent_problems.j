/*
        Some problems, originally cribbed from MacLogic

       $Id$
       
*/

/* 14/ix/92 - RB tidied up some of the provisos in later problems, and can't prove
        17, 21, 24, 26, 41 (or dnegh, dnegh2) using aipc rules.
        
        37 as originally stated was false, RB believes; he also believes that it 
        was probably intended to be stated as 37a.  Or maybe 37b
        
        This activity showed up some problems with proviso simplification/interpretation, 
        q.v. elsewhere.
 */

CONJECTUREPANEL "Conjectures"
  THEOREMS PropositionalProblems ARE
  	Pç(QçR)			æ (PçQ)ç(PçR)
  AND Pç(QçR), Q 		æ PçR
  AND RçS			æ (PçR) ç (PçS)
  AND Pç(PçQ)			æ PçQ
  AND P				æ Qç(P ¦ Q)
  AND P¦(Q¦R)			æ (P¦Q)¦R
  AND PçQ,PçR			æ PçQ¦R
  AND Pç(QçR)			æ (P¦Q) ç R
  AND P¦(QëR)			æ (P¦Q) ë (P¦R)
  AND Pë(Q¦R)			æ (PëQ) ¦ (PëR)
  AND PçS,Qç ÂS		æ Â(P¦Q)
  AND PçQ, Qç ÂP		æ ÂP
  AND Pç ( Q¦R )		æ ( PçQ ) ¦ ( PçR )
  AND (PçQ) ¦ (QçR), ÂR	æ ÂP
  AND PçQ,ÂQ			æ (ÂÂÂP)ëQ
  AND P¦Q				æ Â(P é ÂQ)
  AND PéQ,Qé ÂR		æ R é ÂP
  AND PëQ,ÂP			æ Q
  AND Â(PëQ)			æ ÂP ¦ ÂQ
  AND ÂP ¦ ÂQ			æ Â(PëQ)
  AND Â(P¦Q)			æ ÂP ë ÂQ
  AND ÂP ë ÂQ			æ Â(P¦Q)
  AND 				æ Â(P ¦ ÂP)
  AND 				æ ((P ç Q) ç P) ç P
  AND 				æ (P ¦ ÂP) ç Q
  AND Pç Â(QçR)		æ (PçQ) ¦ (Pç ÂR)
  AND Â(Pç(QëR)) 		æ (QëR)çP
  
  AND èx.ÂQ(x),  Pç(èx.Q(x))			æ ÂP
  AND PëÂP, èx.PçQ(x), èx. ÂPçQ(x) 	æ èx.Q(x)		WHERE x NOTIN P END
  AND RëÂR, èx.RçS(x), èx. ÂRçS(x)	æ èx.S(x)
  AND èx.P(x)çQ(x), èx.Q(x)çR(x) 		æ èx.P(x)çR(x)
  AND èx.P(x)çR(x), èx.Q(x)ç ÂR(x)   	æ èx.(P(x)çÂQ(x)) ¦ (Q(x)çÂP(x))
  AND S(m,n), èx.P(x) ç ÂS(x,n) 		æ ÂP(m)
  AND èx.P(x)çQ(x), èx.R(x)çÂQ(x)   	æ èx.R(x)çÂP(x)
  AND äx.P(x)¦Q(x)			  		æ äx.P(x)
  AND äx.P(x)¦Q(x)			  		æ äx.Q(x)¦P(x)  
  AND äx.P(x)¦ÂQ(x), èx.P(x)çR(x)  	æ äx.R(x)¦ÂQ(x)
  AND (èx.Q(x)) ç (èy.ÂQ(y)) 			æ Â(äz.Q(z))
  AND (èx.Q(x)) ç (èy.ÂQ(y)) 			æ äz.ÂQ(z)
  AND (äx.P(x)) ç (äy.ÂP(y)) 			æ Â(èz.P)
  AND äx.ÂP(x) 						æ Â(èx.P(x))
  AND èx.P(x)çQ 					æ (äx.P(x))çQ
  AND èx.S(x) ç ((ÂP(x)¦ÂQ(x)) ç R(x)) 	æ èx.(S(x)¦ÂR(x))ç(P(x)ëQ(x))
  
  AND ÂÂP 			æ P
  AND P 			æ ÂÂP
  AND äx.èy.P 		æ èy.äx.P		WHERE x NOTIN y END
  AND äx.èy.Q 		æ èy.äx.Q
  AND äx.èy.P(x,y) 	æ èy.äx.P(x,y)
  AND äx.èy.P(x,y) 	æ èv.äu.P(u,v)
  AND èx.P(x)		æ èy.P(y) 
  AND äx.P(x) 		æ äy.P(y)
  AND äy.P(y) 		æ èx.P(x)
  END
END
