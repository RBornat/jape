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
  THEOREM INFER	Pç(QçR)				æ (PçQ)ç(PçR)
  THEOREM INFER	Pç(QçR), Q 			æ PçR
  THEOREM INFER	RçS						æ (PçR) ç (PçS)
  THEOREM INFER	Pç(PçQ)				æ PçQ
  THEOREM INFER	P							æ Qç(P ¦ Q)
  THEOREM INFER	P¦(Q¦R)				æ (P¦Q)¦R
  THEOREM INFER	PçQ,PçR				æ PçQ¦R
  THEOREM INFER	Pç(QçR)				æ (P¦Q) ç R
  THEOREM INFER	P¦(QëR)				æ (P¦Q) ë (P¦R)
  THEOREM INFER	Pë(Q¦R)				æ (PëQ) ¦ (PëR)
  THEOREM INFER	PçS,Qç ÂS			æ Â(P¦Q)
  THEOREM INFER	PçQ, Qç ÂP			æ ÂP
  THEOREM INFER	Pç ( Q¦R )				æ ( PçQ ) ¦ ( PçR )
  THEOREM INFER	(PçQ) ¦ (QçR), ÂR	æ ÂP
  THEOREM INFER	PçQ,ÂQ					æ (ÂÂÂP)ëQ
  THEOREM INFER	P¦Q						æ Â(P é ÂQ)
  THEOREM INFER	PéQ,Qé ÂR				æ R é ÂP
  THEOREM INFER	PëQ,ÂP					æ Q
  THEOREM INFER	Â(PëQ)					æ ÂP ¦ ÂQ
  THEOREM INFER	ÂP ¦ ÂQ					æ Â(PëQ)
  THEOREM INFER	Â(P¦Q)					æ ÂP ë ÂQ
  THEOREM INFER	ÂP ë ÂQ					æ Â(P¦Q)
  THEOREM INFER								æ Â(P ¦ ÂP)
  THEOREM INFER								æ ((P ç Q) ç P) ç P
  THEOREM INFER								æ (P ¦ ÂP) ç Q
  THEOREM INFER	Pç Â(QçR)			æ (PçQ) ¦ (Pç ÂR)
  THEOREM INFER	Â(Pç(QëR)) 			æ (QëR)çP
  
  THEOREM INFER	èx.ÂQ(x),  Pç(èx.Q(x))	æ ÂP
  THEOREM WHERE x NOTIN P INFER PëÂP, èx.PçQ(x), èx. ÂPçQ(x) 	æ èx.Q(x)
  THEOREM INFER	RëÂR, èx.RçS(x), èx. ÂRçS(x)	æ èx.S(x)
  THEOREM INFER	èx.P(x)çQ(x), èx.Q(x)çR(x) 		æ èx.P(x)çR(x)
  THEOREM INFER	èx.P(x)çR(x), èx.Q(x)ç ÂR(x)   	æ èx.(P(x)çÂQ(x)) ¦ (Q(x)çÂP(x))
  THEOREM INFER	S(m,n), èx.P(x) ç ÂS(x,n) 			æ ÂP(m)
  THEOREM INFER	èx.P(x)çQ(x), èx.R(x)çÂQ(x)   	æ èx.R(x)çÂP(x)
  THEOREM INFER	äx.P(x)¦Q(x)			  					æ äx.P(x)
  THEOREM INFER	äx.P(x)¦Q(x)			  					æ äx.Q(x)¦P(x)  
  THEOREM INFER	äx.P(x)¦ÂQ(x), èx.P(x)çR(x) 	 	æ äx.R(x)¦ÂQ(x)
  THEOREM INFER	(èx.Q(x)) ç (èy.ÂQ(y)) 				æ Â(äz.Q(z))
  THEOREM INFER	(èx.Q(x)) ç (èy.ÂQ(y)) 				æ äz.ÂQ(z)
  THEOREM INFER	(äx.P(x)) ç (äy.ÂP(y)) 				æ Â(èz.P(z))
  THEOREM INFER	äx.ÂP(x) 									æ Â(èx.P(x))
  THEOREM INFER	èx.P(x)çQ 								æ (äx.P(x))çQ
  THEOREM INFER	èx.S(x) ç ((ÂP(x)¦ÂQ(x)) ç R(x)) æ èx.(S(x)¦ÂR(x))ç(P(x)ëQ(x))
  
  THEOREM INFER	ÂÂP æ P
  THEOREM INFER	P æ ÂÂP
  THEOREM	WHERE x NOTIN y INFER äx.èy.P æ èy.äx.P
  THEOREM INFER	äx.èy.Q 		æ èy.äx.Q
  THEOREM INFER	äx.èy.P(x,y) 	æ èy.äx.P(x,y)
  THEOREM INFER	äx.èy.P(x,y) 	æ èv.äu.P(u,v)
  THEOREM INFER	èx.P(x)			æ èy.P(y) 
  THEOREM INFER	äx.P(x) 			æ äy.P(y)
  THEOREM INFER	äy.P(y) 			æ èx.P(x)
END
