/* $Id$ */

HYPHIT	P     æ P	IS hyp       
HYPHIT	P¦Q æ R	IS ALT	(SEQ "¦ elim(L)" (WITHHYPSEL hyp))
                                           	(SEQ "¦ elim(R)" (WITHHYPSEL hyp))
                                           	(SEQ (ForwardCut 0 "¦ elim(L)") (ForwardCut 0 "¦ elim(R)"))
HYPHIT	PçQ  æ R	IS ForwardCut 1 "ç elim"
HYPHIT	PëQ  æ R	IS ForwardUncut 0 "ë elim"
HYPHIT	ÂÂP   æ Q	IS ForwardCut 0 "Â elim"
HYPHIT	èx.P æ Q	IS ForwardCut 0 "è elim with side condition hidden"	
HYPHIT	äx.P æ Q	IS ForwardUncut 0 "ä elim"

CONCHIT	Q¦R	IS "¦ intro"
CONCHIT	QëR	IS ALT (SEQ "ë intro(L)" hyp) (SEQ "ë intro(R)" hyp)
CONCHIT	QçR	IS "ç intro"      
CONCHIT	ÂQ	IS "Â intro"       
CONCHIT	èx.Q	IS "è intro"  
CONCHIT	äx.Q	IS "ä intro with side condition hidden"
