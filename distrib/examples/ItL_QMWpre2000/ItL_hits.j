/* $Id$ */

HYPHIT	P     æ P	IS hyp       
HYPHIT	P¦Q æ R	IS ALT	(SEQ "¦-E(L)" (WITHHYPSEL hyp))
                                           	(SEQ "¦-E(R)" (WITHHYPSEL hyp))
                                           	(SEQ (ForwardCut 0 "¦-E(L)") (ForwardCut 0 "¦-E(R)"))
HYPHIT	PçQ  æ R	IS ForwardCut 1 "ç-E"
HYPHIT	PëQ  æ R	IS ForwardUncut 0 "ë-E"
HYPHIT	ÂÂP   æ Q	IS ForwardCut 0 "Â-E"
HYPHIT	èx.P æ Q	IS ForwardCut 0 "è-E"
HYPHIT	äx.P æ Q	IS ForwardUncut 0 "ä-E"

CONCHIT	Q¦R	IS "¦-I"
CONCHIT	QëR	IS ALT (SEQ "ë-I(L)" hyp) (SEQ "ë-I(R)" hyp)
CONCHIT	QçR	IS "ç-I"      
CONCHIT	ÂQ	IS "Â-I"       
CONCHIT	èx.Q	IS "è-I"  
CONCHIT	äx.Q	IS "ä-I"  
