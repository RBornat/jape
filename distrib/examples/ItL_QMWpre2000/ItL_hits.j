/* $Id$ */

HYPHIT	P     æ P	IS hyp       
HYPHIT	P¦Q æ R	IS ALT	(SEQ "¦-E(L)" (hyp (P¦Q)))
                                           	(SEQ "¦-E(R)" (hyp (P¦Q)))
                                           	(SEQ cut "¦-E(L)" (hyp (P¦Q)) cut "¦-E(R)" (hyp (P¦Q)))
HYPHIT	PçQ  æ R	IS "ç-Eforward"  (PçQ)
HYPHIT	PëQ  æ R	IS ForwardUncut "ë-E"  (PëQ)
HYPHIT	ÂÂP   æ Q	IS ForwardCut "Â-E"   (ÂÂP)
HYPHIT	èx.P æ Q	IS ForwardCut "è-E"  (èx.P)
HYPHIT	äx.P æ Q	IS ForwardUncut "ä-E"  (äx.P)

CONCHIT	Q¦R	IS "¦-I"
CONCHIT	QëR	IS ALT (SEQ "ë-I(L)" hyp) (SEQ "ë-I(R)" hyp)
CONCHIT	QçR	IS "ç-I"      
CONCHIT	ÂQ	IS "Â-I"       
CONCHIT	èx.Q	IS "è-I"  
CONCHIT	äx.Q	IS "ä-I"  
