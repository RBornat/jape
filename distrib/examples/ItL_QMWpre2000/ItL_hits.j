/* $Id$ */

HYPHIT	P     æ P	IS hyp       
HYPHIT	P¦Q æ R	IS ALT(SEQ("¦-E(L)", hyp (P¦Q)), 
                                           SEQ("¦-E(R)", hyp (P¦Q)),
                                           SEQ(cut,"¦-E(L)", hyp (P¦Q),cut,"¦-E(R)", hyp (P¦Q))
                                          )
HYPHIT	PëQ  æ R	IS "ë-Eforward"  (PëQ)
HYPHIT	PçQ  æ R	IS "ç-Eforward"  (PçQ)
HYPHIT	ÂÂP   æ Q	IS "Â-Eforward"   (ÂÂP)
HYPHIT	èx.P æ Q	IS "è-Eforward"  (èx.P)
HYPHIT	äx.P æ Q	IS "ä-Eforward"  (äx.P)

CONCHIT	Q¦R	IS "¦-I"
CONCHIT	QëR	IS ALT(SEQ("ë-I(L)", hyp), SEQ("ë-I(R)", hyp))
CONCHIT	QçR	IS "ç-I"      
CONCHIT	ÂQ	IS "Â-I"       
CONCHIT	èx.Q	IS "è-I"  
CONCHIT	äx.Q	IS "ä-I"  
