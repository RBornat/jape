/* $Id$ */

TACTIC ForwardCut (n,Rule)
  SEQ cut (WITHARGSEL Rule) (SUBGOAL n) (WITHHYPSEL hyp)

TACTIC ForwardUncut (n,Rule)
  SEQ (WITHARGSEL Rule) (SUBGOAL n) (WITHHYPSEL hyp)

TACTIC ForwardOrBackward (Forward, n, Rule) IS 
	WHEN	(LETHYP 
				_P 
                          	(ALT	(Forward n Rule)
					(WHEN	(LETARGSEL _Q 
                                                              (Fail (Rule is not applicable to assumption ' _P ' with argument ' _Q '))
							)
							(Fail (Rule is not applicable to assumption ' _P '))
					)
                                )
			)
			(ALT	(WITHSELECTIONS Rule)
                   		(WHEN	(LETARGSEL _P
                                                	(Fail (Rule is not applicable with argument ' _P '))
                                              	)
						(Fail (Rule is not applicable))
				)
           		)
	
MENU Rules IS
	ENTRY "ç-I"
	 ENTRY "¦-I"	
	ENTRY "ë-I(L)"		IS ForwardOrBackward ForwardCut 0 "ë-I(L)"
	ENTRY "ë-I(R)"		IS ForwardOrBackward ForwardCut 0 "ë-I(R)"
	ENTRY "Â-I"
	ENTRY "è-I"
	ENTRY "ä-I"		IS "ä-I with side condition hidden"
	SEPARATOR
	ENTRY "ç-E"		IS ForwardOrBackward ForwardCut 1 "ç-E" 
	ENTRY "¦-E(L)"		IS ForwardOrBackward ForwardCut 0 "¦-E(L)"
	ENTRY "¦-E(R)" 	IS ForwardOrBackward ForwardCut 0 "¦-E(R)"
	ENTRY "ë-E"		IS ForwardOrBackward ForwardUncut 0 "ë-E"	
	ENTRY "Â-E"		IS ForwardOrBackward ForwardCut 0 "Â-E"	
	ENTRY "è-E"		IS ForwardOrBackward ForwardCut 0 "è-E with side condition hidden"	
	ENTRY "ä-E"		IS ForwardOrBackward ForwardUncut 0 "ä-E"
	SEPARATOR
	ENTRY hyp
END
   
/* The next tactic is needed because we can't give an application of a tactical as an argument.
   * But we'd probably include it anyway, for clarity.
   */
TACTIC "è-E with side condition hidden" IS LAYOUT "è-E" (0) (WITHARGSEL "è-E")
TACTIC "ä-I with side condition hidden" IS  LAYOUT "ä-I" (0) (WITHARGSEL "ä-I")

