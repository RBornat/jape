/* $Id$ */

/* ******************** remains of the old 'choose by selection' mechanism, which is dead but may fly again ******************** */

TACTIC ForwardOrBackward (Forward, n, Rule) IS 
	WHEN	(LETHYP _Ah 
					(ALT	(Forward n Rule)
								(WHEN	(LETARGSEL _B  (Fail (Rule is not applicable to antecedent ' _Ah ' with argument ' _B ')))
											(Fail (Rule is not applicable to antecedent ' _Ah '))
								)
					)
				)
				(ALT	(WITHSELECTIONS Rule)
						(WHEN	(LETARGSEL _Ah (Fail (Rule is not applicable with argument ' _Ah ')))
							(Fail (Rule is not applicable))
						)
				)

/* ******************** tactics to apply rules pseudo-forwards ******************** */

TACTIC ForwardCut (n,rule)
	CUTIN (ForwardUncut n rule)

TACTIC ForwardUncut (n,Rule)
	WHEN	(LETHYP _Ah 
					(LETGOALPATH G (WITHARGSEL Rule) (GOALPATH (SUBGOAL G n)) (WITHHYPSEL hyp) (GOALPATH G) NEXTGOAL)
				)
				/* If LETHYP fails at this point, we had better have a singleton LHS.*/
				(LETLHS _Ah
					(LETGOALPATH G (WITHARGSEL Rule) (GOALPATH (SUBGOAL G n)) 
					(LETGOAL _Ag 
						(ALT (UNIFY _Ag _Ah) (Fail ("Error in I2L Jape (can't unify lhs %t with rhs %t in ForwardUncut). Tell Richard.", _Ah, _Ag)))
						(ANY hyp)
					) (GOALPATH G) NEXTGOAL)
				)
				(Fail "Error in I2L Jape (ForwardUncut falls through). Tell Richard.")

/* ******************** tactics to deal with variables in quantification rules ******************** */

TACTIC "è elim forward" IS
	WHEN	(LETHYP2 (actual _i) (è_x._A) ("è elim forward step" (è_x._A) _i))
				(LETHYP (è_x._A) ("è elim forward moan" ("You only selected %t.", è_x._A)))
				(LETHYP (actual _i) ("è elim forward moan" ("You only selected %t.", actual _i)))
				(LETHYPS _As ("è elim forward moan" ("You selected %l.", (_As, ", ", " and "))))
				(LETGOAL (è_x._A)
					(ALERT ("To make a è elim step forward, you must select an antecedent of the form èx.A, and \
								\also a pseudo-assumption of the form actual i. You didn't select any antecedents, but the \
								\current conclusion is %t, which could be used to make a è intro step backwards.\
								\\nDid you perhaps mean to make a backward step with è intro?", è_x._A)
								("OK",STOP) ("Huh?",SEQ Explainantecedentandconclusionwords STOP)
					)
				)
				("è elim forward moan" "You didn't select any antecedents")
			
TACTIC "è elim forward step" (P, i) IS
	Noarg	(CUTIN 
					"è elim"
					(WITHHYPSEL (hyp P))
					(WITHHYPSEL (hyp (actual i)))
				)

TACTIC "è elim forward moan" (extra) IS
	ALERT	("To make a è elim step forward, you must select an antecedent of the form èx.A, and \
				\also a pseudo-assumption of the form actual i.%s",extra)
				("OK", STOP) ("Huh?", SEQ Explainvariables STOP)

TACTIC "ä elim forward" IS
	ForwardUncut 0 "ä elim"
	
TACTIC "ä intro backward" IS  
	WHEN	(LETCONC (ä_x._A)
					("ä intro backward hypcheck" "ä intro backward step" ("You selected the conclusion %t (which is ok), but ", ä_x._A))
				)
				(LETCONC _A
					("ä intro backward selection moan" (" You selected the conclusion %t.", _A))
				)
				(LETGOAL (ä_x._A)
					("ä intro backward hypcheck" "ä intro backward step" ("The conclusion is %t (which is ok), but", ä_x._A))
				)
				(LETGOAL _A /* the wrong goal, sob */
					("ä intro backward selection moan" (" The conclusion you are working on is %t.", _A))
				)
				(LETOPENSUBGOAL G (ä_x._A) /* there is something they might have chosen */
					("ä intro backward hypcheck" 
						(ALERT ("To make an ä intro step backwards, you have to use a conclusion of the form \
									\äx.A, and select a pseudo-assumption of the form actual i. You selected the \
									\pseudo-assumption, but you didn't click on the conclusion %t. Would you like to proceed \
									\with the step using %t?\
									\\n\n(You can avoid this message in the future if you always click on the relevant \
									\conclusion before you make the step.)", ä_x._A, ä_x._A)
									("OK", SEQ (GOALPATH G) "ä intro backward step")  ("Huh?", SEQ Explainvariables STOP) ("Cancel", STOP)
						)
						("You didn't select the unproved conclusion %t, and", ä_x._A))
				)
				("ä intro backward hypcheck" ("ä intro backward selection moan" " You didn't make any conclusion selection.") 
					("You didn't make any conclusion selection, and ")
				)

TACTIC "ä intro backward step" (i) IS
	Noarg	(SEQ
					"ä intro"
					fstep 
					(WITHHYPSEL (hyp (actual i)))
				)
				"ä intro"

TACTIC "ä intro backward selection moan" (stuff) IS
	ALERT ("To make an ä intro step backwards, you have to use a conclusion of the form \
				\äx.A, and select a pseudo-assumption of the form actual i.%s", stuff)
				("OK", STOP) ("Huh?", SEQ Explainvariables STOP)

TACTIC "ä intro backward hypcheck" (action, gmess) IS
	WHEN	(LETHYP (actual _i) (action _i)) /* the right antecedent - hoorah! */
				(LETHYP (ä_x._A) /* looks like a forward step */
					(ALERT ("To make an ä intro step backwards, you have to select a conclusion of the form \
								\äx.A, and a pseudo-assumption of the form actual i. You selected \
								\the antecedent %t, which would fit ä elim going forward.\
								\\nWould you like to make the forward step instead?",ä_x._A)
								("Forward", "ä elim forward") ("Huh?", SEQ Explainvariables STOP) ("Cancel", STOP)
					)
				)
				(LETHYP _A /* the wrong antecedent */
					("ä intro backward selection moan" ("%s you selected the antecedent %t instead.", gmess, _A))
				)
				(LETHYPS _As /* more than one */
					("ä intro backward selection moan" ("%s you selected more than one antecedent Ð %l.", gmess, (_As, ", ", " and ")) )
				)
				("ä intro backward selection moan" ("%s you didn't select an antecedent.", gmess))

/* ******************** most rules don't want arguments ******************** */

TACTIC Noarg (rule, stepname) IS
	WHEN	(LETARGTEXT arg 
					(ALERT	("The %s rule doesn't need an argument, but you text-selected %s. \
								\Do you want to go on with the step, ignoring the text-selection?", stepname, arg)
								("OK", rule)
								("Cancel", STOP)
					)
				)
				rule

TACTIC SingleorNoarg (rule, stepname) IS 
	WHEN	(LETARGSEL _A (WITHARGSEL rule))
				(LETMULTIARG _As 
					(ALERT ("The %s rule can accept a single argument. You text-selected %l, which is more than \
								\it can deal with. Cancel some or all of your text selections and try again.", stepname, _As)
								("OK", STOP)
					)
				)
				rule

/* ******************** tactics for rules that only work backwards ******************** */

TACTIC BackwardOnlyA (pattern, action, stepname, shape) IS BackwardOnly pattern action stepname shape "only makes sense working backwards"

TACTIC BackwardOnlyB (pattern, action, stepname, shape) IS BackwardOnly pattern action stepname shape "doesn't work backwards in I2L Jape yet"

TACTIC BackwardOnlyC (pattern, action, stepname, shape) IS BackwardOnly pattern action stepname shape "can also be used forward -- see the Forward menu"

MACRO BackwardOnly (pattern, action, stepname, shape, explain) IS
	WHEN	(LETGOAL pattern 
					(WHEN	(LETHYP _Ah (BackwardOnly2 stepname action explain _Ah))
								(WITHSELECTIONS action)
					)
				)
				(LETGOAL _A (FailBackwardWrongGoal stepname shape))
				(ComplainBackward pattern stepname shape)

TACTIC BackwardOnly2 (stepname, action, explain, _Ah) IS /* oh for a binding mechanism other than tactic application ... */
	WHEN	(LETCONC _Ac (BackwardOnly3 stepname action explain _Ah _Ac ", selecting"))
				(LETRHS _Ac  /* can't fail */(BackwardOnly3 stepname action explain _Ah _Ac " from"))

TACTIC BackwardOnly3 (stepname, action, explain, _Ah, _Ac, stuff) IS /* Oh for tactic nesting ... */
	(ALERT ("You asked for a backward step with the %s rule%s the conclusion %s, \
				 \but you also selected the antecedent %s. \
				 \\n\nThe %s rule %s. \
				 \ \n\nDo you want to go on with the backward step from %s -- ignoring %s?", 
				stepname, stuff,  _Ac, _Ah, stepname, explain, _Ac, _Ah)
				("Backward", action) STOP
	)

TACTIC FailBackwardWrongGoal (stepname, shape) IS
	WHEN	(LETCONC _Ac (FailBackwardWrongGoal2 stepname shape _Ac "You selected"))
				(LETRHS _Ac (FailBackwardWrongGoal2 stepname shape _Ac "The current conclusion is"))

TACTIC FailBackwardWrongGoal2 (stepname, shape, Pc, stuff) IS	
	ALERT	("To make a backward step with %s, the conclusion must be of the form %s. \
				\\n%s %s, which isn't of that form.", stepname, shape, stuff, Pc)
				("OK", STOP) ("Huh?", SEQ Explainantecedentandconclusionwords STOP )

/* ******************** tactic for rules that allow a hypothesis selection, but must have the right goal ******************** */

TACTIC BackwardWithHyp (gpattern, action, stepname, shape) IS
	WHEN	(LETGOAL gpattern (WITHSELECTIONS action))
				(LETGOAL _A (FailBackwardWrongGoal stepname shape))
				(SEQ 
					(ComplainBackwardDeadConc gpattern stepname shape)
					(ComplainBackwardBadGoal gpattern stepname shape)
					(Fail	("Error in I2L Jape (no error message in BackwardWithHyp [%t] %s [%t]). Tell Richard.",gpattern,stepname,shape))
				)

/* ******************** tactics to deal with bad backward selections ******************** */

MACRO ComplainBackward (pattern, stepname, shape) IS
	SEQ 
		(ComplainBackwardDeadConc pattern stepname shape)
		(ComplainBackwardHypSel pattern stepname shape)
		(ComplainBackwardBadGoal pattern stepname shape)
		(Fail	("Error in I2L Jape (no error message in ComplainBackward [%t] %s [%t]). Tell Richard.",pattern,stepname,shape))
		
MACRO ComplainBackwardDeadConc(pattern, stepname, shape) IS
	WHEN	(LETCONC pattern /* dead conclusion, right shape */
					(LETCONC _Ac
						(ALERT	("You selected the already-proved conclusion %t.\
									\\nTo make a backward step with %s, select an unproved conclusion.", _Ac, stepname)
									("OK", STOP) ("Huh?", Explainunprovedconclusionwords)
						)
					)
				)
				(LETCONC _Ac /* dead conclusion, wrong shape */
					(FailBackwardWrongGoal stepname shape)
				)
				
MACRO ComplainBackwardHypSel(pattern, stepname, shape) IS
				(LETHYP _Ah
					(ALERT	("You selected the antecedent %t.\n\
								\To make a backward %s step, select an unproved conclusion of the \
								\form %s, and don't select an antecedent.", _Ah, stepname, shape)
								("OK", STOP) ("Huh?", Explainantecedentandconclusionwords)
					)
				)
				(LETHYPS _Ahs
					(ALERT	("You selected the antecedents %l.\n\
								\To make a backward %s step, select an unproved conclusion of the \
								\form %s, and don't select any antecedents.", (_Ahs, ", ", " and "), stepname, shape)
								("OK", STOP) ("Huh?", Explainantecedentandconclusionwords)
					)
				)

MACRO ComplainBackwardBadGoal(pattern, stepname, shape) IS
				(LETOPENSUBGOAL G _Ag
					(Fail	("Error in I2L Jape (open subgoal in ComplainBackward [%t] %s [%t]). Tell Richard.",pattern,stepname,shape))
				)
				(LETOPENSUBGOALS _Ags
					(ALERT	("There is more than one unproved conclusion. Please select one to show \
								\Jape where to apply the %s step.", stepname)
								("OK", STOP) 
								("Huh?", Fail	("The unproved conclusions (formulae below a line of dots) in your proof \
													\are %l.\nSelect one of them to tell Jape where to make the %s step.",
													(_Ags,", "," and "), stepname)
								)
					)
				)
				(ALERT "The proof is finished -- there are no unproved conclusions left."
							("OK", STOP) ("Huh?", Explainunprovedconclusionwords)
				)

/* ******************** special one for hyp ******************** */

TACTIC "try hyp" (Ph, Pg, mess) IS
	SEQ	(ALT (UNIFY Ph Pg)
					(BADUNIFY X Y (ALERT ("%s, because the formula structures don't match.", mess) ("OK", STOP)))
					(BADMATCH X Y (ALERT ("%s, because to do so would change the proof (you shouldn't see this message).", mess) ("OK", STOP)))
					(BADPROVISO X Y P 
						(ALERT ("%s, because to do so would smuggle a variable out of its scope \
									\(see the proviso %s in the proviso pane).", mess, P)
									("OK", STOP)
									("Huh?", (ALERT "The proviso i NOTIN _B (for example) appears when the unknown _B occurs both inside \
															\AND outside the scope box for i. It means that nothing unified with _B can contain \
															\the variable i, because that would produce an occurrence of i outside its scope box."
															("OK", STOP)
												)
									)
						) 
					)
			)
			(ALT (WITHHYPSEL hyp)	
					(ALERT "I2L Jape error (hyp failed in hyptac). Tell Richard" ("OK", STOP))
			)

TACTIC hyptac IS
	WHEN	(LETHYP _Ah /* demand antecedent selection to avoid multi-hyp messages ... */
					(WHEN	(LETCONC _Ag ("try hyp" _Ah _Ag ("%t and %t don't unify", _Ah, _Ag)))
								(LETOPENSUBGOAL G _Ag 
									/* shall we just do it if we can? */
									(GOALPATH G) 
									("try hyp" _Ah _Ag 
													("%t and %t (the only relevant unproved conclusion) don't unify", _Ah, _Ag)
									)
								)
								(LETOPENSUBGOALS _Ags
									(ALERT ("Please select one of the unproved conclusions %l to unify with the antecedent %t.", 
												(_Ags, ", ", " or "), _Ah) ("OK", STOP) ("Huh?", SEQ Explainantecedentandconclusionwords STOP )
									)
								)
								(ALERT ("There aren't any unproved conclusions relevant to the antecedent %t.", _Ah)
											("OK", STOP) ("Huh?", SEQ Explainantecedentandconclusionwords STOP )
								)
					)
				)
				(LETGOAL _Ac 
					(WHEN (LETLHS ()
									(ALERT ("You can't use hyp to prove %t, because there aren't any relevant antecedents.", _Ac)
												("OK", STOP) ("Huh?", SEQ Explainantecedentandconclusionwords STOP )
									)
								)
								(ALERT	("Please select an antecedent to unify with the conclusion %s.", _Ac)
											("OK", STOP) ("Huh?", SEQ Explainantecedentandconclusionwords STOP )
								)
					)
				)
				(ALERT "To use hyp, you have to select a conclusion and an antecedent. You didn't select anything"
							("OK", STOP) ("Huh?", SEQ Explainantecedentandconclusionwords STOP )
				)
				
/* ******************** the Backward menu ******************** */

MENU Backward IS
	ENTRY	"ç intro (makes assumption)"		IS BackwardOnlyA (QUOTE (_Aç_B)) (Noarg "ç intro" "ç intro") "ç intro" "AçB"
	ENTRY	"¦ intro"									IS BackwardOnlyC (QUOTE (_A¦_B)) (Noarg "¦ intro backward" "¦ intro") "¦ intro"  "A¦B"
	ENTRY	"ë intro (preserving left)"			IS BackwardOnlyC (QUOTE (_Aë_B)) (Noarg (LAYOUT "ë intro" (0) "ë intro(L)" fstep) "ë intro") "ë intro" "AëB"
	ENTRY	"ë intro (preserving right)"			IS BackwardOnlyC (QUOTE (_Aë_B)) (Noarg (LAYOUT "ë intro" (0) "ë intro(R)" fstep) "ë intro") "ë intro" "AëB"
	ENTRY	"Â intro (makes assumption A)"	IS BackwardOnlyA (QUOTE (Â_A)) (WITHARGSEL "Â intro") "Â intro" "ÂA" 
	ENTRY	"è intro (introduces variable)"		IS BackwardOnlyA (QUOTE (è_x._A)) (Noarg "è intro" "è intro") "è intro" "èx.A" 
	ENTRY	"ä intro (needs variable)"			"ä intro backward"
		
	SEPARATOR
	
	ENTRY	"Â elim (invents formulae)"	IS 
		WHEN 
			(LETHYP2 _A (Â_A) 
				(ALERT	("To make a backward step with Â elim you don't have to select any antecedents. \
							\You selected %t and %t, which would fit a forward step. Since it works better \
							\that way, would you like to use Â elim going forward?", _A, Â_A)
							("Forward", "Â elim forward") ("Cancel", STOP)
				)
			)
			(LETHYP (Â_A) 
				(BackwardWithHyp Ù (Noarg (SEQ ("Â elim") fstep (WITHHYPSEL (hyp (Â_A)))) "Â elim") "Â elim" "Ù")
			)
			(BackwardOnlyC Ù (SEQ (SingleorNoarg "Â elim" "Â elim") fstep fstep) "Â elim" "Ù")
	ENTRY	"Ù elim (constructive)"	IS BackwardOnlyA (QUOTE _A) (Noarg "Ù elim (constructive)" "Ù elim (constructive)") "Ù elim (constructive)" "A"
	ENTRY	"Ù elim (classical; makes assumption ÂA)"	IS BackwardOnlyA (QUOTE _A) (Noarg "Ù elim (classical)" "Ù elim (classical)") "Ù elim (classical)" "A"

	SEPARATOR
	ENTRY	hyp 	IS Noarg hyptac hyp
END
	
MACRO trueforward(tac) IS
	LETGOAL _A (CUTIN (LETGOAL _B (UNIFY _A _B) tac)) (ANY (MATCH hyp))
		
TACTIC fstep IS
	ALT (ANY (MATCH hyp)) (trueforward SKIP) /* avoid nasty 'hyp matches two ways', I hope */
	
TACTIC "¦ intro backward" IS
	WHEN	(LETGOAL (_A¦_B) /* bound to work, surely? */
					"¦ intro" fstep fstep
				)
				(ALERT "¦ intro backward tactic failed. Tell Richard.")
				
/* ******************** tactics to check that a rule can be applied forward ******************** */

TACTIC Forward (fpat, action, frule, brule, shape) IS 
	Forward2 fpat  action  fpat  frule  brule  shape 

MACRO Forward2 (fpat, action, bpat, frule, brule, shape) IS
	WHEN	(LETHYP fpat action)
				(LETLHS fpat action) /* why not? */
				(LETHYP _Ah (FailForwardWrongHyp frule shape _Ah))
				(FailForward bpat frule brule (" of the form %s", shape))

MACRO FailForward (bpat, frule, brule, explainhyp) IS
	WHEN	(LETGOAL bpat 
					(WHEN	(LETCONC _Ac (FailForwardButBackwardPoss frule brule explainhyp ("You did select the conclusion formula %s", _Ac))) 
								(FailForwardButBackwardPoss frule brule  explainhyp ("The current conclusion formula is %s", bpat))
					)
				)
				(FailForwardNoHyp frule explainhyp "")

TACTIC FailForwardButBackwardPoss (frule, brule, explainhyp, explainconc) IS
	FailForwardNoHyp frule explainhyp 
			("\n\n(%s, which would fit %s backwards -- did you perhaps mean to work backwards?)", explainconc, brule)

TACTIC FailForwardWrongHyp (stepname, shape, Ph) IS
	ALERT	("To make a forward step with %s you must select something of the form %s. \
				\\nYou selected %s, which isn't of that form.", stepname, shape, Ph)
				("OK", STOP) 

TACTIC FailForwardNoHyp (stepname, explainhyp, extra) IS
	ALERT	("To make a forward step with %s you must select a formula to work forward from.\
				\\nYou didn't.%s", stepname, extra)
				("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )

/* ******************** special one for rules which don't care about antecedent shape  ******************** */

TACTIC "ë introforward" (rule) IS
	WHEN	(LETHYP _A (ForwardCut 0 (LAYOUT "ë intro" (0) (WITHARGSEL rule))))
				(FailForwardNoHyp "ë intro" "" "")

TACTIC "¦ intro forward"  IS
	WHEN	(LETHYP2 _A _B
					(ALERT ("¦ intro going forward is visually ambiguous Ð that is, it can be carried out either way round. \
					             \\nDo you want to\n\
					             \\n(a)    Make %t¦%t, or \
					             \\n(b)    Make %t¦%t.", _A, _B, _B, _A)
					             ("(a)", (CUTIN	"¦ intro" (ANY (WITHHYPSEL (hyp _A))) (ANY (WITHHYPSEL (hyp _B)))))
					             ("(b)", (CUTIN	"¦ intro" (ANY (WITHHYPSEL (hyp _B))) (ANY (WITHHYPSEL (hyp _A)))))
					             ("Cancel", STOP)
					)
				)
				(LETHYP _A 
					(ALERT	("To make a forward step with ¦ intro, it's necessary to select TWO antecedent formul¾ to combine. \
								\You only selected %t.", _A)
								("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )
					)
				) 
				(LETHYPS _As 
					(ALERT	("To make a forward step with ¦ intro, it's necessary to select only two antecedent formul¾ to combine. \
								\You selected %l.", (_As, ", ", " and "))
								("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )
					)
				) 
				(LETGOAL (_A¦_B)
					(ALERT	("To make a forward step with ¦ intro, it's necessary to select TWO antecedent formul¾ to combine. \
								\You didnÕt select any at all.\
								\\n\nHowever, the current conclusion %t would fit ¦ intro going backwards. Did you perhaps mean to \
								\make a backward step?", _A¦_B)
								("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )
					)
				)
				(ALERT	("To make a forward step with ¦ intro, it's necessary to select TWO antecedent formul¾ to combine. \
							\You didnÕt select any at all.")
							("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )
				)

/* TACTIC "¦ intro forward(L)"(arg) IS 
	ForwardCut 0 ("¦ intro"[B\arg])

TACTIC "¦ intro forward(R)"(arg) IS 
	ForwardCut 0 ("¦ intro"[A\arg])
*/

/* this tactic is so horrible gesturally that even though it works I've taken it out of the Forward menu, and commented it out.

TACTIC "ä intro forward" IS
	WHEN	(LETHYP (actual _i)
					(WHEN
						(LETHYPSUBSTSEL (_P[_x\_i1]) 
							("ä intro forward checkvar" _i _i1) 
							("ä intro forward complain" 
								(" (you only selected actual %t)", _i) 
								(" (as you did). Sorry to be so fussy, but please click on %t and try again", _P[_x\_i1])
							)
						)
						("ä intro forward checktextsel" (" (you only selected actual %t)", _i))
					)
				)
				(LETHYP _A
					("ä intro forward checktextsel" (" (you selected %t)", _A))
				)
				(LETHYP2 (actual _i) _A
					(WHEN
						(LETHYPSUBSTSEL (_P[_x\_i1])
							("ä intro forward checksamehyp" _A (_P[_x\_i1]) _i1)
							("ä intro forward checkvar" _i _i1) 
							("ä intro forward doit" _A _x _i1 )
						)
						("ä intro forward checktextsel" " (as you did)")
					)
				)
				(LETHYPS _As
					("ä intro forward checktextsel" (" (you selected %l)", _As))
				)
				("ä intro forward checktextsel" " (you didn't select any antecedents)")

TACTIC "ä intro forward checktextsel" (varstuff) IS
	WHEN	(LETHYPSUBSTSEL (_P[_x\_i1]) 
					("ä intro forward complain" varstuff " (as you did)")
				)
				(LETHYPSUBSTSEL (_P[_x\_B]) 
					("ä intro forward complain" varstuff (" (your text-selection %t isn't a variable)", _B))
				)
				(LETSUBSTSEL (_P[_x\_B]) 
					("ä intro forward complain" varstuff (" (your text-selection %t isn't in an antecedent)", _B))
				)
				(LETARGTEXT i
					("ä intro forward complain" varstuff (" (your text-selection %s isn't a subformula)", i))
				)
				(SEQ
					("ä intro forward complain" varstuff (" (you didn't text-select anything, or you text-selected several different things)"))
				)

MACRO "ä intro forward checksamehyp" (h, sel, i) IS
	WHEN	(LETMATCH h sel SKIP)
				("ä intro forward complain" " (as you did)" (" (your text selection %t isn't inside the antecedent %t)", i, h))

MACRO "ä intro forward checkvar"(i,j) IS
	WHEN	(LETMATCH i j SKIP)
				("ä intro forward complain" (" (you selected actual %t)",i) (" (you text-selected instance(s) of %t, which doesn't match %t)", j, i))

MACRO "ä intro forward doit" (P1,x1,i1) IS
	(CUTIN "ä intro" (WITHSUBSTSEL hyp) (WITHHYPSEL (hyp (actual i1))))

TACTIC "ä intro forward complain"(varstuff, selstuff) IS
	SEQ
		(ALERT	("To make an ä intro step forward you have to select an assumption like actual i and also \
					\an antecedent formula in its scope%s,\
					\and text-select instances of the variable in that formula%s.",
					varstuff, selstuff)
		)
		STOP
				
 */
 
/* ******************** messages about bad forward selections ******************** */

TACTIC BadForward (pattern, stepname, shape) IS 
	BadForward2 pattern stepname shape (" of the form %s", shape)

MACRO BadForward2(pattern, stepname, shape, stuff) IS
	WHEN	(LETHYP pattern /* right antecedent, other things wrong */ 
					BadForward3 pattern stepname shape stuff
				)
				(LETHYP _Ah /* wrong antecedent, other things wrong as well */
					(FailForwardWrongHyp stepname shape _Ah)
				)
				/* no antecedent at all -- just complain about that */
				(FailForwardNoHyp stepname stuff "")

TACTIC BadForward3(sel, stepname) IS
	WHEN	(LETOPENSUBGOALS _Ags
					(ALERT ("There is more than one unproved conclusion (formula below a line of dots) \
								\associated with the antecedent %t. \
								\\nSelect one of them (%l) before you make a forward step.", sel, (_Ags, ", ", " or "))
								("OK", SKIP) ("Huh?", ExplainHypMulti stepname sel)
					)
				)
				(LETOPENSUBGOAL G _Ag
					(ALERT ("Single subgoal in BadForward3. Error in I2LJape -- tell Richard."))
				)
				(ALERT	("There are no unproved conclusions associated with the antecedent %t.", sel)
							("OK", SKIP) ("Huh?",  ExplainDeadHyp stepname sel)
				)

/* ******************** the Forward menu ******************** */

TACTIC "ç elim forward" IS
	WHEN	(LETHYP2 _A (_Aç_B) 
					(Noarg (CUTIN "ç elim" (WITHHYPSEL (hyp (_Aç_B))) (WITHHYPSEL (hyp _A))) "ç elim")
				)
				(LETHYP2 _C (_Aç_B)
					("ç elim forward fail"
						(" You selected %t, which is of the form AçB, but your other selection %t doesn't match %t.", _Aç_B, _C, _A)
					)
				)
				(LETHYP2 _A _B
					("ç elim forward fail" (" Neither of your selections (%t and %t) is of the form AçB.", _A, _B))
				)
				(LETHYP (_Aç_B) 
					(WHEN (LETGOAL _C (Noarg (CUTIN "ç elim" (WITHHYPSEL hyp) fstep) "ç elim"))
								("ç elim forward fail" (" You only selected %t.", _Aç_B))
					)
				)
				(LETLHS (_Aç_B)
					(WHEN (LETGOAL _C (Noarg (ForwardCut 0 "ç elim") "ç elim"))
								("ç elim forward fail" (" You didn't select anything."))
					)
				)
				(LETHYP _A
					("ç elim forward fail" (" You selected %t, which isn't of the form AçB.", _A))
				)
				(LETHYPS _A
					("ç elim forward fail" (" You selected too many antecedents Ð %l.", (_A, ", ", " and ")))
				)
				("ç elim forward fail" (" You didn't select anything."))
				
TACTIC "ç elim forward fail" (extra) IS
		Fail	("To make a forward step with ç elim you must select something of the form AçB, \
				\and something which matches A (or a target conclusion).%s", extra)

MENU Forward IS
	ENTRY	"ç elim"									IS "ç elim forward"
	ENTRY	"¦ elim (preserving left)"			IS Forward (QUOTE (_A¦_B)) (ForwardCut 0 (Noarg (LAYOUT "¦ elim" (0) "¦ elim(L)") "¦ elim"))  "¦ elim" "¦ intro" "A¦B"
	ENTRY	"¦ elim (preserving right)"			IS Forward (QUOTE (_A¦_B)) (ForwardCut 0 (Noarg (LAYOUT "¦ elim" (0) "¦ elim(R)") "¦ elim")) "¦ elim" "¦ intro" "A¦B"
	ENTRY	"ë elim (makes assumptions)"	IS Forward (QUOTE (_Aë_B)) (Noarg ("targeted forward" (ForwardUncut 0 "ë elim") "ë elim") "ë elim") "ë elim" "ë intro" "AëB"
	ENTRY "Â elim" 										IS "Â elim forward"
	ENTRY	"è elim (needs variable)"				IS "è elim forward"
	ENTRY	"ä elim (assumption & variable)"	IS Forward (QUOTE (ä_x._A)) (Noarg ("targeted forward" (ForwardUncut 0 "ä elim") "ä elim") "ä elim") "ä elim" "ä intro" "äx.A"
	ENTRY	"Ù elim (constructive)"				IS Forward Ù  "Ù elim (constructive) forward" "Ù elim (constructive)" "Â elim" Ù

	SEPARATOR
	ENTRY	"¦ intro"			IS "¦ intro forward"

	ENTRY	"ë intro (invents right)"		IS "ë introforward" "ë intro(L)"
	ENTRY	"ë intro (invents left)"			IS "ë introforward" "ë intro(R)"
	

	/* removed -- see comment on the tactic below
	ENTRY	"ä intro (needs variable)"		IS "ä intro forward"
	*/

	SEPARATOR
	ENTRY	hyp IS Noarg hyptac hyp
END

TACTIC "Ù elim (constructive) forward" IS
	WHEN
		(LETGOAL _A
			"Ù elim (constructive)"
			(WHEN
				(LETHYP Ù (WITHHYPSEL (hyp Ù)))
				(hyp Ù)
			)
		)
		(ForwardCut 0 (Noarg "Ù elim (constructive)" "Ù elim (constructive)"))

TACTIC "Â elim forward" IS
	WHEN	(LETHYP2 _A (Â_A) 
					(Noarg (CUTIN "Â elim" (WITHHYPSEL (hyp _A)) (WITHHYPSEL (hyp (Â_A)))) "Â elim")
				)
				(LETHYP (Â_A)
					(Noarg (CUTIN "Â elim" fstep (WITHHYPSEL (hyp (Â_A)))) "Â elim")
				)
				(LETHYP2 _A _B
					(ALERT	("To make a forward step with Â elim, it's necessary to select two antecedent formul¾ -- one to match \
								\A, the other to match ÂA. \
								\You selected %t and %t.", _A, _B)
								("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )
					)
				)
				(LETHYP _A 
					(ALERT	("To make a forward step with Â elim, it's necessary to select two antecedent formul¾ -- one to match \
								\A, the other to match ÂA. \
								\You only selected %t.", _A)
								("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )
					)
				) 
				(LETHYPS _As 
					(ALERT	("To make a forward step with Â elim, it's necessary to select only two antecedent formul¾ to combine. \
								\You selected %l.", (_As, ", ", " and "))
								("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )
					)
				) 
				(LETGOAL Ù
					(ALERT	("To make a forward step with Â elim, it's necessary to select two antecedent formul¾ -- one to match \
								\A, the other to match ÂA. \
								\You didnÕt select any at all.\
								\\n\nHowever, the current conclusion fits Â elim going backwards. Did you perhaps mean to \
								\make a backward step?", _A¦_B)
								("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )
					)
				)
				(ALERT	("To make a forward step with Â elim, it's necessary to select two antecedent formul¾ -- one to match \
								\A, the other to match ÂA. \
							\You didnÕt select any at all.")
							("OK", STOP) ("Huh?", SEQ ExplainClicks STOP )
				)

	
TACTIC "targeted forward" (action, stepname) IS
	WHEN	(LETHYP _Ah ("targeted forward 2" action stepname _Ah))
				(LETLHS _Ah ("targeted forward 2" action stepname _Ah))
				(Fail ("Error in I2LJape (ë elim forward falls through). Tell Richard."))
				
TACTIC "targeted forward 2"(action, stepname, hyp) IS
	(WHEN	(LETCONC _A action) 
				(LETOPENSUBGOAL G _Ag ("targeted forward single" action stepname G hyp _Ag))
				(LETOPENSUBGOALS _As
					(ALERT	("The %s step needs a ÔtargetÕ conclusion to work towards. \
								\Please click on one of the unproved conclusions Ð %l Ð and try the \
								\%s step again.", stepname, (_As, ", ", " or "), stepname)
								("OK", STOP) ("Huh?", Explainunprovedconclusionwords)
					)
				)
				(BadForward3 hyp stepname) 
	)
MACRO "targeted forward single"(action, stepname, path, selhyp, ogoal) IS
	LETGOALPATH G1
		(WHEN	(LETMATCH path G1 action) /* just do it, it's the next line */
					(ALERT	("The %s step needs a ÔtargetÕ conclusion to work towards. \
								\There's only one unproved conclusion (%t) which is relevant to the antecedent %t \
								\which you selected.\
								\\nWould you like to proceed, using %t as the target?\
								\\n(You can avoid this message in the future if you always select the target conclusion \
								\before making the %s step.)", stepname, ogoal, selhyp, ogoal, stepname)
								("OK", SEQ (GOALPATH path) action) ("Huh?", SEQ Explainantecedentandconclusionwords STOP)
								("Cancel", STOP)
					)
		)

/* ******************** explanations ******************** */

TACTIC ExplainHypMulti (stepname, Ph) IS
	WHEN	(LETOPENSUBGOALS _Aandgs
					(ALERT	("The lines of dots in a proof-in-progress mark places where there is still work to be done. \
								\The formula just below a line of dots is an unproved conclusion (notice that those formulae \
								\don't have reasons next to them). Your job is to show that each unproved conclusion follows \
								\from the line(s) above it. \
								\\n\nYou selected the antecedent %t, and the unproved conclusions which can use that \
								\antecedent are %l. (Conclusions which can't make use of the antecedent are 'greyed out'.)\
								\\n\nIf you select one of those unproved conclusions AS WELL AS selecting %t, then Jape can make a \
								\forward step and put the new formulae that it generates just before the conclusion you selected.", 
								Ph, (_Aandgs, ", ", " and "), Ph)
								("OK", STOP) ("Huh?", SEQ Explainantecedentandconclusionwords STOP)
					)
				)
				
TACTIC ExplainDeadHyp (stepname, Ph) IS
	ALERT	("When you select an antecedent, Jape shows the unproved conclusions that can make use of it \
				\in black, and any other conclusions Ð proved or unproved Ð in grey. You can see that there are no \
				\non-grey unproved conclusions in the proof, after you selected %t. \
				\\n\n(If there are any unproved conclusions in the proof, they are greyed-out either because \
				\they are above %t, or because they are inside a box that %t is outside.)",
				Ph, Ph, Ph)
				("OK", STOP) ("Huh?", SEQ Explainunprovedconclusionwords STOP)

TACTIC ExplainClicks IS
	Fail	("Click on a formula to select it.\n\n\
	         \The red selection marking shows you how you can work with the selected formula.\n\
	         \You can make a forward step from a selection open at the bottom (a downward selection).\n\
	         \You can make a backward step from a selection open at the top (an upward selection).\n\n\
	         \If you want to control where I2L Jape writes the result of a forward step, make both a downward and \
	         \an upward selection Ð the result will be written just before the upward selection, above the line of three dots.")

TACTIC Explainantecedentandconclusionwords IS
	Fail	("When you select an antecedent, you get a downward-pointing selection (a box round the \
			\formula, open at the bottom). You work forward from an antecedent selection.\
			\\n\nUnproved conclusion formulae are written with three dots above them. \
			\When you select an unproved conclusion, you get \
			\an upward-pointing selection (a box round the formula, open at the top). You work \
			\backwards from a conclusion selection, or it can be a target for a forward step.\
			\\n\nSome formulae can be used as antecedent or as unproved conclusion. In those cases \
			\the selection box has a dotted horizontal line. Click in the bottom half of the formula to make an \
			\antecedent selection, in the top half for a conclusion selection. or antecedent.\
			\\n\nAny formula can be used as an antecedent if there are relevant unproved conclusions below it \
			\in the proof.")

TACTIC Explainunprovedconclusionwords IS
	Fail	"Lines of dots mark places where there is still work to be done. \
			\The formula just below a line of dots is an unproved conclusion. \
			\Your job is to show that each unproved conclusion follows \
			\from the line(s) above it.\
			\\nWhen there are no unproved conclusions left, the proof is finished."

TACTIC ExplainDeadConc (stepname, Pc, stuff) IS
	Fail	("In making a proof, you normally select unproved conclusions Ð formulae just below a line of dots, \
			\with no proof-step reason written next to them. You are allowed to select proved conclusions, however, \
			\because sometimes you want to backtrack or prune the proof.  \
			\\n\nThe conclusion %t which you selected is already proved Ð it has a proof-step reason written next to \
			\it Ð so you can't make a forward step towards it, or a backward step from it.%s", Pc, stuff)
				
TACTIC ExplainMulti (stepname, stuff) IS 
	Fail	("The lines of dots in a proof-in-progress mark places where there is still work to be done. \
			\The formula just below a line of dots is an unproved conclusion (notice that those formulae \
			\don't have reasons next to them). \
			\In this proof there's more than one line of dots, which means more than one \
			\unproved conclusion.\
			\Your job is to show that each unproved conclusion follows from the line(s) above it. \
			\\n\nJape puts the results of a forward step just before a line of dots; it puts the results of a \
			\backward step just after a line of dots. If there is more than one line of dots in the proof, \
			\you must tell it where to do its work.\
			\\n\nSo if you want to make a forward step in this proof, you must select (click on) \
			\an unproved conclusion AS WELL AS an antecedent. If you want to make a backward step,\
			\ you only need to select an unproved conclusion.%s", stuff
			)

TACTIC Explainvariables IS
ALERT	"Variables are introduced into a proof by è intro and/or ä elim. They appear at the head of a box \
			\as a special pseudo-assumption actual i (this is Jape's version of scope-boxing; it uses variable \
			\names i, i1, and so on as appropriate). \
			\\n\nYou have to select one of these pseudo-assumptions each time you make a è elim or ä intro step.\
			\\n\nThe box enclosing actual ... is the scope of the variable. If there are unknowns in your proof, \
			\Jape will protect the scope box \
			\by putting a proviso such as i NOTIN _B in the Provisos pane .  If you try to \
			\break the scope using hyp or Unify then Jape will stop you, quoting the proviso."
			
TACTIC Explainprovisos IS
	ALERT "The proviso i NOTIN _B (for example) appears when the unknown _B occurs both inside \
				\AND outside variable i's scope box. It means that nothing unified with _B can contain \
				\the variable i, because that would produce an occurrence of i outside its scope box."

TACTIC CrashHypDeadConc IS
	Fail "I2L Jape error (hyp and dead conc selected). Tell Richard"
	
TACTIC CrashNoHypNoConcs IS
	Fail "I2L Jape error (no sel no concs). Tell Richard."

/* ******************** we don't like some of the automatic alerts ******************** */

PATCHALERT "double-click is not defined" 
	"Double-clicking a formula doesn't mean anything in I2L Jape: you have to select (single-click) and \
	\then choose a step from the Backward or Forward menu."
	("OK")
	
PATCHALERT "You double-clicked on a used antecedent or a proved conclusion"
	"Double-clicking a formula doesn't mean anything in I2L Jape: you have to select (single-click) and \
	\then choose a step from the Backward or Forward menu."
	("OK")

PATCHALERT "To make an ä intro step forward"
	("OK") ("Huh?", HowToTextSelect)

PATCHALERT "The Â elim step can accept a single argument" 
	("OK") ("Huh?", HowToTextSelect)
	
/* ******************** and we do our own (careful) unification ******************** */

MENU Edit
	ENTRY Unify IS I2Lunify
END

TACTIC I2Lunify IS
	WHEN	(LETMULTIARG _As
					(ALT UNIFYARGS
				 			(BADUNIFY X Y (ALERT ("%t and %t don't unify, because the formula structures don't match.", X, Y) ("OK", STOP)))
				 			(BADMATCH X Y (ALERT ("%t and %t don't unify, because to do so would change the proof (you shouldn't see this message).", X, Y) ("OK", STOP)))
				 			(BADPROVISO X Y P 
				 				(ALERT ("%t and %t don't unify, because to do so would smuggle a variable out of its scope \
				 							\(see the proviso %s in the proviso pane).", X, Y, P)
				 							("OK", STOP)
				 							("Huh?", SEQ Explainprovisos STOP)
				 				) 
				 			)
				 	)
				 )
				 (SEQ (ALERT "To use Unify, you must text-select more than one formula.") STOP)

/* this hack to get at HowToTextSelect from within a tactic -- I must do this properly asap */
PATCHALERT "To use Unify, "
	("OK") ("Huh?", HowToTextSelect)
