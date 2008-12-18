/*
    $Id$

    Copyright (C) 2003-8 Richard Bornat
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the jape examples distribution, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*/

/* needs explain_basics.j, explain_words.j */

/* ******************** tactics to deal with bad backward selections ******************** */

/* I don't know why they are macros, but till I find out, don't change a thing ... */

MACRO ComplainBackward (pattern, stepname, shape) IS
    ALT 
        (ComplainBackwardConc pattern stepname shape)
        (ComplainBackwardHyp pattern stepname shape)
        (ComplainBackwardNoGoal pattern stepname shape)
        (ExplainThenStop   ("Jape Error -- no error message in ComplainBackward [%t] %s [%t]).\n\n\
                 \Please send a description of the circumstances to bugs@jape.org.uk.",
                 pattern,stepname,shape))
        
MACRO ComplainBackwardConc(pattern, stepname, shape) IS
    WHEN    
        (LETCONC pattern /* dead conclusion, right shape */
            (LETCONC _Ac /* do I need this? */
                (ALERT  ("You selected the already-proved conclusion %t.\
                        \\nTo make a backward step with %s, select an unproved conclusion.", 
                        _Ac, stepname)
                        ("OK", STOP) ("Huh?", Explainunprovedconclusionwords))
                STOP))
        (LETCONC _Ac /* dead conclusion, wrong shape */
            (ComplainBackwardWrongGoal stepname shape))
                
MACRO ComplainBackwardHyp(pattern, stepname, shape) IS
    WHEN
        (LETHYP _Ah
            (ALERT  ("You selected the hypothesis %t.\n\
                        \To make a backward %s step, select an unproved conclusion of the \
                        \form %s, and don't select a hypothesis.", _Ah, stepname, shape)
                        ("OK", STOP) ("Huh?", Explainhypothesisandconclusionwords)))
        (LETHYPS _Ahs
            (ALERT  ("You selected the hypotheses %l.\n\
                    \To make a backward %s step, select an unproved conclusion of the \
                    \form %s, and don't select any hypotheses.", (_Ahs, ", ", " and "), stepname, shape)
                    ("OK", STOP) ("Huh?", Explainhypothesisandconclusionwords)))

MACRO ComplainBackwardNoGoal(pattern, stepname, shape) IS
    WHEN
        (LETOPENSUBGOAL G _Ag
            (ExplainThenStop   
                ("Jape error -- single open subgoal in ComplainBackwardNoGoal [%t] %s [%t].\n\n \
                \Please send a description of the circumstances to bugs@jape.org.uk.",
                pattern,stepname,shape)))
        (LETOPENSUBGOALS _Ags
            (ALERT  ("There is more than one unproved conclusion. Please select one to show \
                    \Jape where to apply the %s step.", stepname)
                    ("OK", STOP) 
                    ("Huh?", 
                     ExplainThenStop   
                        ("The unproved conclusions (formulae below a line of dots) in your proof \
                        \are %l.\nSelect one of them to tell Jape where to make the %s step.",
                        (_Ags,", "," and "), stepname))))
        (ALERT "The proof is finished -- there are no unproved conclusions left."
                    ("OK", STOP) ("Huh?", Explainunprovedconclusionwords))

TACTIC ComplainBackwardWrongGoal (stepname, shape) IS
    WHEN    (LETCONC _Ac (ComplainBackwardWrongGoal2 stepname shape _Ac "You selected"))
            (LETRHS _Ac (ComplainBackwardWrongGoal2 stepname shape _Ac "The current conclusion is"))

TACTIC ComplainBackwardWrongGoal2 (stepname, shape, Pc, stuff) IS   
    ALERT   ("To make a backward step with %s, the conclusion must be of the form %s. \
            \\n%s %s, which isn't of that form.", stepname, shape, stuff, Pc)
            ("OK", STOP) ("Huh?", SEQ Explainhypothesisandconclusionwords STOP )

/* ******************** tactics to deal with bad forward selections ******************** */

MACRO ComplainForward (fpat, bpat, frule, brule, shape) IS
    WHEN    
        (LETHYPS _Hs (ComplainForwardExtraHyps fpat frule shape _Hs _Hs))
        (ComplainForward2 fpat bpat frule brule shape)

MACRO ComplainForward2 (fpat, bpat, frule, brule, shape) IS
    WHEN
        (LETGOAL bpat 
            (WHEN   (LETCONC _Ac (ComplainForwardButBackwardPoss fpat frule brule shape 
                                    ("You did select the conclusion formula %s", _Ac))) 
                    (ComplainForwardButBackwardPoss fpat frule brule shape 
                        ("The current conclusion formula is %s", bpat))))
        (ComplainForwardNoHyp fpat frule shape "")

TACTIC ComplainForwardButBackwardPoss (fpat, frule, brule, shape, explainconc) IS
    ComplainForwardNoHyp fpat frule shape 
        ("\n\n(%s, which would fit %s backwards -- did you perhaps mean to work backwards?)", 
            explainconc, brule)

TACTIC ComplainForwardWrongHyp (stepname, shape, Ph) IS
    ALERT   ("To make a forward step with %s you must select something of the form %s. \
            \\nYou selected %s, which isn't of that form.", stepname, shape, Ph)
            ("OK", STOP) 

TACTIC ComplainForwardNoHyp (fpat, stepname, shape, extra) IS
        (ALERT   ("To make a forward step with %s you must select a formula to work forward from.\
                  \\nYou didn't.%s", stepname, extra)
                  ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP))

TACTIC ComplainForwardExtraHyps (fpat, stepname, shape, allhyps, testhyps) IS
   WHEN (LETLISTMATCH fpat _Hs testhyps
            (ALERT ("To make a forward step with %s you must select a hypothesis of the form %s. \
                    \\nYou selected more than that: %l.", frule, shape, (allhyps, ", ", " and "))
                   ("OK", STOP)))
        (LETLISTMATCH _H1 _Hs testhyps
            (ComplainForwardExtraHyps fpat stepname shape allhyps _Hs))
        (ALERT ("To make a forward step with %s you must select a hypothesis of the form %s. \
                    \\nYou selected more than one hypothesis -- %l -- but none of them matched %s.", 
                    frule, shape, (allhyps, ", ", " and "), shape)
                   ("OK", STOP) ("Huh?", Explainhypothesisandconclusionwords))
