/*
    $Id$

    Copyright (C) 2004-8 Richard Bornat
     
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

/* needs explain_basics.j */

TACTIC Explainhypothesisandconclusionwords IS
    ExplainThenStop    
        "When you select a hypothesis, you get a downward-pointing selection (a box round the \
        \formula, open at the bottom). You work forward from a hypothesis selection.\n\n\
        \Unproved conclusion formulae are written with three dots above them. \
        \When you select an unproved conclusion, you get \
        \an upward-pointing selection (a box round the formula, open at the top). You work \
        \backwards from a conclusion selection, or it can be a target for a forward step.\n\n\
        \Some formulae can be used as hypothesis or as unproved conclusion. In those cases \
        \the selection box has a dotted horizontal line. Click in the bottom half of the formula to make an \
        \hypothesis selection, in the top half for a conclusion selection.\n\n\
        \Any formula can be used as a hypothesis if there are relevant unproved conclusions below it \
        \in the proof."

TACTIC Explainunprovedconclusionwords IS
    ExplainThenStop    
        "Lines of dots mark places where there is still work to be done. \
        \The formula just below a line of dots is an unproved conclusion. \
        \Your job is to show that each unproved conclusion follows \
        \from the line(s) above it.\n\n\
        \When there are no unproved conclusions left, the proof is finished."

TACTIC ExplainDeadHyp (stepname, Ph) IS
    ALERT   ("When you select a hypothesis, Jape shows the unproved conclusions that can make use of it \
            \in black, and any other conclusions – proved or unproved – in grey. You can see that there are no \
            \non-grey unproved conclusions in the proof, after you selected %t.\n\n\
            \(If there are any unproved conclusions in the proof, they are greyed-out either because \
            \they are above %t, or because they are inside a box that %t is outside.)",
            Ph, Ph, Ph)
            ("OK", STOP) 
            ("Huh?", SEQ Explainunprovedconclusionwords STOP)

TACTIC ExplainClicks IS
    ExplainThenStop    
        ("Click on a formula to select it.\n\n\
         \The red selection marking shows you how you can work with the selected formula.\n\
         \You can make a forward step from a selection open at the bottom (a downward selection).\n\
         \You can make a backward step from a selection open at the top (an upward selection).\n\n\
         \If you want to control where I2L Jape writes the result of a forward step, make both a downward and \
         \an upward selection – the result will be written just before the upward selection, above the line of three dots."
        )

TACTIC ExplainDeadConc (stepname, Pc, stuff) IS
    ExplainThenStop    
        ("In making a proof, you normally select unproved conclusions – formulae just below a line of dots, \
        \with no proof-step reason written next to them. You are allowed to select proved conclusions, however, \
        \because sometimes you want to backtrack or prune the proof.\n\n\
        \The conclusion %t which you selected is already proved – it has a proof-step reason written next to \
        \it – so you can't make a forward step towards it, or a backward step from it.%s", Pc, stuff)

/* for theories which set seektipselection true, and don't always require a conclusion selection */                
TACTIC ExplainHypMulti (stepname, Ph, _Aandgs) IS
    ALERT  ("The lines of dots in a proof-in-progress mark places where there is still work to be done. \
            \The formula just below a line of dots is an unproved conclusion (notice that those formulae \
            \don't have reasons next to them). Your job is to show that each unproved conclusion follows \
            \from the line(s) above it.\n\n\
            \You selected the hypothesis %t, and the unproved conclusions which can use that \
            \hypothesis are %l. (Conclusions which can't make use of the hypothesis are 'greyed out'.)\n\n\
            \If you select one of those unproved conclusions AS WELL AS selecting %t, then a \
            \forward step will put the new formulae that it generates just before the selected conclusion.", 
            Ph, (_Aandgs, ", ", " and "), Ph)
            ("OK", STOP) 
            ("Huh?", SEQ Explainhypothesisandconclusionwords STOP)
                
/* for theories -- most theories -- which require a conclusion selection */                
TACTIC ExplainMulti (stepname, stuff) IS 
    ExplainThenStop    
        ("The lines of dots in a proof-in-progress mark places where there is still work to be done. \
        \The formula just below a line of dots is an unproved conclusion (notice that those formulae \
        \don't have reasons next to them). In this proof there's more than one line of dots, which means \
        \more than one unproved conclusion. Your job is to show that each unproved conclusion follows \
        \from the line(s) above it.\n\n\
        \Jape puts the results of a forward step just before a line of dots; it puts the results of a \
        \backward step just after a line of dots. If there is more than one line of dots in the proof, \
        \you must tell it where to do its work.\n\n\
        \So if you want to make a forward step in this proof, you must select (click on) \
        \an unproved conclusion AS WELL AS a hypothesis. If you want to make a backward step,\
        \ you only need to select an unproved conclusion.%s", stuff)
