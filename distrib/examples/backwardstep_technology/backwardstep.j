/*
    $Id$

    Copyright (C) 2003-4 Richard Bornat
     
        richard@bornat.me.uk

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

/* needs explain_words.j */

/* ******************** backward-only steps -- no hypothesis selection ******************** */

/* pattern : there must be a single goal which matches this.
   action  : what to do
   stepname: how to label the step
   shape   : text which is used in error messages to describe pattern.
   explain : text which is used to describe why you shouldn't select a hypothesis.
 */

MACRO BackwardOnly (pattern, action, stepname, shape, explain) IS
    WHEN
        (LETGOAL pattern (BackwardOnly2 stepname action explain))
        (LETGOAL _A (ComplainBackwardWrongGoal stepname shape))
        (ComplainBackward pattern stepname shape)

TACTIC BackwardOnly2 (stepname, action, explain) IS 
    WHEN   
       (LETHYP _Ah 
           (WHEN    
               (LETCONC _Ac (BackwardOnly3 stepname action explain _Ah _Ac ", selecting"))
               (LETRHS _Ac  /* can't fail */
                   (BackwardOnly3 stepname action explain _Ah _Ac " from"))))
       (WITHSELECTIONS action)
/* oh for a binding mechanism other than tactic application ... */
    

TACTIC BackwardOnly3 (stepname, action, explain, _Ah, _Ac, stuff) IS /* Oh for tactic nesting ... */
    ALERT   ("You asked for a backward step with the %s rule%s the conclusion %s, \
             \but you also selected the antecedent %s. \
             \\n\nThe %s rule %s. \
             \ \n\nDo you want to go on with the backward step from %s -- ignoring %s?", 
            stepname, stuff,  _Ac, _Ah, stepname, explain, _Ac, _Ah)
            ("Backward", action) ("Cancel", STOP)

MACRO BackwardOnlyDouble (pattern1, pattern2, action, stepname, shapes, explain) IS
    WHEN 
        (LETGOAL pattern1 (BackwardOnly2 stepname action explain))
        (BackwardOnly pattern2 action stepname shapes explain)