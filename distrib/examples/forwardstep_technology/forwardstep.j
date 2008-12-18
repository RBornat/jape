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

MACRO trueforward(tac) IS
    LETGOAL _A 
        (CUTIN (LETGOAL _B (UNIFY _A _B) tac)) 
        (ANY (MATCH hyp))
        
TACTIC fstep IS
    ALT (ANY (MATCH hyp)) (trueforward SKIP) /* avoid nasty 'hyp matches two ways', I hope */

/* ******************** tactics to check that a rule can be applied forward ******************** */

TACTIC Forward (fpat, action, frule, brule, shape) IS 
    Forward2    fpat action fpat frule brule shape 

MACRO Forward2 (fpat, action, bpat, frule, brule, shape) IS
    WHEN    (LETHYP fpat action)
            (LETLHS fpat action) /* why not? */
            (LETHYP _Ah (ComplainForwardWrongHyp frule shape _Ah))
            (ComplainForward fpat bpat frule brule shape)

