/*
    Copyright (C) 2020 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the Aristotleian deductive logic encoding, distributed with jape.

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

/* Aristotle's deductive logic according to von Plato.
   Encoded by RB, 07-10/2020 and 05/2022
 */

INITIALISE autoAdditiveLeft true
INITIALISE applyconjectures none    /* see RADIOBUTTON below */

INITIALISE displaystyle box         /* see RADIOBUTTON below */
INITIALISE outermostbox false       
INITIALISE innerboxes false         /* see RADIOBUTTON below */

INITIALISE outerassumptionword "assumption"
INITIALISE innerassumptionword "assumption"

INITIALISE multiassumptionlines false

INITIALISE hidecut true 
INITIALISE hidehyp true             
INITIALISE priorAntes true
INITIALISE hidewhy true             /* see RADIOBUTTON below */

INITIALISE sayDerived false
INITIALISE sayResolve false
INITIALISE sayTheorem false

INITIALISE multihypsel true

AUTOMATCH see

MENU Appearance
  RADIOBUTTON displaystyle
       "sequent style"          IS tree
  AND  "box style"              IS box
  INITIALLY box
  END
  RADIOBUTTON innerboxes
       "with boxes"          IS true
  AND  "linear"              IS false
  INITIALLY false
  END
  RADIOBUTTON hidewhy
       "with step names"    IS false
  AND  "just numbers"       IS true
  INITIALLY true
  END
  RADIOBUTTON priorAntes
       "with explicit antecedents"      IS true
  AND  "just citations"                 IS false
  INITIALLY true
  END
END

MENU EDIT
  RADIOBUTTON applyconjectures
       "apply only proved theorems and rules"   IS none
  AND  "apply unproved theorems"                IS theorems
  AND  "apply unproved rules"                   IS rules
  AND  "apply unproved theorems and rules"      IS all
  INITIALLY none
  END
  
END
