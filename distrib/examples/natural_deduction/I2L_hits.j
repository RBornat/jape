/*
    Copyright (C) 2000-8 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the I2L logic encoding, distributed with jape.

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

HYPHIT  P     ⊢ P   IS hyp   
HYPHIT  P∧Q ⊢ R IS ALT  (SEQ "∧ elim(L)" (WITHHYPSEL hyp))
                        (SEQ "∧ elim(R)" (WITHHYPSEL hyp))
                        (SEQ (ForwardCut 0 "∧ elim(L)") (ForwardCut 0 "∧ elim(R)"))
HYPHIT  P→Q  ⊢ R    IS ForwardCut 0 "→ elim"
HYPHIT  P∨Q  ⊢ R    IS ForwardUncut 0 "∨ elim"
HYPHIT  ¬¬P   ⊢ Q   IS ForwardCut 0 "¬ elim"
HYPHIT  ∀x.P ⊢ Q    IS ForwardCut 0 "∀ elim with side condition hidden" 
HYPHIT  ∃x.P ⊢ Q    IS ForwardUncut 0 "∃ elim"

CONCHIT Q∧R IS "∧ intro"
CONCHIT Q∨R IS ALT (SEQ "∨ intro(L)" hyp) (SEQ "∨ intro(R)" hyp)
CONCHIT Q→R IS "→ intro"      
CONCHIT ¬Q  IS "¬ intro"       
CONCHIT ∀x.Q    IS "∀ intro"  
CONCHIT ∃x.Q    IS "∃ intro with side condition hidden"
